# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Paths  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'QA/processing/linked_data/')

output_path = paste0(sharepoint_path, 'QA/outputs/')

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries ----
{ source(paste0(wd, "config.R")) }

# Functions ----
{ source(paste0(wd, "functions.R"))}

# Load data ----

# 1 load data 
data <- readRDS(file = paste0(
  output_path, 'primary_analysis_analytical_dataset.Rds'))

# Imputation ----

## Workplan ---- 
#0 Assess MNAR
#1 Multiple imputation: binary Norfolk var, regular using LA
#2 Multilevel imputation 
#3 Sensitivity checks: do the results change drastically including/excluding auxiliary variables 

## MNAR with Cramer's V & Chi_2 test ----

missing_data = dplyr::mutate(
  data,
  is_missing_ethnicity = ifelse(is.na(ethnicity_agg), 1, 0),
  is_missing_gender = ifelse(gender == 'Other', 1, 0))

covariates = c(
  'cla_status',
  'local_authority',
  'wedge',
  'treatment_group',
  'age_at_referral_cat',
  'gender',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'number_of_previous_child_protection_plans',
  'referral_no_further_action')

mnar_table = purrr::map_dfr(
  covariates, function(cov){
    
    check_mnar(data = missing_data,
               missing_covariate = 'is_missing_ethnicity',
               auxiliary = cov) })

View(mnar_table)

# Subgroup analysis: check assocation within LAs 
clusters = unique(missing_data$local_authority)

mnar_table_la = purrr::map_dfr(
  clusters, function(c){
    
    la_data = filter(
      missing_data, 
      local_authority == c)
    
    table = purrr::map_dfr(
      covariates[-2], function(cov){
        
        check_mnar(data = la_data,
                   missing_covariate = 'is_missing_ethnicity',
                   auxiliary = cov) }) 
    table %>% 
      dplyr::mutate(cluster = c) %>%
      relocate(cluster) })

mnar_table_la %>%
  arrange(cluster, strength_of_association, desc(stat_significance)) %>%
  View()

View(mnar_table_la)

# LA missingness checks: crosstabs
missing_data %>%
  filter(local_authority == 'norfolk') %>%
  dplyr::group_by(disabled_status, is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  filter(local_authority == 'norfolk') %>%
  dplyr::group_by(unaccompanied_asylum_seeker, is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  filter(local_authority == 'norfolk') %>%
  dplyr::group_by(number_of_previous_child_protection_plans, 
                  is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  filter(local_authority == 'norfolk') %>%
  dplyr::group_by(wedge, 
                  is_missing_ethnicity) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2))

# 11% missing at baseline; then 6, 4, 6 and 8%
# In general, missingness in N is
# in those without previous CPP, not disabled, not UASC 

## Multiple imputation ---- 

covariates = c(
  'local_authority',
  'wedge',
  'treatment_group',
  'age_at_referral_cat',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'number_of_previous_child_protection_plans',
  'referral_no_further_action')

# Refine model data:
# select only model predictors 
model_data = data %>% select(
  child_id,
  referral_date,
  cla_status,
  any_of(covariates),
  cla_cin_cpp_rate_per_10_000_children) %>%
  mutate(
    is_norfolk = ifelse(
      local_authority == 'norfolk', 1, 0),
    splines_cla_cin_cpp_rates = data.frame(splines::ns(
      cla_cin_cpp_rate_per_10_000_children, df = 5)),
    splines_cla_cin_cpp_rates_1 = splines_cla_cin_cpp_rates$X1,
    splines_cla_cin_cpp_rates_2 = splines_cla_cin_cpp_rates$X2,
    splines_cla_cin_cpp_rates_3 = splines_cla_cin_cpp_rates$X3,
    splines_cla_cin_cpp_rates_4 = splines_cla_cin_cpp_rates$X4,
    splines_cla_cin_cpp_rates_5 = splines_cla_cin_cpp_rates$X5) %>%
  mutate(across(.cols = contains('splines'),
                .fns = .as.numeric)) %>%
  relocate(is_norfolk, .after = local_authority) %>%
  select(-cla_cin_cpp_rate_per_10_000_children,
         -splines_cla_cin_cpp_rates)

# View missing data pattern
mice::md.pattern(model_data)

# Get predictor matrix 
bin_predm <- make.predictorMatrix(model_data)

### Binary imputation 4 Norfolk ----
#1 Impute data: use binary var for Norfolk

# Set which predictors should be used to impute 
# missing ethnicity values
bin_predm["ethnicity_agg", ] <- c(
  0, 0, 1, 0, # child ID, ref date: out, outcome: in, LA: out 
  1,1,1,1,
  1,1,1,1,
  1,1,1,1,
  1,1,1) 

imputed_data <- mice::mice(
  model_data,
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = bin_predm,
  maxit = 1000)

# Check logged events 
imputed_data$loggedEvents

# Check the imputed values for 'ethnicity'
#imputed_data$imp$ethnicity

## Imputation checks ----

# Check convergence 
plot(imputed_data)

# Visualize observed vs imputed values for ethnicity
imp_plot = propplot(imputed_data, 
                    label_size = 10,
                    show_prop = TRUE,
                    prop_size = 2)

imp_plot_trt = propplot(
  imputed_data,
  ethnicity_agg ~ treatment_group,
  label_size = 7) 

imp_plot_la = propplot(
  imputed_data, 
  ethnicity_agg ~ local_authority,
  label_size = 5) 

# Extract completed imputed datasets and check consistency
#complete_data_1 <- complete(imputation_model, 1)  # First imputed dataset
#complete_data_2 <- complete(imputation_model, 2)  # Second imputed dataset

# Check summaries
#summary(complete_data_1$ethnicity_agg)
#summary(complete_data_2$ethnicity_agg)

# Sensitivity check: Increase the number of imputations to 10
sensitivity_imputed_data <- mice(
  model_data, 
  m = 10,           # Number of multiple imputations
  method = c('ethnicity_agg' = 'polyreg'),  # Predictive mean matching (appropriate for mixed data)
  predictorMatrix = bin_predm,
  maxit = 1000,      # Maximum iterations for convergence
  seed = 123)

# Check logged events 
sensitivity_imputed_data$loggedEvents

# Compare summaries of the two imputation models
summary(imputed_data)
summary(sensitivity_imputed_data)

# Performance checks
#1 Convergence Diagnostics
#2 Assessing Imputed Values - not doing
#3 Comparing Distributions of Imputed and Observed Data
#4 Check for Consistency Across Imputed Datasets - not doing 
#5 Model Fit on Imputed Data
#6 Pooling Results
#7 Sensitivity Analyses

# Fit model ----

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     'referral_no_further_action',
                     sep = " + ")

##1 Complete case ----

# Rationale for bootstrapped standard errors:
# https://academic.oup.com/ije/article/47/1/321/4091562

# https://trialsjournal.biomedcentral.com/articles/10.1186/s13063-016-1571-2 
# Conversely, a cluster-level analysis, or a mixed-effects model or GEE with 
# a small-sample correction led to much wider confidence intervals and larger P values, 
# which more appropriately reflected the uncertainty around 
# the size of the treatment effect estimate.

re = " + (1 | local_authority)"

cluster_indicator = c(
  #" + cla_cin_cpp_rate_per_10_000_children",
  #" + cla_rate_per_10_000_children",
  #" + cla_cin_cpp_rate_per_10_000_children",
  " + splines::ns(cla_cin_cpp_rate_per_10_000_children, df = 5)")

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re
  ) # RE intercept 4 clusters

# Fit model 
m1 = lme4::glmer(
  as.formula(formula), 
  data = data[data$gender != 'Other', ],
  family = binomial)

# Check summary 
summary(m1)

#2 Check optimisers:
aa <- allFit(m1)
ss <- summary(aa)
ss$msgs[!sapply(ss$msgs,is.null)]

# Standard errors

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  #cluster_indicator, # adjust for time-varying cluster level indicators
  fe # fixed effects for clusters 
  #re
) # RE intercept 4 clusters

# Tidy results
tidy_m1 = broom.mixed::tidy(
  m1, conf.int=TRUE, 
  exponentiate=TRUE,
  #effects=c("fixed", "ran_pars")
  effects=c("fixed")
  )

complete_case_tb = tidy_m1 %>%
  dplyr::mutate(
    across(where(is.numeric), round,4),
    model = 'complete_case',
    formula = formula) %>%
  dplyr::relocate(model, formula)

writexl::write_xlsx(
  complete_case_tb,
  paste0(output_path,
         "model_outputs/",
         "complete_case_glmer_model.xlsx"))

# Sensitivity analysis: 

cluster_indicator = c(
  #" + cla_cin_cpp_rate_per_10_000_children",
  #" + cla_rate_per_10_000_children",
  #" + cla_cin_cpp_rate_per_10_000_children",
  " + splines::ns(cla_cin_cpp_rate_per_10_000_children, df = 5)")

fe = '+ local_authority'

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  fe
) # RE intercept 4 clusters

# Fixed effects 
m1_fe = stats::glm(
  as.formula(formula), 
  data = data[data$gender != 'Other', ],
  family = binomial(link = "logit"))

summary(m1_fe)

# Tidy results
tidy_m1_fe = broom::tidy(
  m1_fe, conf.int=TRUE, 
  exponentiate=TRUE,
  effects="fixed")

complete_case_tb_fe = tidy_m1_fe %>%
  dplyr::mutate(
    across(where(is.numeric), round,2),
    model = 'complete_case',
    formula = formula) %>%
  dplyr::relocate(model, formula)

# Cluster-robust covariance matrix
clustered_se <- vcovCL(m1_fe, cluster = ~local_authority)

# Cluster-robust standard errors and p-values
lmtest::coeftest(m1_fe, vcov = clustered_se)

# Calculate confidence intervals
confint_robust <- coefci(m1_fe, vcov = clustered_se)

# Exponentiate the coefficients and confidence intervals to get odds ratios
exp_coef <- exp(coef(m1_fe))
exp_ci <- exp(confint_robust)

# Print exponentiated coefficients and CIs (Odds Ratios and their CIs)
results <- cbind(OR = exp_coef, CI_Lower = exp_ci[, 1], CI_Upper = exp_ci[, 2])
print(results)



##2 Imputed data ----

# To check in case of singular fit: 
# https://www.biorxiv.org/content/10.1101/2021.05.03.442487v3

# Example of model fitting and pooling
#fit <- with(imputed_data, lm(age ~ gender + cluster))
#pooled_results <- pool(fit)
#summary(pooled_results)

splines = model_data %>% 
  select(contains('splines')) %>%
  colnames() 

cluster_indicator = str_flatten(
  paste0(splines, sep = ' + '))

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, " + ", # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re) # RE intercept 4 clusters

imputed_analyses_tb = purrr::map_dfr(
  c('imputed_data','sensitivity_imputed_data'), 
  
  function(data){
    
    print(paste0('Iteration for model: ', data))
    
    df = get(data)
    
    # Fit model 
    m2 = with(
      df, 
      lme4::glmer(
        as.formula(formula), 
        family = binomial))
    
    print(paste0('Model fitted'))
    
    #2 Check optimisers:
    #aa <- allFit(m2)
    #ss <- summary(aa)
    #print(ss$msgs[!sapply(ss$msgs,is.null)])
    
    print(paste0('Optimisers checked'))
    
    # Check summary 
    pooled_results <- mice::pool(m2)
    print(summary(pooled_results))
    
    print('Summary checked')
    
    tidy_m2 = broom.mixed::tidy(
      pooled_results, conf.int=TRUE, 
      exponentiate=TRUE,
      effects="fixed")
    
    tidy_m2 = tidy_m2 %>%
      dplyr::mutate(
        across(where(is.numeric), round,2),
        model = data,
        formula = formula) %>%
      dplyr::relocate(model, formula)
    
    return(tidy_m2)
    
  })

View(imputed_analyses_tb)

writexl::write_xlsx(
  complete_case_tb,
  paste0(output_path,
         "model_outputs/",
         "imputed_glmer_model.xlsx"))

# Stuff to think about ----
# To think about:

# 0 End of study period 

# 1 Correct model specification: 
# fixed/random effects 4 cluster and time 
# what participant level covariates to include
# how to model: age, 'acuity' and censoring 
# what cluster level covariates to include: 
# time-dependent, readiness, different length of wedges 
# control/treatment group balance: 
# weighted analyses so cluster 1 does not influence outcome too much? 

# 2 Robustness 
# Robust standard errors: 
# To check whether study is under powered   
# How to correct for small nb of clusters:
# x clubSandwich robust SE 
# x lmerTest Kenward-Roger correction
# Fixed vs random effects & consequences on SE (see https://www.youtube.com/watch?v=aG_zxnsVtLc)

# Issues to investigate:
# Small cluster correction - OK
# Different length for wedges - OK (weighting + time trends)
# Complexity of model; not converging; too granular; random effects very small: 
# 1 High Dimensionality 
# 2 Sparse data
# 3 Unique rows 

# Model 1: standard model 1 (time unvarying, random and fixed effects)
# Random effects for clusters, fixed effects for time; time-unvarying indicators only

# Fit model: simplest
# FE for time, RE intercepts for clusters 

# Assumptions for this model: 

# Fixed effects assumption
# 1 Common secular trend: 
# The effect of time is common across clusters and sequences 
# Modeled as categorical: not assuming any parametric shape (e.g. linear)
# 2 constant intervention effect: 
# The difference between control and treatment in outcomes is constant through time 

# RE intercept assumption:
# Random differences in outcome at baseline adjusted for

# Check the convergence information
# https://rdrr.io/cran/lme4/man/convergence.html
#model_0@optinfo$conv$opt
#isSingular(model_0)

# Standard errors:
# https://economics.mit.edu/sites/default/files/2022-09/cluster-6.pdf

# Get robust standard errors using the cluster sandwich estimator
#robust_se <- clubSandwich::coef_test(
#  model_1, vcov = "CR2", cluster = data$local_authority)

# vcov specifies the bias-reduced "CR2" variance estimator,
# which is recommended for small-sample clustered designs.

# Print the robust results (including adjusted standard errors)
#print(robust_se)

 # Sensitivity analyses ----

## S1: Switching imputation methods ----

### LA imputation ----
# Impute data: use LA to impute 

reg_predm <- make.predictorMatrix(model_data)

reg_predm["ethnicity_agg", ] <- c(
  0, 0, 0, # child ID, ref date, outcome: not auxiliary vars
  1, 0, 1, 1, # LA: in; is_norfolk binary var: out; wedge, trt group: in
  1, 1, 0, # age, gender: in; ethnicity: out
  1, 1, 1, # disability, UASC and CPP: in
  0, 0)  # Ref NFA and rate of CLA,CIN,CPP per 10,000 children: out 

reg_imp_data <- mice::mice(
  model_data,
  m = 5, 
  method = '2l.2stage.pmm',
  seed = 123, 
  predictorMatrix = reg_predm,
  maxit = 1000)

# Fit model 
m3 = with(
  reg_imp_data, 
  lme4::glmer(
    as.formula(
      paste0(
        "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
        demographics, # adjust for person level demographics
        cluster_indicator, # adjust for time-varying cluster level indicators
        re)), # RE intercept 4 clusters
    data = data[data$gender != 'Other',],
    family = binomial))

# Check summary 
pooled_results <- pool(m3)
summary(pooled_results)

tidy_m3 = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, 
  exponentiate=TRUE,
  effects="fixed")

tidy_m3 %>%
  dplyr::mutate(across(is.numeric, round,2)) %>%
  View()

#2 Check optimisers:
aa <- allFit(m3)
ss <- summary(aa)
ss$msgs[!sapply(ss$msgs,is.null)]

### Multilevel imputation ----

ml_predm <- make.predictorMatrix(model_data)

ml_predm[, 'local_authority'] <- -2  # Cluster is a level-2 variable

ml_predm["ethnicity_agg", ] <- c(
  0, 0, 0, # child ID, ref date, outcome: not auxiliary vars
  -2, 0, 1, 1, # LA: in; is_norfolk binary var: out; wedge, trt group: in
  1, 1, 0, # age, gender: in; ethnicity: out
  1, 1, 1, # disability, UASC and CPP: in
  0, 0)  # Ref NFA and rate of CLA,CIN,CPP per 10,000 children: out 

mlm_imp_data <- mice::mice(
  model_data,
  m = 5, 
  method = '2l.2stage.pmm',
  seed = 123, 
  predictorMatrix = predictorMatrix,
  maxit = 1000)

# Fit model 
m4 = with(
  mlm_imp_data, 
  lme4::glmer(
    as.formula(
      paste0(
        "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
        demographics, # adjust for person level demographics
        cluster_indicator, # adjust for time-varying cluster level indicators
        re)), # RE intercept 4 clusters
    data = data[data$gender != 'Other',],
    family = binomial))

# Check summary 
pooled_results <- pool(m4)
summary(pooled_results)

tidy_m4 = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, 
  exponentiate=TRUE,
  effects="fixed")

tidy_m4 %>%
  dplyr::mutate(across(is.numeric, round,2)) %>%
  View()

#2 Check optimisers:
aa <- allFit(m4)
ss <- summary(aa)
ss$msgs[!sapply(ss$msgs,is.null)]

## S2 Posthoc analyses ----

### Remove Rochdale ----
s_model_1 = lme4::glmer(
  as.formula(
    paste0(
      "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
      demographics,  " + cla_rate_per_10_000_children", # adjust for person and cluster level covs
      " + (1 | local_authority)")), # RE for slope and intercept 4 clusters
  data = data[data$local_authority != 'rochdale',],
  family = binomial)

summary(s_model_1) 
tidy_sm1 = broom.mixed::tidy(
  s_model_1, conf.int=TRUE,exponentiate=TRUE,effects="fixed")
View(tidy_sm1)

### Remove 17 year-olds ---- 
s_model_2 = lme4::glmer(
  as.formula(
    paste0(
      "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
      demographics,  " + cla_rate_per_10_000_children", # adjust for person and cluster level covs
      " + (1 | local_authority)")), # RE for slope and intercept 4 clusters
  data = data[data$age_at_referral_cat != '17',],
  family = binomial)

summary(s_model_2) 
tidy_sm2 = broom.mixed::tidy(
  s_model_2, conf.int=TRUE,exponentiate=TRUE,effects="fixed")
View(tidy_sm2)

### Remove NFA cohort ----
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action',
                     #'cla_rate_per_10_000_children',
                     sep = " + ")

s_model_3 = lme4::glmer(
  as.formula(
    paste0(
      "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
      demographics,  " + cla_rate_per_10_000_children", # adjust for person and cluster level covs
      " + (1 | local_authority)")), # RE for slope and intercept 4 clusters
  data = data[data$referral_no_further_action == 'Further action',],
  family = binomial)

summary(s_model_3) 
tidy_sm3 = broom.mixed::tidy(
  s_model_3, conf.int=TRUE,exponentiate=TRUE,effects="fixed")
View(tidy_sm3)

### Test for covid influence ----

# 1.0 without covid period rochdale (?)
lockdown_periods = c(
  seq(from = as.Date('2020-03-23'),
      to =  as.Date('2020-06-01'), 
      by = "day"),
  seq(from = as.Date("2020-09-14"), 
      to = as.Date("2020-10-30"),
      by = "day"),
  seq(from = as.Date('2020-11-01'),
      to =  as.Date('2020-12-02'), 
      by = "day"),
  seq(from = as.Date("2020-12-03"), 
      to = as.Date("2021-01-05"),
      by = "day"),
  seq(from = as.Date('2021-01-06'),
      to =  as.Date('2021-03-08'), 
      by = "day"))

covid_data = mutate(
  data,
  lockdown_restrictions = ifelse(
    referral_date %in% c(lockdown_periods), 1, 0))

s_model_2 = lme4::glmer(
  as.formula(
    paste0(
      "cla_status ~ treatment_group + ", # FE for trt + time effects
      demographics," + cla_rate_per_10_000_children", # adjust for person and cluster level covs
      " + (wedge | local_authority)")), # RE for slope and intercept 4 clusters
  data = covid_data,
  family = binomial)

summary(s_model_2) 
s_model_2@optinfo$conv$opt

# 2 investigate covid stuff
# e.g. add dummy for covid lockdowns > time-varying

# 2 deal with censorship
# e.g. poisson model 
# e.g. coxph 

