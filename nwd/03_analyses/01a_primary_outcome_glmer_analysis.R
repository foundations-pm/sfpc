# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths

# where the primary outcome dataset is
data_path = paste0(sharepoint_path, 'QA/outputs/') 

# where to save final output list
output_path = paste0(sharepoint_path, 'QA/outputs/model_outputs/primary_analyses/')

# where to save individual model/output files 
working_folder = paste0(output_path, 'working_folder/')

# where to save individual sensitivity checks / files
#sensitiviy_checks_folder = paste0(output_path, 'sensitivity_analyses/')

# Dates
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folders to save output findings 
# in a neat an organised manner 
# Save individual files in a new directory
# Named after the month when the analyses were conducted

if(!dir.exists(file.path(paste0(working_folder, dir_date)))){
  
  dir.create(file.path(working_folder, dir_date)) # create new dir
  paste0("Creating new directory: '", dir_date,"'")# confirm new dir is created
  
} else { 
  
  cat(
    crayon::green(
      crayon::bold(
        paste0("Directory '", dir_date, "' already exists."))))
  
  } # confirms dir already exists

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions
{ source(paste0(wd, "functions.R"))}

#00 Cohort descriptives -----------------------------------------------------------------

## Load data --------------------------------------------------------------------

# 1 load data 
data <- readRDS(file = paste0(
  sharepoint_path, 'QA/outputs/',
  'primary_analysis_analytical_dataset_V2.Rds'))

## Sample demographics ----
covariates = c(
  'local_authority',
  'wedge',
  'treatment_group',
  'cla_status',
  'age_at_referral_cat',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'number_of_previous_child_protection_plans',
  'prop_white_british',
  'new_referrals_rate_per_10_000_children',
  'cin_rate_per_10_000_children',
  'cpp_rate_per_10_000_children',
  'cla_rate_per_10_000_children',
  'turnover_rate_fte',
  'population_0_to_17')

# Basic checks
model_desc_table = data %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(treatment_group = as.character(treatment_group),
         cla_status = as.character(cla_status)) %>%
  describe(class = 'categorical') %>%
  dplyr::mutate(count = ifelse(count < 5, '[z]', count))

model_desc_la_table = data %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(treatment_group = as.character(treatment_group),
         cla_status = as.character(cla_status)) %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical',
           group = 'local_authority') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(count = ifelse(count < 5, '[z]', count))

# Baseline characteristics by LA and trial periods
model_desc_la_wedge_table = data %>%
  dplyr::select(any_of(covariates)) %>%
  dplyr::mutate(treatment_group = as.character(treatment_group),
                cla_status = as.character(cla_status)) %>%
  dplyr::group_by(local_authority, wedge) %>%
  describe(class = 'categorical',
           group = c('local_authority', 'wedge')) %>%
  dplyr::ungroup() %>%
  clean_table_for_publication()

# Save descriptives
setwd(output_path) # Model outputs

writexl::write_xlsx(
  model_desc_table, 
  paste0(
    output_path,
    "descriptives/nwd_primary_cohort_sample_descriptives.xlsx"))

writexl::write_xlsx(
  model_desc_la_table, 
  paste0(
    output_path,
    "descriptives/nwd_primary_cohort_sample_descriptives_by_la.xlsx"))

writexl::write_xlsx(
  model_desc_la_wedge_table, 
  paste0(
    output_path,
    "descriptives/nwd_primary_cohort_sample_descriptives_by_la_by_wedge.xlsx"))

## Outcome description ----

outcome_desc_for_tavistock = data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::summarise(
    children_eligible_referred = n(), # total nb of children referred during wedge
    children_referred_with_previous_cpp_plans = sum(
      number_of_previous_child_protection_plans != "0"), 
    children_looked_after = sum( # total number of children looked after during wedge
      !is.na(date_period_of_care_commenced)),
    children_referred_who_became_looked_after_within_18_months= sum(
      cla_status),
    test = sum(cla_status),
    children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial = sum(
      cla_status == 1 & number_of_previous_child_protection_plans != "0")) %>% # total number experiencing the outcome out of the children referred during wedge
  dplyr::mutate( # cumulative numbers
    cumulative_number_of_eligible_children = cumsum(
      children_eligible_referred),
    cumulative_number_of_eligible_children_with_previous_cpp_plans = cumsum(
      children_referred_with_previous_cpp_plans),
    cumulative_number_of_children_in_care = cumsum(
      children_looked_after),
    cumulative_number_of_children_experiencing_outcome = cumsum(
      children_referred_who_became_looked_after_within_18_months),
    cumulative_number_of_children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial = cumsum(
      children_referred_with_previous_cpp_and_who_became_looked_after_during_the_trial)) %>%
  ungroup() %>%
  dplyr::mutate(across(
    .cols = where(is.numeric), 
    .fns = ~ ifelse(.x < 10, '[z]', .x))) # suppression checks

#01 Imputation ---------------------------------------------------------------------

## Load data --------------------------------------------------------------------

# 1 load data 
data <- readRDS(file = paste0(
  sharepoint_path, 'QA/outputs/',
  'primary_analysis_analytical_dataset_V2.Rds'))

## Workplan
#0 Assess MNAR
#1 Multiple imputation: binary Norfolk var, regular using LA
#2 Multilevel imputation 
#3 Sensitivity checks: do the results change drastically including/excluding auxiliary variables 

## Missingness assessment---------------------------------------------

missing_data = dplyr::mutate(
  data,
  is_missing_ethnicity = ifelse(is.na(ethnicity_agg), 1, 0),
  is_missing_gender = ifelse(gender == 'Other', 1, 0))

# MNAR with Cramer's V and Chi squared test

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
  'referral_no_further_action' #,
  #'new_referrals_rate_per_10_000_children',
  #'cin_rate_per_10_000_children',
  #'cpp_rate_per_10_000_children',
  #'cla_rate_per_10_000_children',
  #'turnover_rate_fte',
  #'population_0_to_17'
  )

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
    
    la_data = dplyr::filter(
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

#View(mnar_table_la)

# Cont-Cat corr
#test = stats::glm(is_missing_ethnicity ~ local_authority + turnover_rate_fte,
#                  family = binomial,
#                  data = missing_data)

#test_tb = broom::tidy(test, exponentiate = TRUE)

# LA missingness checks: crosstabs
missing_data %>%
  dplyr::filter(local_authority == 'norfolk') %>%
  dplyr::group_by(disabled_status, is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  dplyr::filter(local_authority == 'norfolk') %>%
  dplyr::group_by(unaccompanied_asylum_seeker, is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  dplyr::filter(local_authority == 'norfolk') %>%
  dplyr::group_by(number_of_previous_child_protection_plans, 
                  is_missing_ethnicity) %>%
  dplyr::summarise(n())

missing_data %>%
  dplyr::filter(local_authority == 'norfolk') %>%
  dplyr::group_by(wedge, 
                  is_missing_ethnicity) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2))

# 11% missing at baseline; then 6, 4, 6 and 8%
# In general, missingness in N is
# in those without previous CPP, not disabled, not UASC 

## Multiple imputation -----------------------------------------

covariates = c(
  'local_authority',
  'wedge',
  'treatment_group',
  'cla_status',
  'age_at_referral_cat',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'number_of_previous_child_protection_plans',
  'referral_no_further_action',
  'prop_white_british',
  'population_0_to_17',
  'turnover_rate_fte',
  'new_referrals_rate_per_10_000_children',
  'cin_rate_per_10_000_children',
  'cpp_rate_per_10_000_children',
  'cla_rate_per_10_000_children')

# Refine model data:
# select only model predictors 
model_data = data %>% select(
  child_id, referral_date, cla_status,
  any_of(covariates)) %>%
  mutate(
    wedge = relevel(
      factor(wedge), ref = 'baseline'),
    is_norfolk = ifelse(
      local_authority == 'norfolk', 1, 0),
    splines_cla_rates = data.frame(splines::ns(
      cla_rate_per_10_000_children, df = 5)),
    splines_cin_rates = data.frame(splines::ns(
      cin_rate_per_10_000_children, df = 5)),
    splines_cpp_rates = data.frame(splines::ns(
      cpp_rate_per_10_000_children, df = 5))) %>%
  mutate(across(.cols = contains('splines'),
                .fns = .as.numeric)) %>%
  mutate(gender = relevel(
    factor(as.character(gender)), ref = 'Male')) %>%
  relocate(is_norfolk, .after = local_authority) 

model_data = tidyr::unpack(
  model_data, cols=c(
    splines_cla_rates,
    splines_cpp_rates,
    splines_cin_rates),
  names_sep = '_')

# View missing data pattern
mice::md.pattern(model_data)

# Get predictor matrix 
predm <- make.predictorMatrix(model_data)

### Imputation m=5 ----

# Further resources:
# https://stats.stackexchange.com/questions/577135/iterations-in-multiple-imputation
# https://stefvanbuuren.name/fimd/sec-howmany.html 
# https://bookdown.org/mike/data_analysis/imputation-missing-data.html 
# https://www.econstor.eu/bitstream/10419/113873/1/79212409X.pdf 

#1 Impute data: use binary var for Norfolk

# Set which predictors should be used to impute 
# missing ethnicity values
rates = model_data %>% dplyr::select(
  contains('rate')) %>% colnames()

predm[,"child_id"] <- 0
predm[,"referral_date" ] <- 0
predm[, "local_authority"] <- 0
predm[, "population_0_to_17"] <- 0
predm[, "prop_white_british"] <- 0
predm[, rates] <- 0

imputed_data_m5 <- mice::mice(
  model_data,
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = predm,
  maxit = 100)

# Check logged events 
imputed_data_m5$loggedEvents

# Save imputed data
setwd(paste0(output_path, "imputed_datasets/"))

miceadds::write.mice.imputation(
  imputed_data_m5, 
  name = "Norfolk_binary_single_level_m5_imputation", 
  include.varnames=TRUE,
  long=TRUE, 
  mids2spss=FALSE,
  dattype=NULL)

# Check the imputed values for 'ethnicity'
#imputed_data_m5$imp$ethnicity

# Check convergence 
plot(imputed_data_m5)

# Visualize observed vs imputed values for ethnicity
imp_plot = propplot(imputed_data_m5, 
                    label_size = 10,
                    show_prop = TRUE,
                    prop_size = 2)

imp_plot_trt = propplot(
  imputed_data_m5,
  ethnicity_agg ~ treatment_group,
  label_size = 7) 

imp_plot_la = propplot(
  imputed_data_m5, 
  ethnicity_agg ~ local_authority,
  label_size = 5) 

# Extract completed imputed datasets and check consistency
#complete_data_1 <- complete(imputation_model, 1)  # First imputed dataset
#complete_data_2 <- complete(imputation_model, 2)  # Second imputed dataset

# Check summaries
#summary(complete_data_1$ethnicity_agg)
#summary(complete_data_2$ethnicity_agg)

### Imputation m=10 ----

# Sensitivity check: Increase the number of imputations to 10
imputed_data_m10 <- mice(
  model_data, 
  m = 10,           # Number of multiple imputations
  method = c('ethnicity_agg' = 'polyreg'),  # Predictive mean matching (appropriate for mixed data)
  predictorMatrix = predm,
  maxit = 100,      # Maximum iterations for convergence
  seed = 123)

# Check logged events 
imputed_data_m10$loggedEvents

# Save imputed data
setwd(paste0(output_path, "imputed_datasets/"))

miceadds::write.mice.imputation(
  imputed_data_m10, 
  name = "Norfolk_binary_single_level_m10_imputation", 
  include.varnames=TRUE,
  long=TRUE, 
  mids2spss=FALSE,
  dattype=NULL)

# Compare summaries of the two imputation models
summary(imputed_data_m5)
summary(imputed_data_m10)

## Hot deck imputation ----------------------------------------------
#TBC

# Performance checks
#1 Convergence Diagnostics
#2 Assessing Imputed Values - not doing
#3 Comparing Distributions of Imputed and Observed Data
#4 Check for Consistency Across Imputed Datasets - not doing 
#5 Model Fit on Imputed Data
#6 Pooling Results
#7 Sensitivity Analyses

#02 Fit models ---------------------------------------------------------------------------------------------

# Rationale for bootstrapped standard errors:
# https://academic.oup.com/ije/article/47/1/321/4091562

# https://trialsjournal.biomedcentral.com/articles/10.1186/s13063-016-1571-2 
# Conversely, a cluster-level analysis, or a mixed-effects model or GEE with 
# a small-sample correction led to much wider confidence intervals and larger P values, 
# which more appropriately reflected the uncertainty around 
# the size of the treatment effect estimate.

##001 GLMER Complete case & MI method --------------------------------------------
# MI = missing indicator 

### Load data -------------------------------------------------------------------

data <- readRDS(file = paste0(
  sharepoint_path, 'QA/outputs/',
  'primary_analysis_analytical_dataset_V2.Rds'))

### Formula ---------------------------------------------------------------------

setwd(output_path)

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action', # not in EP
                     sep = " + ")

re = " + (1 | local_authority)"

cluster_indicator = c(
  " + prop_white_british",
    " + turnover_rate_fte",
    " + population_0_to_17" #,
    #" + splines::ns(cla_rate_per_10_000_children, df = 5)" #,
    #" + splines::ns(cpp_rate_per_10_000_children, df = 5)" #,
    #" + splines::ns(cin_rate_per_10_000_children, df = 5)"
  )

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

# Prep data:
# Prep data for missing indicator analysis
missing_indicator_data = dplyr::mutate(
  data,
  ethnicity_agg = ifelse(
    is.na(ethnicity_agg), 'Missing', ethnicity_agg))

### Fit model -------------------------------------

# Fit model on standard data: complete case analysis
# Fit model on data with missing indicator recoded: missing indicator analysis

m1_list = lapply(
  c('data', 'missing_indicator_data'), 
  function(dataset){
  
  df = get(dataset)
  
  # Re-scaling numeric variables 
  #df = dplyr::mutate(df, 
  #            turnover_rate_fte = turnover_rate_fte/100,
  #            dplyr::across(.cols = c('prop_white_british',
  #                             'turnover_rate_fte',
  #                             'population_0_to_17'),
  #                   .fns = ~ scale(.x, center = T, scale = T)))
  
  lme4::glmer(
    as.formula(formula), 
    data = df,
    family = binomial)
  
})

# Set standard names to keep track of which model is which
# Names will be used to provide summaries & save outputs 
# with a tag indicating which model the estimates are from
names(m1_list) = c('complete_case', 'missing_indicator')

names_m1 = names(m1_list)

# Check summary of models
summary_m1 = lapply(setNames(names_m1, names_m1),
       function(names_index) summary(m1_list[[names_index]]))

names(summary_m1)

### Diagnostics -----------------------------------------

# Workplan:
# (1) Troubleshooting model (checking optimiser performance and model warnings) 
# (2) Model diagnostics / fit assessment (VIF, R2, AIC, BIC) - TBC, check model assumptions 
# (e.g., heteroskedasticity etc.)

# Resources: 
# (1 Trouble shooting)
# GLMM FAQ, Aug 2024: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction  
# Intro to Multilevel Modelling: Chap. 7, Model Estimation Options, Problems, and Troubleshooting https://www.learn-mlms.com/07-module-7.html 
# Convergence warnings in lme4: https://rpubs.com/palday/lme4-singular-convergence 
# lme4 convergence warnings: troubleshooting https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html 
# Dealing with convergence failure in mixed models: https://www.youtube.com/watch?v=tSO5JmXR8hk&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=21 

# (2 Model diagnostics & inference)
# Performance package: https://easystats.github.io/performance/index.html 
# Building and Comparing Mixed Models in R: ICC, Bayes Factor, and Variance Explained: https://www.youtube.com/watch?v=Wtk5iZ65XHk&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=4
# Mixed Models: Diagnostics and Inference https://sscc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html

--------------------------------

# (1) Troubleshooting GLMM:
# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction 
# and https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html  
# and https://www.learn-mlms.com/07-module-7.html 

# Approach to troubleshooting:
# 1. double-check the model specification and the data for mistakes
# 2. center and scale continuous predictor variables (e.g. with scale())
# 3. try all available optimizers (e.g. several different implementations of BOBYQA and Nelder-Mead, L-BFGS-B from optim, nlminb(), …).
# "we consider it the gold standard; if all optimizers converge to values that are practically equivalent"
# "(it’s up to the user to decide what “practically equivalent means for their case”)" 
# "then we would consider the model fit to be good enough."

# OPTIMIZERS
# Check how optimisers are doing
# Optimisers: variations of BOBYQA, Nelder-Mead, L-BFGS-B
# More info: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction

#summary(warnings())

m1_ss_list = lapply(setNames(names_m1, names_m1), 
                  function(names_index){

  aa <- allFit(m1_list[[names_index]])
  ss <- summary(aa) 
  
  })

#warnings_df <- data.frame(
#  Optimizer = names(msgs),
#  Message = sapply(msgs, function(msg) {
#    if (is.null(msg)) {
#      "[OK]"  # Replace NULL with "[OK]" or use NA if preferred
#    } else {
#      msg  # Keep the warning message
#    }
#  }),
#  stringsAsFactors = FALSE
#)

# Convert into table 
m1_ss_table = purrr::map_dfr(
  setNames(names_m1, names_m1), 
  function(names_index){
    
    ss_df = data.frame(
      Optimizer = names(m1_ss_list[[names_index]]$msgs),
      Message = sapply(
        m1_ss_list[[names_index]]$msgs, 
        function(msg) {
      if (is.null(msg)) {
        "[OK]"  # Replace NULL with "[OK]" or use NA if preferred
      } else {
        msg  # Keep the warning message
      }
    }),
    stringsAsFactors = FALSE)
    
    ss_df <- ss_df %>%
      pivot_wider(names_from = Optimizer, 
                  values_from = Message)

    ss_df = ss_df %>%
      dplyr::mutate(analysis_type = names_index,
                    formula = formula,
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
    })

# REVIEW OF WARNINGS 

# 1. Convergence issues - not necessarily fatal; false positive rate is high with lme4

# 2. Singular fits - check https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#singular-fits for full explanations that may apply
# Singular fits commonly occur in two scenarios: ... applicable scenario here:
# (1) small numbers of random-effect levels (e.g. <5), 
# as illustrated in these simulations and discussed (in a somewhat different, Bayesian context) by Gelman (2006).

# Ben Bolker and others suggest great approaches to singularity problems -
# To discuss with Andi on 23/01

--------------------------------

# (2) Model diagnostics / fit assessment 
# https://easystats.github.io/performance/index.html 
# https://www.youtube.com/watch?v=Wtk5iZ65XHk&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=4
# https://sscc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html

# Overall checks 
performance::check_model(m1_list[['complete_case']])
performance::check_model(m1_list[['missing_indicator']])

# Check ICC
m1_icc = lapply(
  setNames(names_m1, names_m1),
  function(names_index) performance::icc(m1_list[[names_index]]))

print(m1_icc)

# Check VIF
# Quick look
performance::check_collinearity(m1_list[['complete_case']]) 
performance::check_collinearity(m1_list[['missing_indicator']]) 
car::vif(m1_list[['complete_case']])
car::vif(m1_list[['missing_indicator']])

# VIF table 
m1_vif_table = purrr::map_dfr(
  setNames(names_m1, names_m1),
  function(names_index) { 
    
    vif_table = performance::check_collinearity(m1_list[[names_index]]) 
    
    vif_table = vif_table %>%
      dplyr::mutate(analysis_type = names_index, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
    })

# Performance & fit indicators: AIC, BIC, R2...
m1_diagnostics_table = purrr::map_dfr( 
  setNames(names_m1, names_m1),
  function(names_index){
    
    performance_df = performance::model_performance(m1_list[[names_index]])
    
    performance_df = performance_df %>%
      dplyr::mutate(analysis_type = names_index, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

m1_diagnostics_table = dplyr::left_join(
  m1_ss_table,
  m1_diagnostics_table,
  by = c('analysis_type', 'formula', 'date'))

### Tidy up ---------------------------------------------

# Tidy results into dataframes 
#1 Raw model estimates
raw_m1_list <- lapply(
  setNames(names_m1, names_m1),
  function(names_index){
    
    df = data.frame(
      analysis_type = names_index,
      formula = formula,
      #icc = m1_icc[[names_index]],
      Coefficients = summary_m1[[names_index]]$coefficients[, "Estimate"],       # Log Odds
      `Standard Error` = summary_m1[[names_index]]$coefficients[, "Std. Error"],
      #`z value` = summary_m1$coefficients[, "z value"],           # Optional
      `p-value` = summary_m1[[names_index]]$coefficients[, "Pr(>|z|)"],
      date = date)
    
    df = df %>% 
      tibble::rownames_to_column('term') %>%
      dplyr::relocate(term, .after = formula)
    
  })

names(raw_m1_list)
  
#2 Tidy model estimates 
tidy_m1_list <- lapply(
  setNames(names_m1, names_m1),
  function(names_index) {
    
    tidy_m1 = broom.mixed::tidy(
      m1_list[[names_index]], conf.int=TRUE, 
      exponentiate=TRUE,
      #effects=c("fixed", "ran_pars")
      effects=c("fixed"))
    
    tidy_m1 = tidy_m1 %>%
      dplyr::mutate(
        date = date,
        across(where(is.numeric), round,4),
        analysis_type = names_index,
        formula = formula,
        #icc = m1_icc[[names_index]]
        ) %>%
      dplyr::relocate(date, analysis_type, formula) 
    
  })

names(tidy_m1_list)

### Save outputs ----------------------------------------

###### List ----
# Append latest results to existing findings on Sharepoint
# And save these results back into Sharepoint

# Bind all results from analyses into one df
m1_raw_table = do.call(bind_rows, raw_m1_list)
m1_tidy_table = do.call(bind_rows, tidy_m1_list)

# Working directory to save diagnostics table 
setwd(output_path) # Model outputs

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m1_tidy_table,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m1_raw_table,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = m1_diagnostics_table,
               table_2_to_append = m1_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, 'working_folder/', dir_date)) # Month folder 

#### Tidy and raw
cohort = 'main_cohort'

lapply(
  setNames(names_m1, names_m1),
  function(names_index) {
    
    writexl::write_xlsx(
      raw_m1_list[[names_index]], 
      paste0(
        cohort,
        "_raw_", names_index, "_",
        "_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
    
    writexl::write_xlsx(
      tidy_m1_list[[names_index]], 
      paste0(
        cohort,
        "_tidy_", names_index, "_",
        "_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
  })

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'complete_case_performance' = m1_diagnostics_table,
  'complete_case_vif' = m1_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    cohort,
    '_complete_case_glmer_diagnostics', 
    file_date , '.xlsx'),
  overwrite = TRUE)

##002 GLMER Imputed data ------------------------------------------

# To check in case of singular fit: 
# https://www.biorxiv.org/content/10.1101/2021.05.03.442487v3

# Example of model fitting and pooling
#fit <- with(imputed_data_m5, lm(age ~ gender + cluster))
#pooled_results <- pool(fit)
#summary(pooled_results)

### Load data -------------------------------------------------------------------

# Read data
setwd(paste0(
  sharepoint_path,
  'QA/outputs/datasets/imputed_datasets/'))

file.info(
  list.files(
    "Norfolk_binary_single_level_m5_imputation/",
    full.names=T))[,1, drop=F]

load(paste0("Norfolk_binary_single_level_m5_imputation/",
            "Norfolk_binary_single_level_m5_imputation.Rdata"))

imputed_data_m5 = mi.res 

imputed_data_m10 = load(
  paste0("Norfolk_binary_single_level_m10_imputation/",
         "Norfolk_binary_single_level_m10_imputation.Rdata"))

imputed_data_m10 = mi.res 

rm(mi.res)

# Number of iteration used to impute datasets
iteration_number = 100

# checks 
#d5_m5 = complete(imputed_data_m5, 5)
#d5_m5_test = complete(mi.res, 5)
#d5_m5 %>% dplyr::group_by(ethnicity_agg) %>% dplyr::summarise(n())
#d5_m5_test %>% dplyr::group_by(ethnicity_agg) %>% dplyr::summarise(n())

### Formula -------------------------------------------------------------------
# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     #'referral_no_further_action', # not in EP
                     sep = " + ")

re = " + (1 | local_authority)"

# Cluster indicator
# splines = model_data %>% 
#  select(contains('splines')) %>%
#  colnames() 

cluster_indicator = c(
  " + prop_white_british",
  " + turnover_rate_fte",
  " + population_0_to_17" #,
  #paste0(splines, sep = ' + ')
)

formula = paste0(
  "cla_status ~ treatment_group + wedge + ", # FE for trt + time effects
  demographics, # adjust for person level demographics
  str_flatten(cluster_indicator), # adjust for time-varying cluster level indicators
  re
) # RE intercept 4 clusters

### Fit models -----------------------------------------

# Fit model on imputed datasets with m= 5 and m=10
# Fitting models:
m2_list = lapply( # Creates a list of model objects
  c("imputed_data_m5", "imputed_data_m10"),  # models are fitted to both datasets
  function(dataset){
    
    print(paste0(
      'Fitting model for: ', dataset))
    
    df = get(dataset)

    # Fit model 
    with( 
      df, 
      lme4::glmer(
        as.formula(formula), 
        family = binomial)) 
    
  })

# Set standard names to keep track of which model is which
# Names will be used to provide summaries & save outputs 
# with a tag indicating which model the estimates are from
names(m2_list) = c('imputation_m5', 'imputation_m10')

names_m2 = names(m2_list)

# Pooling results 
# As per Stef Van Buurren's workflow recs:
# https://stefvanbuuren.name/fimd/workflow.html

m2_pooled_results_list <- lapply(
  setNames(names_m2, names_m2), function(names_index){
    
    mice::pool(m2_list[[names_index]]) # pool results
    
    })

names(m2_pooled_results_list)

# Get summaries from pooled results
# This retrieves raw model estimates for m=5 and m=10
m2_summary_list = lapply(
  setNames(names_m2, names_m2), function(names_index){ 
    
    summary(m2_pooled_results_list[[names_index]]) })

names(m2_summary_list)

### Diagnostics -------------------------------------

# OPTIMIZERS
# Check how optimisers are doing
# Optimisers: variations of BOBYQA, Nelder-Mead, L-BFGS-B
# More info: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#introduction

#summary(warnings())

m2_ss_list = lapply(
  setNames(names_m2, names_m2), 
  function(names_index){
    
    # assess only the first imputed dataset for m=5 and m=10
    aa <- allFit(m2_list[[names_index]]$analyses[[1]])
    ss <- summary(aa) 
    
    })

# Convert into table 
m2_ss_table = purrr::map_dfr(
  setNames(names_m2, names_m2), 
  function(names_index){
    
    ss_df = data.frame(
      Optimizer = names(m2_ss_list[[names_index]]$msgs),
      Message = sapply(
        m2_ss_list[[names_index]]$msgs, 
        function(msg) {
          if (is.null(msg)) {
            "[OK]"  # Replace NULL with "[OK]" or use NA if preferred
          } else {
            msg  # Keep the warning message
          }
        }),
      stringsAsFactors = FALSE)
    
    ss_df <- ss_df %>%
      pivot_wider(names_from = Optimizer, 
                  values_from = Message)
    
    ss_df = ss_df %>%
      dplyr::mutate(analysis_type = names_index,
                    formula = formula,
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

# REVIEW OF WARNINGS 

# 1. Convergence issues - not necessarily fatal; false positive rate is high with lme4

# 2. Singular fits - check https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#singular-fits for full explanations that may apply
# Singular fits commonly occur in two scenarios: ... applicable scenario here:
# (1) small numbers of random-effect levels (e.g. <5), 
# as illustrated in these simulations and discussed (in a somewhat different, Bayesian context) by Gelman (2006).

# Ben Bolker and others suggest great approaches to singularity problems -
# To discuss with Andi on 23/01

--------------------------------
  
# (2) Model diagnostics / fit assessment 
# https://easystats.github.io/performance/index.html 
# https://www.youtube.com/watch?v=Wtk5iZ65XHk&list=PL8F480DgtpW9_IT7xN1XeRF_dglZmK0nM&index=4
# https://sscc.wisc.edu/sscc/pubs/MM/MM_DiagInfer.html
  
# Overall checks 
#performance::check_model(m2_list[['complete_case']])
#performance::check_model(m2_list[['missing_indicator']])

# Check ICC
#m2_icc = lapply(
#  setNames(names_m2, names_m2),
#  function(names_index) performance::icc(m2_list[[names_index]]))

#print(m2_icc)

# Check VIF
#car::vif(m2_list[['complete_case']])
#car::vif(m2_list[['missing_indicator']])

m2_vif_table = purrr::map_dfr(
  setNames(names_m2, names_m2),
  function(names_index) { 
    
    # assess only the first imputed dataset for m=5 and m=10
    vif_table = performance::check_collinearity(
      m2_list[[names_index]]$analyses[[1]]) 
    
    vif_table = vif_table %>%
      dplyr::mutate(analysis_type = names_index, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

# Performance & fit indicators: AIC, BIC, R2...
m2_diagnostics_table = purrr::map_dfr( 
  setNames(names_m2, names_m2),
  function(names_index){
    
    # assess only the first imputed dataset for m=5 and m=10
    performance_df = performance::model_performance(
      m2_list[[names_index]]$analyses[[1]])
    
    performance_df = performance_df %>%
      dplyr::mutate(analysis_type = names_index, 
                    formula = formula, 
                    date = date) %>%
      dplyr::relocate(analysis_type, formula)
    
  })

m2_diagnostics_table = dplyr::left_join(
  m2_ss_table,
  m2_diagnostics_table,
  by = c('analysis_type', 'formula', 'date'))

### Tidy up -------------------------------------------

# Tidy outputs into dataframes 
# Raw estimate table
raw_m2_list <- lapply(
  setNames(names_m2, names_m2), function(names_index){
    
    df = data.frame(
      analysis_type = names_index,
      number_of_iteration = iteration_number,
      formula = formula,
      term = m2_summary_list[[names_index]]$term,
      #icc = m2_icc[[names_index]],
      Coefficients = m2_summary_list[[names_index]]$estimate,       # Log Odds
      `Standard.Error` = m2_summary_list[[names_index]]$std.error,
      #`Statistic` = m2_summary_list[[names_index]]$statistic,           # Optional
      `df` =  m2_summary_list[[names_index]]$df,
      `p.value` = m2_summary_list[[names_index]]$p.value,
      date = date)
    
    #df = df %>% 
    #  tibble::rownames_to_column('term') %>%
    #  dplyr::relocate(term, .after = formula)
    
  })

names(raw_m2_list)

# Tidy estimate table 
tidy_m2_list = lapply(
  setNames(names_m2, names_m2), function(names_index){
    
    tidy_m2 = broom.mixed::tidy(
      m2_pooled_results_list[[names_index]], conf.int=TRUE, 
      exponentiate=TRUE,
      effects="fixed")
    
    tidy_m2 = tidy_m2 %>%
      dplyr::mutate(
        across(where(is.numeric), round, 4),
        analysis_type = names_index,
        formula = formula,
        #icc = m2_icc[[names_index]],
        effect = 'fixed',
        number_of_iteration = iteration_number,
        date = date) %>%
      dplyr::relocate(analysis_type, number_of_iteration, 
                      formula, effect) 
  })

names(tidy_m2_list)

### Save outputs ----------------------------------------

###### List ----
# Append latest results to existing findings on Sharepoint
# And save these results back into Sharepoint

# Bind all results from analyses into one df
m2_raw_table = do.call(bind_rows, raw_m2_list)
m2_tidy_table = do.call(bind_rows, tidy_m2_list)

# Working directory to save diagnostics table 
setwd(output_path) # Model outputs

##### Tidy: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'tidy_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m2_tidy_table,
  save_to = 'tidy_output_list.xlsx') 

##### Raw: Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'raw_output_list.xlsx')

append_results(
  output_file = output_file,
  table_1_to_append = m2_raw_table,
  save_to = 'raw_output_list.xlsx') 

##### Diagnostics 
# Append and/or save table
output_file = str_subset( # find if file exists in directory
  list.files(), 
  'diagnostics_list.xlsx')

append_results(output_file = output_file,
               table_1_to_append = m2_diagnostics_table,
               table_2_to_append = m2_vif_table,
               is_multisheet_workbook = TRUE,
               save_to = 'diagnostics_list.xlsx')

###### Individual files ----
# Save/export raw & tidy estimates into excel file & into folder with monthly date
setwd(paste0(output_path, 'working_folder/', dir_date))

#### Tidy and raw
cohort = 'main_cohort'

lapply(
  setNames(names_m2, names_m2),
  function(names_index) {
    
    writexl::write_xlsx(
      raw_m2_list[[names_index]], 
      paste0(
        cohort,
        "_raw_", names_index, "_",
        "_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
    
    writexl::write_xlsx(
      tidy_m2_list[[names_index]], 
      paste0(
        cohort,
        "_tidy_", names_index, "_",
        "_glmer_model", # Saves files with a tag indicating which of complete case or MI analyses the estimates belong to
        file_date, ".xlsx"))
  })

#### Diagnostics
wb = openxlsx::createWorkbook()

diagnostics_list = list(
  'imputed_models_performance' = m2_diagnostics_table,
  'imputed_models_vif' = m2_vif_table)

# Add tables to different worksheets based on list's name
lapply(names(diagnostics_list), function(name){
  
  openxlsx::addWorksheet(wb, name)
  writeData(wb, name, diagnostics_list[[name]])
  
})

openxlsx::saveWorkbook(
  wb, paste0(
    cohort, '_imputed_data_glmer_diagnostics',
    file_date, '.xlsx'),
  overwrite = TRUE)

#03 Imputed data: hot deck -----------------------------------------------------

# Stuff to think about ---------------------------------------------------------
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
