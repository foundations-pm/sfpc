# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set-up  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'QA/processing/linked_data/')

output_path = paste0(sharepoint_path, 'QA/outputs/')

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions 
{ source(paste0(wd, "functions.R"))}

# Load data ----

# 1 load data 
data <- readRDS(file = paste0(
  output_path, 'primary_analysis_analytical_dataset.Rds'))


# Sensitivity analyses ----

## S1: Switching imputation methods ----

### LA imputation ----
# Impute data: use LA to impute 

reg_predm <- make.predictorMatrix(model_data)

reg_predm["ethnicity_agg", ] <- c(
  0, 0, 1, 1, # child ID, ref date: out, outcome: in, LA: in 
  0,1,1,1, # is_norfolk = out
  1,1,1,1,
  1,1,1,1,
  1,1,1) 

reg_imp_data <- mice::mice(
  model_data,
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = reg_predm,
  maxit = 1000)

# Save datasets
setwd(paste0(output_path, "imputed_datasets/"))

miceadds::write.mice.imputation(
  reg_imp_data, 
  name = "LA_single_level_imputation", 
  include.varnames=TRUE,
  long=TRUE, 
  mids2spss=FALSE,
  dattype=NULL)

# Fit model

# Formula
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     'referral_no_further_action',
                     sep = " + ")

re = " (1 | local_authority)"

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

# Fit model 
m3 = with(
  reg_imp_data, 
  lme4::glmer(
    as.formula(
      formula), # RE intercept 4 clusters
    family = binomial))

# Check summary 
pooled_results <- pool(m3)
m3_summary <- summary(pooled_results)

# Save raw results
# Create a data frame with coefficients, standard errors, and p-values
raw_m3 <- data.frame(
  model_type = 'Imputation using LA as a 4 level factor',
  formula = formula,
  m = 5,
  iteration = 1000,
  Coefficients = m3_summary$estimate,       # Log Odds
  `Standard Error` = m3_summary$std.error,
  `Statistic` = m3_summary$statistic,           # Optional
  `df` =  m3_summary$df,
  `p-value` = m3_summary$p.value
)

# Export the data frame to a CSV file
writexl::write_xlsx(
  raw_m3, 
  paste0(
    output_path,
    "model_outputs/",
    "raw_imputed_la_only_glmer_model.xlsx"))

# Tidy results 

tidy_m3 = broom.mixed::tidy(
  pooled_results, conf.int=TRUE, 
  exponentiate=TRUE,
  effects="fixed")

tidy_m3 = tidy_m3 %>%
  dplyr::mutate(
    across(where(is.numeric), round, 4),
    model = 'imputed_data_using_la',
    formula = formula) %>%
  dplyr::relocate(model, formula)

writexl::write_xlsx(
  tidy_m3,
  paste0(output_path,
         "model_outputs/",
         "imputed_la_only_glmer_model.xlsx"))

#2 Check optimisers:
aa <- allFit(m3)
ss <- summary(aa)
ss$msgs[!sapply(ss$msgs,is.null)]

### Multilevel imputation ----

ml_predm <- make.predictorMatrix(model_data)

ml_predm[, 'local_authority'] <- -2  # Cluster is a level-2 variable

ml_predm["ethnicity_agg", ] <- c(
  0, 0, 1, # child ID, ref date, outcome: not auxiliary vars
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

tidy_m4 = tidy_m4 %>%
  dplyr::mutate(
    across(where(is.numeric), round, 4),
    model = 'multilevel_imputed_data',
    formula = formula) %>%
  dplyr::relocate(model, formula)

writexl::write_xlsx(
  tidy_m4,
  paste0(output_path,
         "model_outputs/",
         "multilevel_imputation_glmer_model.xlsx"))

#2 Check optimisers:
aa <- allFit(m4)
ss <- summary(aa)
ss$msgs[!sapply(ss$msgs,is.null)]

## S2: Posthoc analyses ----
s_data = data %>% 
  filter(gender != 'Other') %>% 
  dplyr::mutate(
    gender = relevel(
      factor(as.character(gender)), ref = 'Male'),
    local_authority = as.character(local_authority))

### Fixed effect and clustered CI ----

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

# Cluster-robust covariance matrix
clustered_se <- vcovCR(
  m1_fe, 
  cluster = data[data$gender != 'Other', ]$local_authority, 
  type = 'CR3')

# Calculate confidence intervals
confint_robust <- coefci(
  m1_fe, 
  vcov = clustered_se,
  test = 'naive.t',
  conf.int = TRUE)

# Convert to a data frame for easier handling
confint_robust <- as.data.frame(confint_robust)
colnames(confint_robust) <- c("conf.low", "conf.high") # Rename columns

# Extract coefficient estimates
coef_estimates <- coef(m1_fe)

# Combine estimates and CIs
results <- data.frame(
  term = names(coef_estimates),                      # Variable names
  odds_ratio = exp(coef_estimates),                  # Odds ratio (exp of coefficient)
  conf.low = exp(confint_robust$conf.low),           # Lower bound of CI on OR scale
  conf.high = exp(confint_robust$conf.high)          # Upper bound of CI on OR scale
)

# Print the results
print(results)

# Tidy results
tidy_m1_fe = broom::tidy(
  m1_fe, conf.int=TRUE, 
  exponentiate=TRUE,
  effects="fixed")

complete_case_tb_fe = tidy_m1_fe %>%
  dplyr::mutate(
    across(where(is.numeric), round, 4),
    model = 'complete_case',
    formula = formula) %>%
  dplyr::relocate(model, formula)


# TECHNIQUE 2
# Cluster-robust standard errors and p-values
lmtest::coeftest(m1_fe, vcov = clustered_se)

# Exponentiate the coefficients and confidence intervals to get odds ratios
exp_coef <- exp(coef(m1_fe))
exp_ci <- exp(confint_robust)

# Print exponentiated coefficients and CIs (Odds Ratios and their CIs)
results <- cbind(OR = exp_coef, CI_Lower = exp_ci[, 1], CI_Upper = exp_ci[, 2])
print(results)

### Random effect for time ----

re = " + (1 | local_authority) + (1 | wedge)"

cluster_indicator = c(
  #" + cla_cin_cpp_rate_per_10_000_children",
  #" + cla_rate_per_10_000_children",
  #" + cla_cin_cpp_rate_per_10_000_children",
  " + splines::ns(cla_cin_cpp_rate_per_10_000_children, df = 5)")

formula = paste0(
  "cla_status ~ treatment_group + ", # FE for trt 
  demographics, # adjust for person level demographics
  cluster_indicator, # adjust for time-varying cluster level indicators
  re # random intercept for time and LA
) # RE intercept 4 clusters

# Fit model 
m1 = lme4::glmer(
  as.formula(formula), 
  data = data[data$gender != 'Other', ],
  family = binomial)

# Check summary 
summary_m1 = summary(m1)

#2 Check optimisers:
aa <- allFit(m1)
ss <- summary(aa)
ss$msgs[!sapply(ss$msgs,is.null)]

# Save raw estimates
raw_m1 <- data.frame(
  model_type = "complete case analysis",
  formula = formula,
  Coefficients = summary_m1$coefficients[, "Estimate"],       # Log Odds
  `Standard Error` = summary_m1$coefficients[, "Std. Error"],
  `z value` = summary_m1$coefficients[, "z value"],           # Optional
  `p-value` = summary_m1$coefficients[, "Pr(>|z|)"])

# Export the data frame to a CSV file
#writexl::write_xlsx(
#  raw_m1, 
#  paste0(
#    output_path,
#    "model_outputs/",
#    "raw_complete_case_glmer_model.xlsx"))

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

#writexl::write_xlsx(
#  complete_case_tb,
#  paste0(output_path,
#         "model_outputs/",
#         "complete_case_glmer_model.xlsx"))

### Remove Rochdale ----
test = lme4::glmer(
  as.formula(
    paste0(formula)), 
  data = s_data[s_data$local_authority != 'rochdale', ],
  family = binomial)

table = broom.mixed::tidy(test,
                          conf.int=TRUE, 
                          exponentiate=TRUE)

s_model_1 = lapply(
  
  unique(s_data$local_authority),
  
  function(la) {
    
    data = s_data[s_data$local_authority != la,]
    print(unique(data$local_authority))
    
    model = lme4::glmer(
      as.formula(
        paste0(formula)), 
      data = data,
      family = binomial)
    
    return(model)
    
  })

names(s_model_1) = unique(s_data$local_authority)

# Save raw estimates
raw_s1 <- purrr::map_dfr(
  names(s_model_1),
  function(la){
    
    summary = summary(s_model_1[[la]])
    
    data.frame(
      model_type = "LA-specific analysis",
      la_removed = la,
      formula = formula,
      Coefficients = summary$coefficients[, "Estimate"],       # Log Odds
      `Standard Error` = summary$coefficients[, "Std. Error"],
      `z value` = summary$coefficients[, "z value"],           # Optional
      `p-value` = summary$coefficients[, "Pr(>|z|)"])
    
  }) 

# Export the data frame to a CSV file
writexl::write_xlsx(
  raw_s1,
  paste0(
    output_path,
    "model_outputs/",
    "raw_la_specific_complete_case_glmer_model.xlsx"))

# Tidy results
tidy_s1 = purrr::map_dfr(
  names(s_model_1), function(la){
    
    complete_case_tb = broom.mixed::tidy(
      s_model_1[[la]], conf.int=TRUE, 
      exponentiate=TRUE,
      effects=c("fixed"))
    
    complete_case_tb = complete_case_tb %>%
      dplyr::mutate(
        across(where(is.numeric), round,4),
        model = 'LA-specific analysis',
        la_removed = la,
        formula = formula) %>%
      dplyr::relocate(model, la_removed, formula)
    
  })

writexl::write_xlsx(
  tidy_s1,
  paste0(output_path,
         "model_outputs/",
         "la_specific_complete_case_glmer_model.xlsx"))

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
