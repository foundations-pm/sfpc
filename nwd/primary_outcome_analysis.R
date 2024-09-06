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

# Time unvarying 
#data = readxl::read_excel(
#  paste0(
#    data_path, "time_unvarying_analytical_dataset.xlsx"))

# Person-level time dependent  
data = readxl::read_excel(
  paste0(
    data_path,
    "time_dependent_person_level_analytical_dataset.xlsx"))

# Workplan ---- 
#0 complete & describe; diagnosis
#1 data cleaning 
#2 derive wedge, treatment and outcome
#3 fit models 

##1. Complete data ----

# 1 Change dates back to as.Date (lubridate) class
# 2 Turn nb of previous CPP plans into categorical variable
# 3 Add variable for readiness 
data = mutate(
  data,
  across(.cols = c('referral_date',
                   'eosp',
                   'year_and_month_of_birth_of_the_child',
                   'date_turned_18',
                   'date_period_of_care_commenced'),
         .fns = as.Date),
  across(.cols = c("is_censored",
                   "eligibility",
                   "care_period_number"),
         .fns = as.character),
  number_of_previous_child_protection_plans = case_when(
    number_of_previous_child_protection_plans ==  0 ~ '0',
    number_of_previous_child_protection_plans ==  1 ~ '1',
    number_of_previous_child_protection_plans ==  2 ~ '2',
    number_of_previous_child_protection_plans > 2 ~ "3+"),
  number_of_previous_child_protection_plans = factor(
    as.character(number_of_previous_child_protection_plans),
    levels = c('0', '1', '2', '3+')),
  readiness = ifelse(
    local_authority %in% c('rochdale', 'warrington'), 
    'High readiness', 'Low readiness'))

# 2 describe before data cleaning
var_class = c("numeric", "categorical", "date")
vars_to_exclude = c("child_id", "referral_id", "referral_id_or_case_id")

data_description = list()

for(class in var_class){
  
  data_description[[class]] = data %>% 
    dplyr::select(-any_of(vars_to_exclude)) %>%
    describe(class = class) }

##2. Clean data -----

###0. Clean values ----
# Categorical vars to clean:

# referral_no_further_action
# gender
# ethnicity
# disabled_status
# unaccompanied_asylum_seeker
# free_school_meal_eligibility_ever_fsm
# pupil_premium_eligibility_for_reception_year_1_and_year_2

data = recode_values(data) # recode value function
# function is in function.R

# Set factor levels 

data = mutate(
  data,
  
  gender = relevel(
    factor(gender), ref = 'Male'),
  
  ethnicity_agg = relevel(
    factor(ethnicity_agg), ref = 'White British or Irish'),
  
  disabled_status = relevel(
    factor(disabled_status), ref = '0'),
  
  unaccompanied_asylum_seeker = relevel(
    factor(unaccompanied_asylum_seeker), ref = '0'),
  
  referral_no_further_action = relevel(
    factor(referral_no_further_action), ref = '0'),
  
  number_of_previous_child_protection_plans = relevel(
    number_of_previous_child_protection_plans,
    ref = '0'))

# sort column order
colnames = colnames(data)

data = relocate(
  data, 
  local_authority, readiness, child_id,
  colnames[3:15], ethnicity_agg)

# describe 
cleaned_data_description = list()
vars_to_exclude = c("child_id", "referral_id", 
                    "referral_id_or_case_id", 'ethnicity')

for(class in var_class){
  
  cleaned_data_description[[class]] = data %>% 
    dplyr::select(-any_of(vars_to_exclude)) %>%
    describe(class = class) }

###1. Trial wedges (secular trends) ----

# Trial dates: Oct 2019 to March 22; 2019-10-01 to 2022-03-31

# Period 0: Baseline; 2019-10-01 to 2020-03-31
# Period 1: Rochdale in treatment; 2020-04-01 
# Period 2: Norfolk in treatment; 2021-06-01 
# Period 3: Warrington in treatment; 2021-04-01 
# Period 4: Redcar in treatment; 2021-09-01

baseline = seq(from = as.Date('2019-10-01'),
                   to =  as.Date('2020-03-31'), 
                   by = "day")
wedge_1 = seq(from = as.Date('2020-04-01'),
                   to =  as.Date('2021-03-31'), 
                   by = "day")
wedge_2 = seq(from = as.Date('2021-04-01'),
                   to =  as.Date('2021-05-31'), 
                   by = "day")
wedge_3 = seq(from = as.Date('2021-06-01'),
                   to =  as.Date('2021-08-31'), 
                   by = "day")
wedge_4 = seq(from = as.Date('2021-09-01'),
              to =  as.Date('2022-03-31'), 
              by = "day")

data = mutate(
  data, 
  wedge  = case_when(
    referral_date %in% baseline ~ 'baseline',
    referral_date %in% wedge_1 ~ 'wedge_1',
    referral_date %in% wedge_2 ~ 'wedge_2',
    referral_date %in% wedge_3 ~ 'wedge_3',
    referral_date %in% wedge_4 ~ 'wedge_4',
    ))

###2. Treatment assignment ----

# 0 if not within the treatment period, 
# 1 if within the treatment period 

data = data %>%
  mutate(
  la_treatment_start  = case_when(
    local_authority == 'rochdale' ~ as.Date('2020-04-01'),
    local_authority == 'warrington' ~ as.Date('2021-04-01'),
    local_authority == 'norfolk' ~ as.Date('2021-06-01'),
    local_authority == 'redcar' ~ as.Date('2021-09-01'))) %>%
  group_by(local_authority) %>%
  mutate(
  treatment_group = ifelse(
    referral_date >= la_treatment_start, 1, 0)) %>%
  ungroup()

###3. Outcome ----

# 1 if has become looked after within 18 months of first referral 
# 0 otherwise 

data = data %>%
  mutate(
    cla_status  = case_when(
      is.na(date_period_of_care_commenced) ~ 0,
      date_period_of_care_commenced >= referral_date & 
        date_period_of_care_commenced <= eosp ~ 1,
      date_period_of_care_commenced >= referral_date & 
        date_period_of_care_commenced > eosp ~ 0,
      TRUE ~ NA))

###4. Cross-contamination ----

# Social services must respond to a referral within 45 days 
# Hypothesis: children referred under control condition less than 45 days 
# before their cluster switched to the treatment condition might have been 
# contaminated 

# Deriving indicator for it: 
data = data %>%
  mutate(
    cross_contamination  = ifelse(
      referral_date >= (la_treatment_start - 45) & 
        referral_date < la_treatment_start,
      1, 0))

# Missingness ----

missing_data = mutate(
  data,
  is_missing_ethnicity = ifelse(is.na(ethnicity_agg), 1, 0),
  is_missing_gender = ifelse(gender == 'Other', 1, 0))

covariate_list = colnames(missing_data[
  ,c(2,7,12,13,14,18,19,26)])

p_vals = list()

for(i in 1:length(covariate_list)){
  
  covariate = covariate_list[i]
  
  missing_model = summary(
    lme4::glmer(
      as.formula(
        paste0(
          "is_missing_ethnicity ~ ", 
          covariate,
          " + (1 | local_authority)")),
      data = missing_data,
      family = binomial))
  
  p_vals[i] = missing_model$coefficients[2,4]
  
}

# ICC ----

# Step 1: get filtered dataset
desc_data = filter(
  data,
  is.na(care_period_number) |
  care_period_number == 1#,
  #!is.na(ethnicity_agg),
  #gender != 'Other'
  )

# Step 2: get ICC
model_bin <- glmer(
  cla_status ~ 1 + (1 | local_authority) + (1 | local_authority:wedge), 
  family = binomial, data = desc_data)

# Summary to get variance components
summary(model_bin)

# Extract variance components as done before
# Variances are on the logit scale for binary outcomes
var_components_bin <- as.data.frame(VarCorr(model_bin))

# Between-cluster and cluster-period variances
var_cluster_bin <- var_components_bin[
  which(
    var_components_bin$grp == "local_authority"), "vcov"]

var_cluster_time_bin <- var_components_bin[
  which(
    var_components_bin$grp == "local_authority:wedge"), "vcov"]

# For binary outcomes, residual variance is fixed at π²/3 on the logistic scale
var_residual_bin <- pi^2 / 3

# Calculate the ICC for binary outcome
icc_binary <- var_cluster_bin / (
  var_cluster_bin + var_cluster_time_bin + var_residual_bin)

# Output the binary ICC
print(paste("Binary outcome ICC: ", round(icc_binary, 5)))

# Cohort description ----

# 1 Cohort sizes 
# Number of unique children referred:
# Total, by lA, by wedge, by wedge by LA > 
# By wedge = cluster sizes 
# By wedge by LA = cross sectional cohorts during trial period 

# Total
length(
  unique(
    desc_data$child_id)) # 9,107 children were referred between Oct 2019 and March 2022 

# By LA
desc_data %>% 
  group_by(local_authority) %>%
  summarise(count = n_distinct(child_id))

# By wedge
desc_data %>% 
  group_by(wedge) %>%
  summarise(count = n_distinct(child_id))

# By LA by wedge 
desc_data %>% 
  group_by(local_authority, wedge) %>%
  summarise(count = n_distinct(child_id))

# 2 Treatment
# Number unique children who received treatment vs did not (were referred under control condition)
# Total, by LA, by wedge, by wedge by LA

# Total
desc_data %>% 
  group_by(treatment_group) %>%
  summarise(count = n_distinct(child_id))

# By LA
desc_data %>% 
  group_by(local_authority, treatment_group) %>%
  summarise(count = n_distinct(child_id))

# By wedge
desc_data %>% 
  group_by(wedge, treatment_group) %>%
  summarise(count = n_distinct(child_id))

# By LA by wedge 
desc_data %>% 
  group_by(local_authority, wedge, treatment_group) %>%
  summarise(count = n_distinct(child_id))

# 3 Outcome 
# Number of unique children who went into care vs did not within 18months 
# Total, by LA, by wedge, by wedge by LA

# 4 Cross-contamination 
# Nb of children referred within 45 days of their cluster switching to the treatment condition

# 5 Balance checks:
# between control and treatment groups

# Data cleaning: exclusion criteria
# How many records were removed?

# To do: data flow + numbers 
# See CONSORT guidelines: participant flow

# total records from start
# 0 invalid records: 
# age is not missing
# age must be > referral date
# 
# 1 children need to be 12-17 when referred 
# 2 only children within trial period are included 
# 3 only first referral was kept (how many total referrals, how many referrals removed)
# 3 children with a care period starting before their referral are removed (= nb)
# 4 children with multiple care period: first care period kept (number record removed)



# Fit model ----

# FILTERS FOR MODEL ----
# QA: 16 children went to care before their referral date 
# 1- Have to filter these out 
# 2- Have to filter for care period == 1

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

# Filters to check ----
# Filter 
model_data = filter(data,
                    is.na(care_period_number) |
                    care_period_number == 1,
                    !is.na(ethnicity_agg),
                    gender != 'Other')

# Prep formula 
individual_covariates = paste('age_at_referral',
                              'gender',
                              'ethnicity_agg',
                              'disabled_status',
                              'unaccompanied_asylum_seeker',
                              'number_of_previous_child_protection_plans',
                              'referral_no_further_action',
                              sep = " + ")

# Fit model: simplest
# FE for time, RE for clusters 

# Assumptions for this model: 

# Fixed effect assumption
# 1 Common secular trend: 
# The effect of time is common across clusters and sequences 
# Modelled as categorical: not assuming any parametric shape (e.g. linear)
# 2 constant intervention effect: 
# The difference between control and treatment in outcomes is constant through time 

# 

model_fe = lme4::glmer(
  as.formula(
    paste0(
      "cla_status ~ treatment_group + wedge + readiness + ",
      individual_covariates,  " + (1 | local_authority)")), 
  data = model_data,
  family = binomial)

# Model 2: standard model 2 (time unvarying, random effects only)
# Random effects for clusters, random effects for time; time-unvarying indicators only

# Model 3: standard model 3 (time varying LA indicators, random & fixed effects)
# Random effects for clusters, fixed effects for time; time-varying LA indicators 

# Model 3: standard model 3 (time varying LA indicators, random & fixed effects)
# Random effects for clusters, fixed effects for time; time-varying LA indicators 


