# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set-up  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - No Wrong Door')

# Data and output paths
data_path = paste0(sharepoint_path, '/Working folder/processing/')

output_path = paste0(sharepoint_path, '/Working folder/outputs/')

descriptive_path = paste0( sharepoint_path,
                           "QA/outputs/descriptives/",
                           "Sensitivity cohort description/")

# Working directory
wd = paste0(user_directory, "Documents/SFPC/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions 
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
    'linked_data/sensitivity_cohort/',
    "time_dependent_person_level_analytical_dataset_V1.xlsx"))

# Add yearly ONS & DfE estimates ----

# Clean ONS MYEB1 (mid-year population estimate, by age, sex and LA)
ons_mye_2023 = readxl::read_excel(
  paste0(
    data_path,
    'additional data sources/',
    "ons_myeb1_mid_apr_2023.xlsx"))

ons_mye_2023 = ons_mye_2023 %>%
  dplyr::mutate(local_authority = ifelse(
    laname23 %in% c('Breckland', 'Broadland', 'Great Yarmouth',
                    "King's Lynn and West Norfolk", 'North Norfolk',
                    'Norwich','South Norfolk'),
    'norfolk', tolower(laname23))) %>%
  dplyr::mutate(local_authority = ifelse(
    local_authority == 'redcar and cleveland', 
    'redcar', local_authority))

ons_mye_2023 = ons_mye_2023 %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(across(
    .cols = any_of(contains('population')),
    .fns = sum)) 

ons_mye_2023 = tidyr::pivot_longer(
  ons_mye_2023, 
  cols = colnames(ons_mye_2023)[2:ncol(ons_mye_2023)],
  names_to = 'year',
  values_to = 'population_0_to_17')

ons_mye_2023 = dplyr::mutate(
  ons_mye_2023,
  year = as.numeric(
    stringr::str_remove(year, 'population_')))

# Clean DfE CSC staff turnover
dfe_staff_turnover = read.csv(
  paste0(
    data_path,
    'additional data sources/',
    "dfe_dataset_childrens_social_work_workforce_2023.csv"))

dfe_staff_turnover = dfe_staff_turnover %>%
  dplyr::mutate(
    year = as.numeric(time_period),
    la_name = case_when(
      la_name == 'Redcar and Cleveland' ~ 'redcar',
      TRUE ~ tolower(la_name))) %>%
  dplyr::rename(local_authority = la_name) %>% 
  dplyr::select(-new_la_code, -time_period)

# Join data 
data = data %>% 
  dplyr::mutate(
    year = lubridate::year(child_protection_plan_start_date)) %>%
  dplyr::relocate(year, .after = cp_number)

data = left_join(
  data, ons_mye_2023,
  by = c('local_authority', 'year'))

data = left_join(
  data, dfe_staff_turnover,
  by = c('local_authority', 'year'))

# Workplan ---- 
#0 complete & describe; diagnosis
#1 data cleaning 
#2 derive wedge, treatment and outcome
#3 fit models 

##1. Complete data ----

# 1 Change dates back to as.Date (lubridate) class
# 2 Turn nb of previous CPP plans into categorical variable
# 3 Add variable for readiness 
data = dplyr::mutate(
  data,
  across(.cols = c('child_protection_plan_start_date',
                   'eosp',
                   'year_and_month_of_birth_of_the_child',
                   'date_turned_18',
                   'date_period_of_care_commenced',
                   'month'),
         .fns = as.Date),
  across(.cols = c("is_censored",
                   "eligibility",
                   "care_period_number"),
         .fns = as.character),
  age_at_cp_start_cat = as.character(age_at_cp_start),
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

#data_description = list()

#for(class in var_class){

#  data_description[[class]] = data %>% 
#    dplyr::select(-any_of(vars_to_exclude)) %>%
#    describe(class = class) }

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

data = dplyr::mutate(
  data,
  
  age_at_cp_start_cat = relevel(
    factor(age_at_cp_start_cat), ref = '12'),
  
  gender = relevel(
    factor(gender), ref = 'Male'),
  
  ethnicity_agg = relevel(
    factor(ethnicity_agg), ref = 'White British or Irish'),
  
  disabled_status = relevel(
    factor(disabled_status), ref = 'Not disabled'),
  
  unaccompanied_asylum_seeker = relevel(
    factor(unaccompanied_asylum_seeker), ref = 'Not UASC'),
  
  number_of_previous_child_protection_plans = relevel(
    number_of_previous_child_protection_plans,
    ref = '0'))

# sort column order
colnames = colnames(data)

data = data %>%
  relocate(readiness, .after = local_authority) %>%
  relocate(age_at_cp_start_cat, .after = age_at_cp_start) %>%
  relocate(ethnicity_agg, .after = ethnicity) %>%
  select(-month_id)

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

data = dplyr::mutate(
  data, 
  wedge  = case_when(
    child_protection_plan_start_date %in% baseline ~ 'baseline',
    child_protection_plan_start_date %in% wedge_1 ~ 'wedge_1',
    child_protection_plan_start_date %in% wedge_2 ~ 'wedge_2',
    child_protection_plan_start_date %in% wedge_3 ~ 'wedge_3',
    child_protection_plan_start_date %in% wedge_4 ~ 'wedge_4',
  ))

###2. Treatment assignment ----

# 0 if not within the treatment period, 
# 1 if within the treatment period 

data = data %>%
  dplyr::mutate(
    la_treatment_start  = case_when(
      local_authority == 'rochdale' ~ as.Date('2020-04-01'),
      local_authority == 'warrington' ~ as.Date('2021-04-01'),
      local_authority == 'norfolk' ~ as.Date('2021-06-01'),
      local_authority == 'redcar' ~ as.Date('2021-09-01'))) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::mutate(
    treatment_group = ifelse(
      child_protection_plan_start_date >= la_treatment_start, 1, 0)) %>%
  ungroup()

###3. Outcome ----

# 1 if has become looked after within 18 months of first referral 
# 0 otherwise 

data = data %>%
  dplyr::mutate(
    cla_status  = case_when(
      is.na(date_period_of_care_commenced) ~ 0,
      date_period_of_care_commenced >= child_protection_plan_start_date & 
        date_period_of_care_commenced <= eosp ~ 1,
      date_period_of_care_commenced >= child_protection_plan_start_date & 
        date_period_of_care_commenced > eosp ~ 0,
      TRUE ~ NA))

###4. Cross-contamination ----

# Social services must respond to a referral within 45 days 
# Hypothesis: children referred under control condition less than 45 days 
# before their cluster switched to the treatment condition might have been 
# contaminated 

# Deriving indicator for it: 
data = data %>%
  dplyr::mutate(
    cross_contamination  = ifelse(
      child_protection_plan_start_date >= (la_treatment_start - 45) & 
        child_protection_plan_start_date < la_treatment_start,
      1, 0))

###5. Rate of CIN/CPP per 10,000 children ----
data = data %>% 
  dplyr::group_by(local_authority, year) %>%
  dplyr::mutate(
    new_referrals_rate_per_10_000_children = as.numeric( 
      number_of_new_referrals_this_month_in_the_la/
        population_0_to_17 * 10000),
    cin_rate_per_10_000_children = 
      number_of_open_cin_cases_this_month_in_the_la/
      population_0_to_17 * 10000,
    cpp_rate_per_10_000_children = 
      number_of_open_cp_ps_this_month_in_the_la/
      population_0_to_17 * 10000,
    cla_rate_test = as.numeric(
      number_of_children_looked_after_at_the_end_of_the_month_in_the_la/
        population_0_to_17 * 10000)) %>%
  dplyr::ungroup() %>%
  relocate(cla_rate_test,
           cpp_rate_per_10_000_children,
           cin_rate_per_10_000_children,
           new_referrals_rate_per_10_000_children,
           .after = cla_rate_per_10_000_children)

mean_checks = data %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(mean(cpp_rate_per_10_000_children),
                   mean(cin_rate_per_10_000_children),
                   mean(cla_rate_per_10_000_children),
                   mean(cla_rate_test),
                   mean(new_referrals_rate_per_10_000_children))

# CiN not super close to figures reported by DfE
# CPP and CLA rates, using the 'total at the end of the month' pretty similar
# Use 2024 DfE data to check: 
# https://explore-education-statistics.service.gov.uk/data-tables/children-in-need

#monthly_data = data %>% 
#  dplyr::group_by(local_authority) %>%
#  dplyr::distinct(month, .keep_all = TRUE) %>%
#  ungroup() %>%
#  dplyr::group_by(month) %>% 
#  dplyr::summarise(
#    total_cin = sum(total_cin),
#    total_cpp = sum(total_cpp),
#    total_cla = sum(total_cla),
#    total_pop_2019 = sum(population_2019),
#    total_pop_2020 = sum(population_2020),
#    total_pop_2021 = sum(population_2021),
#    total_pop_2022 = sum(population_2022),
#  ) %>%
#  dplyr::mutate(
#    overvall_rate = case_when(
#      year(month) == 2019 ~ (total_cin + total_cpp + total_cla) / total_pop_2019 * 10000,
#      year(month) == 2020 ~ (total_cin + total_cpp + total_cla) / total_pop_2020 * 10000,
#      year(month) == 2021 ~ (total_cin + total_cpp + total_cla) / total_pop_2021 * 10000,
#      year(month) == 2022 ~ (total_cin + total_cpp + total_cla) / total_pop_2022 * 10000)
#  )

# Streamline data
data = select(
  data,
  #-contains('population'),
  -contains('number_of_c'),
  -contains('number_of_o'),
  -contains('number_of_n'),
  -contains('total_'),
  #  -total_cin_cpp,
  -year,
  -month_return,
  -free_school_meal_eligibility_ever_fsm,
  -pupil_premium_eligibility_for_reception_year_1_and_year_2)

###6. Proportion White British ----
# per month per LAs at month of referral

wb_table = data %>%
  dplyr::mutate(white_british = ifelse(
    ethnicity_agg == 'White British or Irish', 1, 0)) %>%
  dplyr::group_by(local_authority, month) %>%
  dplyr::summarise(total_referred = sum(n()), 
                   total_wb = sum(as.numeric(white_british),
                                  na.rm = TRUE),
                   prop_white_british = total_wb/total_referred)

data = left_join(data, wb_table, 
                 join_by('local_authority', 'month'))

data = data %>%
  relocate(prop_white_british, .after = ethnicity_agg) %>%
  select(-total_referred, -total_wb)

###7. Filters ----

data = data%>%
  dplyr::filter(
    is.na(care_period_number) |
      care_period_number == 1) %>% # keep first date of care only
  dplyr::filter(!is.na(cla_status)) # make sure date of care >= open CP date

lapply(colnames(data), function(x) { 
  sum(is.na(data[[x]]))})

###8. Save analytical dataset ----
# Saving as RDS retains data class etc. 
saveRDS(data, file = paste0(
  output_path,"sensitivity_analysis_open_cp_analytical_dataset_V2.Rds")) 

writexl::write_xlsx(
  data,
  path = paste0(
    output_path,
    "sensitivity_analysis_open_cp_analytical_dataset_V2.xlsx"))

# Analytical dataset descriptives ----

# describe 
cleaned_data_description = list()
vars_to_exclude = c("child_id", "referral_id", 
                    "referral_id_or_case_id", 'ethnicity')

for(class in var_class){
  
  cleaned_data_description[[class]] = data %>% 
    dplyr::select(-any_of(vars_to_exclude)) %>%
    describe(class = class) }

cleaned_data_description_by_la = list()

for(class in var_class){
  
  cleaned_data_description_by_la[[class]] = data %>% 
    dplyr::select(-any_of(vars_to_exclude)) %>%
    dplyr::group_by(local_authority) %>%
    describe(class = class, 
             group = 'local_authority') }

cleaned_data_description_by_la_by_wedges = list()

for(class in var_class){
  
  cleaned_data_description_by_la_by_wedges[[class]] = data %>% 
    dplyr::select(-any_of(vars_to_exclude)) %>%
    dplyr::group_by(local_authority, wedge) %>%
    describe(class = class, 
             group = c('local_authority', 'wedge')) }

cleaned_data_description_by_la_by_treatment = list()

for(class in var_class){
  
  cleaned_data_description_by_la_by_treatment[[class]] = data %>% 
    dplyr::select(-any_of(vars_to_exclude)) %>%
    dplyr::group_by(local_authority, treatment_group) %>%
    describe(class = class, 
             group = c('local_authority', 'treatment_group')) }

# Save tables
writexl::write_xlsx(
  cleaned_data_description,
  path = paste0(descriptive_path,
                "Analytical dataset/",
                "dataset_description.xlsx"))

la_desc_list = c(cleaned_data_description_by_la,
                 cleaned_data_description_by_la_by_wedges,
                 cleaned_data_description_by_la_by_treatment)

writexl::write_xlsx(
  la_desc_list,
  path = paste0(descriptive_path,
                "Analytical dataset/",
                "dataset_by_la_description.xlsx"))

# Plot DR1 distribution

monthly_la_data = data %>%
  dplyr::group_by(local_authority) %>%
  dplyr::distinct(month, .keep_all = TRUE) %>%
  dplyr::mutate(month = as.Date(month))

custom_colors <- c("rochdale" = "#E89633", 
                   "warrington" = "#708CCC", 
                   "norfolk" = "#E86E42", 
                   "redcar" = "#464C8B")

# https://commonslibrary.parliament.uk/research-briefings/cbp-9068/


title = "CLA rate per 10,000 children"

cla_plot = get_monthly_rates(
  monthly_cla_data,
  rate = 'cla_rate_per_10_000_children',
  title = title)

title = "CIN/CPP rate per 10,000 children"

cin_cpp_plot = get_monthly_rates(
  monthly_cla_data,
  #rate = 'total_cin_cpp',
  rate = 'cin_cpp_rate_per_10_000_children',
  title = title)

title = "CLA and CIN/CPP rate per 10,000 children"

cla_cin_cpp_plot = get_monthly_rates(
  monthly_cla_data,
  #rate = 'total_cin_cpp',
  rate = 'cla_cin_cpp_rate_per_10_000_children',
  title = title)

dr1_cla_distribution = data %>% 
  select(local_authority, 
         cla_rate_per_10_000_children, 
         cin_cpp_rate_per_10_000_children,
         cla_cin_cpp_rate_per_10_000_children) %>%
  group_by(local_authority) %>%
  describe(.,
           group = 'local_authority')

writexl::write_xlsx(
  dr1_cla_distribution,
  path = paste0(descriptive_path,
                "Analytical dataset/",
                "dr1_cla_cin_cpp_distribution.xlsx"))

ggsave(
  paste0(descriptive_path, 
         "Analytical dataset/", 
         "cla_distribution_plot.jpg"),
  cla_plot,
  dpi = 300)

ggsave(
  paste0(descriptive_path, 
         "Analytical dataset/", 
         "cin_cpp_distribution_plot.jpg"),
  cin_cpp_plot,
  dpi = 300)

ggsave(
  paste0(descriptive_path, 
         "Analytical dataset/", 
         "cla_cin_cpp_distribution_plot.jpg"),
  cla_cin_cpp_plot,
  dpi = 300)


# ICC ----

model_bin <- glmer(
  cla_status ~ 1 + (1 | local_authority) + (1 | local_authority:wedge), 
  family = binomial, data = data)

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

# Randomisation checks ----

# Reflection
# Threat to randomisation:
# 'High readiness' = correlated with less chances of becoming CLA? 
# Very possible in this case

baseline_data = filter(
  data, wedge == 'baseline')

strata_outcome_corr_check = check_mnar(
  data = baseline_data,
  missing_covariate = 'cla_status',
  auxiliary = 'readiness')

View(strata_outcome_corr_check)
# Readiness is NOT associated with CLA status for the baseline cohort 
# The risk of becoming looked after for people at baseline is not associated with
# How ready their cluster was to implement NWD

# Baseline outcome equivalence ----
# Cluster heterogeneity in the proportion of CLA children 

#1 Check observed proportion/likelihood/risk of CLA 
baseline_data %>%
  dplyr::group_by(local_authority, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# For the baseline cohort: 
# 6% CLA within 18 of first referral in Rochdale, Norfolk, Warrington
# 4% in Redcar 

# Overall during the study period:
data %>%
  dplyr::group_by(local_authority, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View() 

# For the entire cohort: 
# 7% CLA within 18 of first referral in Norfolk, Warrington and Redcar
# 4% in Rochdale 

data %>%
  dplyr::group_by(local_authority, wedge, cla_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  arrange(cla_status, local_authority, wedge) %>%
  View() 

# SMD for CLA status by LA and readiness
table_one <- tableone::CreateTableOne(
  vars = 'cla_status', 
  strata = 'readiness',
  factorVars = 'cla_status',
  includeNA = TRUE,
  data = baseline_data)

smd_table = print(table_one, smd = TRUE)

# SMD of 0.023 for proportion of CLA for high ready and low ready strata
# SMD not significant > weak association & non-significant 
# Good sign?

# SMD for CLA status by LA and readiness
table_one <- tableone::CreateTableOne(
  vars = 'cla_status', 
  strata = 'readiness',
  factorVars = 'cla_status',
  includeNA = TRUE,
  data = baseline_data)

smd_table = print(table_one, smd = TRUE)

# SMD of 0.023 for proportion of CLA for high ready and low ready strata
# SMD not significant > weak association & non-significant 
# Good sign?

# Cluster heterogeneity ----

#1 Check SMDs for different baseline demographics by LA
covariates = c(
  'local_authority',
  'age_at_cp_start_cat',
  #'age_at_cp_start',
  'gender',
  'ethnicity_agg',
  'disabled_status',
  'unaccompanied_asylum_seeker',
  'number_of_previous_child_protection_plans',
  'referral_no_further_action'#,
  #'cla_rate_per_10_000_children'
)

formula_la <- as.formula(
  paste(
    "local_authority ~", 
    paste0('treatment_group + ', 
           paste(covariates[-1], collapse = " + "))))

balance_la <- cobalt::bal.tab(
  formula_la, 
  data = data, 
  estimand = "ATE", 
  continuous = 'std',
  s.d.denom = "pooled")

love.plot(balance_la,
          stats = "mean.diffs", 
          abs = TRUE,
          var.order = "unadjusted", 
          drop.missing = FALSE,
          stars = 'std',
          threshold = 0.05) +
  labs(title = paste0("Absolute Mean Differences",
                      " across Local Authorities"))

#2 Compare with baseline 
balance_la_baseline <- cobalt::bal.tab(
  formula_la, 
  data = baseline_data, 
  estimand = "ATE", 
  s.d.denom = "pooled")

love.plot(balance_la_baseline,
          stat = "mean.diffs", 
          abs = TRUE,
          var.order = "unadjusted", 
          stars = 'std',
          threshold = 0.05) +
  labs(title = paste0("Standardized Mean Differences",
                      " by Local Authority at Baseline"))

# Balance checks ----

# Standardized means differences 

#1 Method 1

# Create a table of covariates grouped by treatment status
table_one <- tableone::CreateTableOne(
  vars = covariates, 
  strata = "treatment_group",
  includeNA = TRUE,
  data = data)

smd_table = print(table_one, smd = TRUE)

write.csv(
  smd_table,
  file = paste0(descriptive_path,
                "Balance checks/",
                "tableone_balance_checks.csv"))

# Checks crosstabs for ethnicity, disability, referral no further action

covariates_to_check = c(
  'gender',
  'ethnicity_agg', 
  'disabled_status',
  'referral_no_further_action')

table_check_1 = data %>% 
  select(treatment_group, any_of(covariates_to_check)) %>%
  dplyr::group_by(treatment_group) %>%
  describe(class = 'categorical', 
           group = 'treatment_group') %>%
  arrange(levels)

table_check_2 = data %>% 
  select(local_authority, any_of(covariates_to_check)) %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical', 
           group = 'local_authority') %>%
  arrange(levels)

table_check_3 = data %>% 
  select(local_authority, treatment_group, 
         any_of(covariates_to_check)) %>%
  dplyr::group_by(local_authority) %>%
  describe(class = 'categorical', 
           group = c('local_authority', 
                     'treatment_group')) %>%
  arrange(levels)

balance_checks_tabs = list(
  table_check_1,
  table_check_2,
  table_check_3)

#2 Method 2:

# Create a formula with covariates
formula_trt <- as.formula(
  paste("treatment_group ~",
        paste(covariates, collapse = " + ")))

# Use bal.tab() to calculate SMDs for covariates by treatment
balance_trt <- cobalt::bal.tab(
  formula_trt, 
  data = data, 
  estimand = "ATE", 
  s.d.denom = "pooled")

# Generate love plot for treatment group balance
love.plot(balance_trt,
          stat = "mean.diffs", 
          abs = TRUE,
          stars = 'std',
          var.order = "unadjusted", 
          threshold = 0.05) +
  labs(title = paste0("Absolute Mean Differences",
                      " by Treatment Group"))

# Key take-aways:
#1 Ethnicity 
# Imbalance in White & Asian groups 
# We know Asian groups are less at risk 
# In treatment group > less white (67%), more Asian (12%)
# In control group > more white (76%), less Asian (3%)
# treatment group perhaps less overall risk of CLA
# More missing values in control group (5% vs 3% in trt)
# Missing more likely to be white in treatment, and minority ethnic in control?

data %>% 
  dplyr::group_by(treatment_group, ethnicity_agg) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  print()

# Which cluster is driving this?
data %>% 
  dplyr::group_by(local_authority, ethnicity_agg) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# Rochdale has 21% Asian children in CSC 
# 19% in general population (Rochdale Census)
# Difference between treatment & control entirely driven by Rochdale
# who's the first LA to switch to treatment 

#2 NFA
# More NFAs in treatment group = overall risk of CLA is lower in treatment group
# 181/3511 = 5% of treated are NFAs
# 65/5334 = 1% of control are NFAs 

data %>% 
  dplyr::group_by(treatment_group, referral_no_further_action) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  print()

# Which cluster is driving this?
data %>% 
  dplyr::group_by(local_authority, referral_no_further_action) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# Rochdale 
# 7% of children have NFA, compared with 0, 1, 3% for R, N, W
# Rochdale overall has a population less at risk of becoming CLA 

#3 Disability
# More not disable in control group = overall risk of CLA is lower in control group (?)
# 9% in control versus 5% in treatment 

data %>% 
  dplyr::group_by(treatment_group, disabled_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  print()

# Which cluster is driving this?
data %>% 
  dplyr::group_by(local_authority, disabled_status) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# Norfolk/Warrington  
# 9% and 7% of children have a disability status respectively
# Clusters more in control condition, explains the above 

#4 Age
# Pretty even distribution within 1 year bands
# Age 17: 1% difference between control/treatment;
# 12% in treatment, 13% in control 
# Control slightly more censorship? 

data %>% 
  dplyr::group_by(treatment_group, age_at_cp_start_cat) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# Which cluster is driving this?
data %>% 
  dplyr::group_by(local_authority, age_at_cp_start_cat) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# Norfolk more 17 yos (14% versus 10, 12, 12 for Redcar, R and W)

#5 CPP
# Manier CPPs in control group (at least 1 = 11% against 9%)
# 0 CPPs = 89% in trt, 86% in control 
# Trt slightly less at risk 
data %>% 
  dplyr::group_by(treatment_group, 
                  number_of_previous_child_protection_plans) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# Which cluster is driving this?
data %>% 
  dplyr::group_by(local_authority, 
                  number_of_previous_child_protection_plans) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# Warrington: 95% without previous CPPs (against 85-88% range)
# 4% with at least 1CPP, against 11-12% in other LAs

# Questions: 
# Is it likely that reception of treatment was correlated with ethnicity? 
# E.g., LA with most children from less at risk ethnicity receives treatment first 
# = Rochdale much manier Asians? 
# Is it likely that reception of treatment was correlated to NFAs? 
# (e.g., Rochdale has more NFA and is the first recipient of treatment)
# Is it likely that reception of treatment was correlated to CPPs? 
# (e.g., Warrington has less CPP and is the second recipient of treatment)

# Missingness ----

# View missing data pattern
mice::md.pattern(data, rotate.names = TRUE)

missing_data = dplyr::mutate(
  data,
  is_missing_ethnicity = ifelse(is.na(ethnicity_agg), 1, 0),
  is_missing_gender = ifelse(gender == 'Other', 1, 0))

# 1 Crosstabs
missing_eth_crosstabs = get_missing_crosstabs(
  covariate = 'is_missing_ethnicity',
  data = missing_data)

writexl::write_xlsx(
  missing_eth_crosstabs,
  path = paste0(descriptive_path,
                "Missingness/",
                "ethnicity_crosstabs.xlsx"))

missing_gender_crosstabs = get_missing_crosstabs(
  covariate = 'is_missing_gender',
  data = missing_data)

writexl::write_xlsx(
  missing_gender_crosstabs,
  path = paste0(descriptive_path,
                "Missingness/",
                "gender_crosstabs.xlsx"))

# Almost all the ethnicity missing data is in Norfolk (336/376)
# 'Other' gender also slightly over-represented in Norfolk (24 vs 5/6, none in Rochdale)

# 2 MAR analysis: mixed-level modelling 
mar_model <- glmer(
  as.formula(
    paste0(
      "is_missing_ethnicity ~ ",
      individual_covariates, " + (1 | local_authority) + (1 | wedge)")), 
  family = binomial(link = "logit"), 
  data = missing_data)

View(mar_model$coefficients)

# Sample description ----

# 1 Cohort sizes 
# Number of unique children referred:
# Total, by lA, by wedge, by wedge by LA > 
# By wedge = cluster sizes 
# By wedge by LA = cross sectional cohorts during trial period 

# Total
length(
  unique(
    data$child_id)) # 9,107 children were referred between Oct 2019 and March 2022 

# By LA
data %>% 
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(count = n_distinct(child_id))

# By wedge
data %>% 
  dplyr::group_by(wedge) %>%
  dplyr::summarise(count = n_distinct(child_id))

# By LA by wedge 
data %>% 
  dplyr::group_by(local_authority, wedge) %>%
  dplyr::summarise(count = n_distinct(child_id)) %>%
  View()

# 2 Treatment
# Number unique children who received treatment vs did not (were referred under control condition)
# Total, by LA, by wedge, by wedge by LA

# Total
data %>% 
  dplyr::group_by(treatment_group) %>%
  dplyr::summarise(count = n_distinct(child_id))

# By LA
data %>% 
  dplyr::group_by(local_authority, treatment_group) %>%
  dplyr::summarise(count = n_distinct(child_id))

# By wedge
data %>% 
  dplyr::group_by(wedge, treatment_group) %>%
  dplyr::summarise(count = n_distinct(child_id))

# By LA by wedge 
data %>% 
  dplyr::group_by(local_authority, wedge, treatment_group) %>%
  dplyr::summarise(count = n_distinct(child_id))

# 3 Outcome 
# Number of unique children who went into care vs did not within 18months 
# Total, by LA, by wedge, by wedge by LA

# Total outcome
data %>% 
  dplyr::group_by(cla_status) %>%
  dplyr::summarise(count = n())

# Total CLA
data %>% 
  dplyr::filter(!is.na(date_period_of_care_commenced)) %>%
  nrow()

# Total outcome with NFA 
data %>% 
  dplyr::group_by(cla_status, referral_no_further_action) %>%
  dplyr::summarise(count = n())

# Total outcome with CPP
data %>% 
  dplyr::group_by(cla_status, number_of_previous_child_protection_plans) %>%
  dplyr::summarise(count = n())

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

# Power ----
stepped_wedge_power <- SWSamp::sim.power(
  I = 5, # Number of clusters
  J = 5, # Number of periods
  H = 600, # Number of units randomised at each time point
  K = 3600, # Average size of each cluster
  # design = cross sectional by default
  mu = 0.07, # Baseline outcome value
  b.trt = 0.0185, # MDES / treatment effect
  rho = 0.00268, # ICC
  family = 'binomial',
  sig.level=0.05 # alpha
)

print(stepped_wedge_power$power)