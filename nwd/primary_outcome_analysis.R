# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Paths  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'QA/processing/linked_data/')

output_path = paste0(sharepoint_path, 'QA/outputs/')

descriptive_path = paste0( sharepoint_path,
                           "QA/outputs/descriptives/",
                           "Primary cohort description/")

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

ons_mye_2023 = readxl::read_excel(
  paste0(
    output_path,
    "ons_mye_mid_apr_2023.xlsx"))

# Clean MYE 
ons_mye_2023 = ons_mye_2023 %>%
  dplyr::mutate(local_authority = ifelse(
    laname23 %in% c(
      'Breckland', 'Broadland', 'Great Yarmouth',
      "King's Lynn and West Norfolk", 'North Norfolk',
      'Norwich','South Norfolk'), 'norfolk', tolower(laname23))) %>%
  dplyr::mutate(local_authority = ifelse(
    local_authority == 'redcar and cleveland', 
    'redcar', local_authority)) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(across(.cols = any_of(contains('population')),
                   .fns = sum)) 

data = left_join(data,
                 ons_mye_2023,
                 by = 'local_authority')

data = data %>% relocate(
  population_2019, population_2020, population_2021, population_2022, 
  .after = total_cin_cpp)

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
  age_at_referral_cat = as.character(age_at_referral),
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
  
  age_at_referral_cat = relevel(
    factor(age_at_referral_cat), ref = '12'),
  
  gender = relevel(
    factor(gender), ref = 'Male'),
  
  ethnicity_agg = relevel(
    factor(ethnicity_agg), ref = 'White British or Irish'),
  
  disabled_status = relevel(
    factor(disabled_status), ref = 'Not disabled'),
  
  unaccompanied_asylum_seeker = relevel(
    factor(unaccompanied_asylum_seeker), ref = 'Not UASC'),
  
  referral_no_further_action = relevel(
    factor(referral_no_further_action), ref = 'Further action'),
  
  number_of_previous_child_protection_plans = relevel(
    number_of_previous_child_protection_plans,
    ref = '0'))

# sort column order
colnames = colnames(data)

data = data %>%
  relocate(readiness, .after = local_authority) %>%
  relocate(age_at_referral_cat, .after = age_at_referral) %>%
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
  dplyr::mutate(
  la_treatment_start  = case_when(
    local_authority == 'rochdale' ~ as.Date('2020-04-01'),
    local_authority == 'warrington' ~ as.Date('2021-04-01'),
    local_authority == 'norfolk' ~ as.Date('2021-06-01'),
    local_authority == 'redcar' ~ as.Date('2021-09-01'))) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::mutate(
  treatment_group = ifelse(
    referral_date >= la_treatment_start, 1, 0)) %>%
  ungroup()

###3. Outcome ----

# 1 if has become looked after within 18 months of first referral 
# 0 otherwise 

data = data %>%
  dplyr::mutate(
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
  dplyr::mutate(
    cross_contamination  = ifelse(
      referral_date >= (la_treatment_start - 45) & 
        referral_date < la_treatment_start,
      1, 0))

###5. Rate of CIN/CPP per 10,000 children ----
data = data %>% mutate(
  cin_cpp_rate_per_10_000_children = case_when(
    year(referral_date) == 2019 ~ total_cin_cpp / population_2019 * 10000,
    year(referral_date) == 2020 ~ total_cin_cpp / population_2020 * 10000,
    year(referral_date) == 2021 ~ total_cin_cpp / population_2021 * 10000,
    year(referral_date) == 2022 ~ total_cin_cpp / population_2022 * 10000),
  cla_cin_cpp_rate_per_10_000_children = case_when(
    year(referral_date) == 2019 ~ (total_cla + total_cin_cpp) / population_2019 * 10000,
    year(referral_date) == 2020 ~ (total_cla + total_cin_cpp) / population_2020 * 10000,
    year(referral_date) == 2021 ~ (total_cla + total_cin_cpp) / population_2021 * 10000,
    year(referral_date) == 2022 ~ (total_cla + total_cin_cpp) / population_2022 * 10000)) %>%
  relocate(cin_cpp_rate_per_10_000_children,
           cla_cin_cpp_rate_per_10_000_children,
           .after = cla_rate_per_10_000_children)

monthly_data = data %>% 
  dplyr::group_by(local_authority) %>%
  dplyr::distinct(month, .keep_all = TRUE) %>%
  ungroup() %>%
  dplyr::group_by(month) %>% 
  dplyr::summarise(
    total_cin_cpp = sum(total_cin_cpp),
    total_cla = sum(total_cla),
    total_pop_2019 = sum(population_2019),
    total_pop_2020 = sum(population_2020),
    total_pop_2021 = sum(population_2021),
    total_pop_2022 = sum(population_2022),
  ) %>%
  dplyr::mutate(
    overvall_rate = case_when(
      year(month) == 2019 ~ (total_cin_cpp + total_cla) / total_pop_2019 * 10000,
      year(month) == 2020 ~ (total_cin_cpp + total_cla) / total_pop_2020 * 10000,
      year(month) == 2021 ~ (total_cin_cpp + total_cla) / total_pop_2021 * 10000,
      year(month) == 2022 ~ (total_cin_cpp + total_cla) / total_pop_2022 * 10000)
  )

data = select(
  data,
  -contains('population'),
  -contains('number_of_c'),
  -contains('number_of_o'),
  -contains('number_of_n'),
  -month_return,
  -free_school_meal_eligibility_ever_fsm,
  -pupil_premium_eligibility_for_reception_year_1_and_year_2)


###6. Filters ----

data = data%>%
  filter(
  is.na(care_period_number) |
    care_period_number == 1) %>% # keep first date of care only
  filter(!is.na(cla_status)) # make sure date of care >= referral date

# Save analyical dataset
writexl::write_xlsx(
  data,
  path = paste0(
    output_path,
    "primary_analysis_analytical_dataset.xlsx"))

###5. Descriptives ----

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

# Reflection ----
# Threat to randomisation:
# 'High readiness' = correlated with less chances of becoming CLA? 
# Very possible in this case

baseline_data = filter(
  data, wedge == 'baseline')

# Randomisation checks ----
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
  'age_at_referral_cat',
  #'age_at_referral',
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
  dplyr::group_by(treatment_group, age_at_referral_cat) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(freq = round(count/sum(count),2)) %>%
  View()

# Which cluster is driving this?
data %>% 
  dplyr::group_by(local_authority, age_at_referral_cat) %>% 
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

# Imputation ----

## Workplan ---- 
#0 Assess MNAR
#1 Multiple imputation 
#2 Sensitivity checks: do the results change drastically including/excluding auxiliary variables 
#3 Multilevel imputation 

##0 MNAR with Cramer's V & Chi_2 test ----

covariates = c(
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
      covariates[-1], function(cov){
        
        check_mnar(data = la_data,
                   missing_covariate = 'is_missing_ethnicity',
                   auxiliary = cov) }) 
    table %>% 
      dplyr::mutate(cluster = c) %>%
      relocate(cluster) })

View(mnar_table_la)

# LA missingness checks: crosstabs with possible auxiliary vars
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

##1 Multiple imputation ---- 

# Derive imputation dataset: select only relevant features
# = column with missing data + auxiliary variables
imputation_data = data %>%
  select(
    child_id,
    ethnicity_agg, 
    any_of(covariates)) #%>%
  #mutate(is_norfolk = ifelse(
  #  local_authority == 'norfolk', 1,0)) #%>%
  #select(-referral_no_further_action,
  #       -local_authority) 

# View missing data pattern
mice::md.pattern(imputation_data)

# Auxiliary variables

# To ensure balance = treatment group
# Related to ethnicity = age, gender
# Related to missingness = cluster, wedges
# Related to missingness (in Norfolk) = UASC, disability, nb previous cpp
# Sensitivity checks = with/without NFA

# Multilevel imputation 

# Set up the imputation model, specifying auxiliary variables
# Set 'ethnicity' as the target variable to impute


# Remove variables with only one unique value (including ethnicity if applicable)
imputed_data <- mice::mice(
  data,
  m = 5, 
  method = 'polyreg',
  seed = 123, 
  predictorMatrix = make.predictorMatrix(
    imputation_data))
 
# Check convergence 
plot(imputed_data)

# Visualize observed vs imputed values for ethnicity
imp_plot = propplot(imputed_data, 
                    label_size = 10,
                    show_prop = TRUE,
                    prop_size = 2)

imp_plot_trt = propplot(
  imputation_model,
  ethnicity_agg ~ treatment_group,
  label_size = 7) 

imp_plot_la = propplot(
  imputed_data, 
  ethnicity_agg ~ local_authority,
  label_size = 5) 

# Extract completed imputed datasets and check consistency
complete_data_1 <- complete(imputation_model, 1)  # First imputed dataset
#complete_data_2 <- complete(imputation_model, 2)  # Second imputed dataset

# Check summaries
#summary(complete_data_1$ethnicity_agg)
#summary(complete_data_2$ethnicity_agg)

# Sensitivity check: Increase the number of imputations to 10
sensitivity_model <- mice(
  imputation_data, 
  m = 10,           # Number of multiple imputations
  method = c('ethnicity_agg' = 'polyreg'),  # Predictive mean matching (appropriate for mixed data)
  predictorMatrix = quickpred(imputation_data, exclude = "child_id"),
  maxit = 10,      # Maximum iterations for convergence
  seed = 123)

# Compare summaries of the two imputation models
summary(imputation_model)
summary(sensitivity_model)

# Performance checks
#1 Convergence Diagnostics
#2 Assessing Imputed Values - not doing
#3 Comparing Distributions of Imputed and Observed Data
#4 Check for Consistency Across Imputed Datasets - not doing 
#5 Model Fit on Imputed Data
#6 Pooling Results
#7 Sensitivity Analyses

# Add imputed data to dataset

# Get the final imputed dataset
completed_data <- complete(
  imputation_model, action = "long", include = TRUE)

# Merge the imputed ethnicity back into the original dataset using Cluster and CHILD_ID as keys
imputed_data <- data %>%
  left_join(
    completed_data %>%
      filter(.imp == 1) %>%
      select(local_authority, child_id, ethnicity), 
    by = c("child_id" = ".id",
           "local_authority" = "local_authority"))

# Cohort description ----

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

# Fit model ----

# 1 load data 
data = readxl::read_excel(
  paste0(
    output_path, 'primary_analysis_analytical_dataset.xlsx'))

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

# Prep formula 
demographics = paste('age_at_referral_cat',
                     'gender',
                     'ethnicity_agg',
                     'disabled_status',
                     'unaccompanied_asylum_seeker',
                     'number_of_previous_child_protection_plans',
                     'referral_no_further_action',
                     sep = " + ")

## Model 0 ----

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

model_0 = lme4::glmer(
  as.formula(
    paste0(
      "cla_status ~ treatment_group + wedge + ",
      demographics,  " + (1 | local_authority)")), 
  data = data,
  family = binomial)

summary(model_0)

# Check the convergence information
# https://rdrr.io/cran/lme4/man/convergence.html
#model_0@optinfo$conv$opt
#isSingular(model_0)

# Tidy results
tidy_m0 = broom.mixed::tidy(
  model_0, conf.int=TRUE,exponentiate=TRUE,effects="fixed")

# Standard errors:
# https://economics.mit.edu/sites/default/files/2022-09/cluster-6.pdf

# Get robust standard errors using the cluster sandwich estimator
#robust_se <- clubSandwich::coef_test(
#  model_1, vcov = "CR2", cluster = data$local_authority)

# vcov specifies the bias-reduced "CR2" variance estimator,
# which is recommended for small-sample clustered designs.

# Print the robust results (including adjusted standard errors)
#print(robust_se)

## Model 1 ----
# Model 1: PROTOCOL MODEL (time varying LA indicators, random & fixed effects)
# = Model 1 + CLA rate per 10,000 children
re = " + (1 | local_authority)"

model_1 = lme4::glmer(
  as.formula(
    paste0(
      "cla_status ~ treatment_group + ", # FE for trt + time effects
      demographics, # adjust for person level demographics
      #" + cla_cin_cpp_rate_per_10_000_children",
      #" + cla_rate_per_10_000_children",
      #" + cla_cin_cpp_rate_per_10_000_children",
      " + splines::ns(cla_cin_cpp_rate_per_10_000_children, df = 5)", # adjust for time-varying cluster level indicators
      re)), # RE intercept 4 clusters
  data = data[data$gender != 'Other',],
  family = binomial)

summary(model_1)

# Model checks 

#1 Check VIF
#vif(model_1) # VIF ok 

#2 Check optimisers:
aa <- allFit(model_1)
ss <- summary(aa)
ss$msgs[!sapply(ss$msgs,is.null)]
        
# Tidy results
tidy_m1 = broom.mixed::tidy(
  model_1, conf.int=TRUE, 
  exponentiate=TRUE,
  effects="fixed")

tidy_m1 %>%
  dplyr::mutate(across(is.numeric, round,2)) %>%
  View()

# Sensitivity analyses ----

# 2 Cluster-time interactiomn

# 1 without Rochdale
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

# 2 Without 17s 
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

# 3 Without NFAs 
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