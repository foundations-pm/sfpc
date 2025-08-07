# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set up  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 
                   'QA/processing/processed_data/')

output_path = paste0(sharepoint_path, 'QA/processing/linked_data/')

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions 
{ source(paste0(wd, "functions.R"))}

# Load data ----

# DR1: LA-level characteristics
# DR2: baseline, referrals 
# DR3: outcome (whether in care or not)

dr1_path = paste0(data_path, "DR1/DR1_processed.xlsx")
dr1_data = readxl::read_excel(dr1_path)

#dr2_path = paste0(data_path, "DR2/DR2_processed_referrals.xlsx")
#dr2_cpp = readxl::read_excel(dr2_path)

# Using open CPP as cohort instead of referred within trial period
dr2_path = paste0(data_path, "DR2/DR2_processed_cpp.xlsx")
dr2_cpp = readxl::read_excel(dr2_path)

dr3_path = paste0(data_path, "DR3/DR3_processed_cla.xlsx")
dr3_cla = readxl::read_excel(dr3_path)

# DATA LINKAGE ----

## 1. Derive eligible population from DR2 ----

# Eligibility criteria:
# 1) Referred to CSC during trial period: 01/10/2019 to 31/03/2022
# 2) aged [12;17] during first referral
### TO THINK ABOUT: censoring/dosage 
# Those who turn 18 during trial period: can no longer be at risk of CLA

# Check missingness
nrow(dr2_cpp)
sapply(dr2_cpp, function(x) sum(is.na(x)))

# How many children had an open CPP during trial dates?
dr2_cpp %>% dplyr::mutate(
  child_protection_plan_start_date = as.Date(child_protection_plan_start_date)) %>%
  dplyr::filter(child_protection_plan_start_date >=  '2019-10-01' & child_protection_plan_start_date <= '2022-03-31') %>%
  nrow()

nrow(dr2_cpp)
# 5,197 - all children in DR2 CPP are eligible

### Step 0: missing/incoherent DOB & referral dates ----
# or how many records are DOB > referral

# missing
sum(
  is.na(
    dr2_cpp[[
      'year_and_month_of_birth_of_the_child']])) # 17

# incoherent or unborn children
dr2_cpp %>% dplyr::filter(
  year_and_month_of_birth_of_the_child >= child_protection_plan_start_date) %>%
  nrow() # 593

# 5.53% missing total; 
# 244 ppl (0.5%): missing amongst the possibly eligible population

### Step 1: Age at referral ----
#### TO THINK ABOUT: impact of deriving age at the start of the month 
date_cols = dr2_cpp %>% 
  select(
    year_and_month_of_birth_of_the_child,
    contains('date')) %>%
  colnames()

dr2_cpp = dr2_cpp %>%
  dplyr::filter( 
    !is.na(year_and_month_of_birth_of_the_child), # remove missing age values
    year_and_month_of_birth_of_the_child < child_protection_plan_start_date) %>% # remove incoherent values or unborn children
  dplyr::mutate( # standardise date format
    across(.cols = all_of(date_cols),
           .fns = as.Date)) %>%
  dplyr::mutate(
    age_at_cp_start = floor(
      eeptools::age_calc(
        dob = as.Date(
          year_and_month_of_birth_of_the_child),
        enddate = as.Date(
          child_protection_plan_start_date),
        units = "years"))
  ) 

### Step 2: Date turned 18 ----
#### TO THINK ABOUT: impact of deriving age at the start of the month
dr2_cpp = dr2_cpp %>%
  dplyr::mutate(
    date_turned_18 = as.Date( # add 18 years to DOB
      year_and_month_of_birth_of_the_child) %m+% years(18))  

### Step 3: Rank CP plans ----
dr2_cpp = dr2_cpp %>%
  dplyr::group_by( # group by child
    child_id) %>%
  dplyr::arrange( # sort by referrals by date, from oldest to latest
    child_protection_plan_start_date) %>% 
  dplyr::mutate( 
    cp_number = dplyr::dense_rank(child_protection_plan_start_date)) %>% # assign a rank
  dplyr::ungroup()

# A tibble: 3 × 2
#cp_number `n()`
#    1  4478
#    2   107
#    3     2

### Step 4: Define 18 months post referral  ----
#### TO CHECK WITH OANA WHEN MONTH CALC SHOULD START 
# = month referred == month 0 or 1? 
dr2_cpp = dr2_cpp %>%
  dplyr::mutate( # eosp = end of study period
    eosp = as.Date( # add 18 months to DOB
      child_protection_plan_start_date) %m+% months(18))

### Step 5: Define time at risk t ----
# Exposure time:
# Let End of study period (EOSP) be referral date + 18 months for each child
# (1) for those who did not turn 18 before EOSP:
# t = EOSP - child_protection_plan_start_date
# (2) for those who turned 18 before EOSP:
# t = date_turned_18 - child_protection_plan_start_date
dr2_cpp = dr2_cpp %>%
  dplyr::mutate( # create a dummy for whether child turned 18 within study period
    is_censored = ifelse(
      date_turned_18 <= eosp, 1,0),
    t = case_when(
      is_censored == 1 ~ as.numeric(as.Date(date_turned_18) - as.Date(child_protection_plan_start_date)),
      is_censored == 0 ~ as.numeric(as.Date(eosp) - as.Date(child_protection_plan_start_date))))

# A tibble: 2 × 2
#is_censored `n()`
#  1           0  4475
#  2           1   112

### Step 6: Define eligible population ----

# Define trial period
trial_period = seq(from = as.Date('2019-10-01'),
                   to =  as.Date('2022-03-31'), 
                   by = "day")

# Eligible population:
# 1 - referrals within trial period
# 2 - age 12 to 17 at referral 
# (3 - first referral only (will filter as a last step below))
dr2_cpp = dr2_cpp %>%
  dplyr::mutate(
    eligibility = ifelse(
      child_protection_plan_start_date %in% trial_period & # referral date is within trial period 
        age_at_cp_start %in% c(12:17), # age when referred is between 12 and 17
      1, 0))

# A tibble: 2 × 2
#eligibility `n()`
#<dbl> <int>
#  1           0  3457
#  2           1  1130 (includes children with more than 1 open CP plans during trial)

# checks 
dr2_cpp %>% dplyr::filter(
  child_protection_plan_start_date %in% trial_period & 
    age_at_cp_start %in% c(12:17) &
    cp_number == 1) %>% 
  nrow()

# [1] 1097 unique children 

# Arrange / clean up dataset
dr2_cpp = dr2_cpp %>% 
  dplyr::relocate(
    local_authority, month_return,
    referral_id_or_case_id, child_id,
    child_protection_plan_start_date, cp_number, eosp,
    year_and_month_of_birth_of_the_child, age_at_cp_start, date_turned_18,
    is_censored, t, eligibility) %>%
  dplyr::arrange(
    local_authority, child_id, desc(cp_number))

# Derive eligible population
# Eligible population ranks referrals using a unique number
# If 2 first referrals have the same dates; it rank the first row as referral 1
# the second row as referral 2
# This is so that we can apply adequate filters in the dataset

# WARNING: 
# Eligible population contains everyone in DR2; 
# The analytical population (trial sample) is derived by using filters 
# On referral = 1; referral being in the study period & age being between 12-17
eligible_population = dr2_cpp %>% 
  dplyr::group_by(child_id) %>%
  dplyr::arrange(child_protection_plan_start_date) %>% 
  dplyr::mutate(cp_number = row_number(child_protection_plan_start_date)) %>% 
  dplyr::ungroup() 

# Check missingness
nrow(dr2_cpp)
sapply(dr2_cpp, function(x) sum(is.na(x)))

nrow(eligible_population)
sapply(eligible_population, function(x) sum(is.na(x)))

### Step 7: Checks ----

#### Check 1: QA duplicated 1st referrals with dense_rank() ----

# total nb of referrals in DR2: 
nrow(dr2_cpp) # 4587...

# Check nb of duplicate 1st referrals 
dr2_cpp %>% 
  dplyr::filter(cp_number == 1) %>% 
  nrow() # 4478 first CP plans open

length(
  unique(
    dr2_cpp$child_id)) # 4478 unique children

#### Check 2: Investigate dense_rank() duplicates ----
# check whether we can drop any rows and use the row_number() method
duplicate_open_cps = dr2_cpp %>% 
  dplyr::group_by(child_id, referral_id_or_case_id) %>% 
  dplyr::mutate(duplicate_open_cps = any(duplicated(child_protection_plan_start_date))) %>%
  ungroup() %>%
  dplyr::filter(duplicate_open_cps == TRUE,
         cp_number == 1)  # none

#### Check 3: QA eligibility filter using row_number() ----

# Re-define row ranks using the row_number method
# > gives unique rank even to duplicates
# Use this dataset to select cp_number == 1 to describe eligible population
qa_eligibility = eligible_population %>%
  dplyr::mutate(
    is_open_cp_in_trial = ifelse(child_protection_plan_start_date %in% trial_period, 1,0),
    is_of_age = ifelse(age_at_cp_start %in% c(12:17), 1, 0))

qa_eligibility %>% 
  dplyr::filter(is_open_cp_in_trial == 1 &
           is_of_age == 1) %>%
  nrow() # 1130 open cps are eligible

qa_eligibility %>% 
  dplyr::filter(is_open_cp_in_trial == 1 &
           is_of_age == 1) %>%
  distinct(child_id) %>%
  nrow() # 1103 unique children are eligible

qa_eligibility %>% 
  dplyr::filter(cp_number == 1 & 
           is_open_cp_in_trial == 1 &
           is_of_age == 1) %>%
  distinct(child_id) %>%
  nrow() # 1097 unique children are included in the trial analysis

### Step 8: Sample frame DESCRIPTIVES ----

# 1) Total children eligible out of sample frame 
eligible_population %>%  
  dplyr::filter(cp_number == 1) %>% 
  dplyr::group_by(eligibility) %>% 
  dplyr::summarise(n())

# 1097 children are eligible 

# by LA:
eligible_population %>% 
  dplyr::filter(cp_number == 1) %>% 
  dplyr::group_by(local_authority, eligibility) %>% 
  dplyr::summarise(n())

# Norfolk: 404
# Redcar: 176
# Rochdale: 232
# Warrington: 95

# 2) Mean nb referrals for eligible children 

eligible_population %>% 
  dplyr::filter(eligibility == 1) %>% 
  dplyr::summarise(mean = mean(cp_number),
                   median = quantile(cp_number, .5),
                   sd = sd(cp_number),
                   min = min(cp_number),
                   max = max(cp_number))

eligible_population %>% 
  dplyr::filter(eligibility  == 1) %>% 
  dplyr::group_by(cp_number) %>%
  dplyr::summarise(count = n())

# 3) Total nb of children turning 18 during study period 

eligible_population %>% 
  dplyr::filter(cp_number == 1 & eligibility == 1) %>%
  dplyr::group_by(is_censored) %>%
  dplyr::summarise(count = n())

# 108 children turned 18 during the study period 
# 9.8% of sample

# excluding Leicester:
eligible_population %>% 
  dplyr::filter(local_authority != 'leicester',
         cp_number == 1 & eligibility == 1) %>%
  dplyr::group_by(is_censored) %>%
  dplyr::summarise(count = n()) # 92 = 10.1%

# 4) Mean, sd, min, max time at risk 

# Amongst those turning 18, check distribution of time at risk
eligible_population %>% 
  dplyr::filter(local_authority != 'leicester',
         cp_number == 1 & eligibility == 1,
         is_censored == 1) %>%
  dplyr::summarise(mean = mean(t),
                   median = quantile(t, .5),
                   sd = sd(t),
                   min = min(t),
                   max = max(t)) 

# time at risk during study period = 518 days
# mean t = 256 (half the study period)

# Amongst those who did not turn 18, check distribution of time at risk
eligible_population %>% 
  dplyr::filter(local_authority != 'leicester',
         cp_number == 1 & eligibility == 1,
         is_censored == 0) %>%
  dplyr::summarise(mean = mean(t),
                   median = quantile(t, .5),
                   sd = sd(t),
                   min = min(t),
                   max = max(t)) # looks good 

# 4) NAs amongst eligible population
eligible_population %>% 
  dplyr::filter(local_authority != 'leicester',
         cp_number == 1 & eligibility == 1,
         is_censored == 0) %>%
  sapply(., function(x) sum(is.na(x)))

## 2. Link data: DR2+DR3 ----

### Step 1: Derive analytical population ----
analytical_dataset_dr2 = eligible_population %>% 
  dplyr::filter(
    local_authority != 'leicester', # drop Leicester
    cp_number == 1 & eligibility == 1) 

# eligibility = referral date within study period 
# + age at referral is 12 to 17

### Step 2: Derive analytical DR3 ----

# Check number of unique children in dr3
dr3_cla %>%
  dplyr::filter(!is.na(date_period_of_care_commenced)) %>%
  nrow() # 2,274 unique children 

analytical_dr3_cla = dr3_cla %>%
  dplyr::filter(!is.na(date_period_of_care_commenced)) 

nrow(analytical_dr3_cla) # 2,274 children
length(unique(analytical_dr3_cla$child_id)) # 2,116 children 
length(unique(analytical_dr3_cla$referral_id_or_case_id)) # 2,136 children 

# 158 children with multiple period of care? 
# Some children have multiple period of care under the same referral ID; 
# Some have multiple period of care under different referral IDs
# TO CHECK: multiple period of care as outcome 

duplicated_dr3_rows = analytical_dr3_cla %>%
  dplyr::group_by(child_id) %>%
  dplyr::mutate(duplicated_child_ids = any(
    duplicated(child_id))) %>%
  ungroup() %>%
  dplyr::filter(duplicated_child_ids == TRUE)

duplicated_dr3_rows %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(n())

# Number of children by LA - including duplicate rows 
total_rows_dr3_by_la = analytical_dr3_cla %>% 
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(total_rows_dr3 = n()) 
# norfolk           937
# redcar            363
# rochdale          644
# warrington        330

# Number of unique children 
analytical_dr3_cla %>% 
  distinct(child_id, .keep_all = TRUE) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(n()) 
# norfolk           841
# redcar            344
# rochdale          627
# warrington        304

# Rank care episode by Child ID
analytical_dr3_cla = analytical_dr3_cla %>% 
  dplyr::group_by(child_id) %>%
  dplyr::mutate(care_period_number = row_number(
    date_period_of_care_commenced))

analytical_dr3_cla %>%
  dplyr::group_by(care_period_number) %>%
  dplyr::summarise(n())

### Step 3: Link DR2 to DR3 ----
# DR2 to DR3 via LA, Child ID only

analytical_dataset_dr2 = analytical_dataset_dr2 %>%
  dplyr::mutate(dr2_id = 'dr2')

analytical_dr3_cla = analytical_dr3_cla %>%
  dplyr::mutate(dr3_id = 'dr3')

# Left join: keep all baseline records, join outcome records 
linked_data = dplyr::left_join(
  analytical_dataset_dr2[,-c(2,3)], # drop cols 'month return' and 'referral_id_or_case_id'
  analytical_dr3_cla[, -c(2,4)], # drop col 'month_return' and 'referral_id_or_case_id'
  by = c("local_authority",
         "child_id"))

unique_dr3 = analytical_dr3_cla %>%
  distinct(child_id, .keep_all = TRUE)

raw_linked_data = dplyr::left_join(
  dr2_cpp[,-c(2,3)], # drop cols 'month return' and 'referral_id_or_case_id'
  unique_dr3[, -c(2,4)], # drop col 'month_return' and 'referral_id_or_case_id'
  by = c("local_authority",
         "child_id"))

### Step 4: Linkage rate DR3 to DR2 ----
# Linkage rate: how many records in DR3 could be linked to DR2?

(linked_data %>% dplyr::filter(!is.na(dr3_id)) %>% nrow()) / nrow(analytical_dr3_cla) *100
# 8.9% of records in DR3 (204/2,274) could link to DR2 

(raw_linked_data %>% dplyr::filter(!is.na(dr3_id)) %>% nrow()) / nrow(analytical_dr3_cla) *100
# 33.06% linkage rate between children in DR3 could link to all records in DR2

linkage_rate_by_LA_dr3_to_dr2 = linked_data %>% 
  dplyr::filter(!is.na(dr3_id)) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(total_rows_linked_dr3_to_dr2 = n()) 
# norfolk           390
# redcar            103
# rochdale          110
# warrington        128

linkage_rate_by_LA_dr3_to_dr2 = merge(
  total_rows_dr3_by_la,
  linkage_rate_by_LA_dr3_to_dr2,
  by = 'local_authority')

linkage_rate_by_LA_dr3_to_dr2 %>%
  dplyr::mutate(linkage_rate = (
    total_rows_linked_dr3_to_dr2/total_rows_dr3 * 100)) %>%
  print()

### Step 5: Checks ----

#### Check 1 Proportion of children who went into care ----
# In DR3 records with a match in DR2

# Nb unique child in dr2 analytical dataset
# Analytical DR2 should only contain unique children 
# 1 row = 1 child
length(unique(analytical_dataset_dr2$child_id)) # 907 children
length(unique(linked_data$child_id)) # 907 children
nrow(linked_data) # 949 children - this is because some children will have more than 1 period of care commenced
# 42 children with duplicated matches 
# Should keep rank care ep and keep episode == 1

# By LA
analytical_dataset_dr2 %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(n())
# norfolk           404
# redcar            176
# rochdale          232
# warrington         95

linked_data %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(n()) 

# Nb children (including duplicates) in DR3 records that matched DR2
linked_data %>%
  dplyr::filter(!is.na(dr3_id)) %>%
  nrow() # 204 children 

# Nb unique child in dr3 records that matched DR2
# in linked dataset; dplyr::filter for !is.na(dr3_id)
total_care_children = linked_data %>%
  dplyr::filter(!is.na(dr3_id)) %>%
  distinct(child_id) %>%
  nrow() # 162 children 

# 162 children with multiple episode of care 
# matched the eligible baseline population 
# Broken down by LA:
into_care = linked_data %>%
  dplyr::filter(!is.na(dr3_id)) %>%
  distinct(child_id, .keep_all = TRUE) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(in_care = n()) 

# Same method below
#into_care = linked_data %>%
#  dplyr::filter(care_period_number == 1) %>%
#  dplyr::group_by(local_authority) %>%
#  dplyr::summarise(in_care = n()) 

# Proportion of children who went into care
# At least once
# From the eligible population in DR2
eligible_population = linked_data %>%
  distinct(child_id, .keep_all = TRUE) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(total_eligible = n()) 

care_proportions_by_las = merge(
  eligible_population,
  into_care,
  by = 'local_authority')

care_proportions_by_las %>%
  dplyr::mutate(care_prop = in_care / total_eligible * 100)

#### Check 2 unmatched records  ----
# Records in DR3 that do not match DR2
unmatched_raw_records = anti_join( # returns all rows from x (DR3) without a match in y (DR2)
  unique_dr3[, -c(2,4)], 
  dr2_cpp[,-c(2,3)], 
  by = c("local_authority",
         "child_id"))

unmatched_raw_records_by_la = unmatched_raw_records %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(unmatched = n())

total_raw_dr2_by_la = dr2_cpp %>%
  dplyr::filter(local_authority != 'leicester') %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(total = n())

proportion_raw_unlinked = merge(
  unmatched_raw_records_by_la,
  total_raw_dr2_by_la,
  by = 'local_authority')

proportion_raw_unlinked %>%
  dplyr::mutate(percent_unmatched = unmatched / total * 100) %>%
  print()

unmatched_records = anti_join( # returns all rows from x (DR3) without a match in y (DR2)
  analytical_dr3_cla[, -c(2,4)], 
  analytical_dataset_dr2[,-c(2,3)], 
  by = c("local_authority",
         "child_id"))

unmatched_records %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(n())

# Save unmatched IDs
writexl::write_xlsx(
  unmatched_raw_records, 
  path = paste0(output_path,
                "/sensitivity_cohort/unmatched_raw_records_dr3_to_dr2.xlsx"))

writexl::write_xlsx(
  unmatched_records, 
  path = paste0(output_path,
                "/sensitivity_cohort/unmatched_records_dr3_to_dr2.xlsx"))

## 3. Link data: DR2/3 to DR1 via LA and months ----

dr1_data = dr1_data %>% 
  dplyr::filter(local_authority != 'leicester')

## Save time-unvarying dataset ----
linked_data = select(linked_data,
                     -dr2_id, -dr3_id)
writexl::write_xlsx(
  linked_data, 
  path = paste0(output_path,
                "/sensitivity_cohort/time_unvarying_analytical_dataset.xlsx"))

### TIME DEPENDENT LA INDICATORS:
### Strategy 1: match LA indicator value at the month of recruitment (i.e., referral)

### Step 1: Add common month month var for LA return and referral date 
linked_data = linked_data %>%
  dplyr::mutate(month_id = format(
    as.Date(child_protection_plan_start_date), '%Y-%m')) %>%
  relocate(month_id)

dr1_data = dr1_data %>%
  dplyr::mutate(month_id = format(
    as.Date(month), '%Y-%m')) %>%
  relocate(month_id)

### Step 2 : link via month ID and LA
linked_data_time_dep = left_join(
  linked_data, dr1_data, by = c(
    'local_authority', 'month_id'))

## Save time-dependent, person-level dataset ----
#linked_data_time_dep = select(linked_data_time_dep,
#                              -dr2_id, -dr3_id)
writexl::write_xlsx(
  linked_data_time_dep, 
  path = paste0(output_path,
                "/sensitivity_cohort/time_dependent_person_level_analytical_dataset_V1.xlsx"))

### Strategy 2: time dependent dataset
# Duplicated rows for participants within each LA monthly returns

### Step 1: link dr2/3 to dr1 > dr1 is the most granular, timewise 
# linkage should be by LA
linked_data_time_dep_la_level = left_join(
  dr1_data, linked_data, by = c('local_authority'), 
  keep = TRUE)

linked_data_time_dep_la_level = linked_data_time_dep_la_level %>%
  dplyr::select(- local_authority.y) %>%
  dplyr::rename('local_authority' = 'local_authority.x') %>%
  dplyr::arrange(child_id, local_authority, month)

check = linked_data_time_dep_la_level %>% 
  dplyr::group_by(local_authority, child_id) %>% 
  dplyr::summarise(count = n_distinct(month))

## Save time-dependent, nested dataset ----
#linked_data_time_dep_la_level = select(
#  linked_data_time_dep_la_level,
#  -dr2_id, -dr3_id)

writexl::write_xlsx(
  linked_data_time_dep_la_level, 
  path = paste0(
    output_path,
    "/sensitivity_cohort/time_dependent_la_level_analytical_dataset_V1.xlsx"))

# Clean final data 
# colnames(final_data)
