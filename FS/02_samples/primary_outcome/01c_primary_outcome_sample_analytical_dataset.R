#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## ANALYSIS: PRIMARY OUTCOME ANALYTICAL DATASET ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

## Set-up  ----
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/samples/')
output_path = paste0(sharepoint_path, '/Datasets/analytical_datasets')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

## Load data --------------------------------------------------------------------
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "primary_outcome_sample_raw.Rds")) 
    
### Derive analytical variables ---------------------------------------------------

#### 1. Trial periods ----

# Trial period: 1st March 2020 to 24th November 2022
# 2020-03-01 to 2022-11-24

# Period 0: Baseline; 2020-03-01 to 2020-08-31
# Period 1: Walsall in treatment; 2020-09-01 
# Period 2: Lancashire in treatment; 2021-02-01 
# Period 3: Telford & Wr in treatment; 2021-06-28 
# Period 4: Wandsworth in treatment; 2022-01-24
# Period 5: Swindon in treatment; 2022-05-24

baseline = seq(from = as.Date('2020-03-01'),
               to =  as.Date('2020-08-31'), 
               by = "day")
wedge_1 = seq(from = as.Date('2020-09-01'),
              to =  as.Date('2021-01-31'), 
              by = "day")
wedge_2 = seq(from = as.Date('2021-02-01'),
              to =  as.Date('2021-06-27'), 
              by = "day")
wedge_3 = seq(from = as.Date('2021-06-28'),
              to =  as.Date('2022-01-23'), 
              by = "day")
wedge_4 = seq(from = as.Date('2022-01-24'),
              to =  as.Date('2022-05-23'), 
              by = "day")
wedge_5 = seq(from = as.Date('2022-05-24'),
              to =  as.Date('2022-11-24'), 
              by = "day")

data = dplyr::mutate(
  data, 
  wedge  = case_when(
    dr2_referral_date %in% baseline ~ 'baseline',
    dr2_referral_date %in% wedge_1 ~ 'wedge_1',
    dr2_referral_date %in% wedge_2 ~ 'wedge_2',
    dr2_referral_date %in% wedge_3 ~ 'wedge_3',
    dr2_referral_date %in% wedge_4 ~ 'wedge_4',
    dr2_referral_date %in% wedge_5 ~ 'wedge_5'
  ))

#### 2. End of study period (eosp) ----
# For primary outcome == 18-months
data = data %>%
  dplyr::mutate( # eosp = end of study period
    eosp = as.Date( # add 18 months to referral date
      dr2_referral_date) %m+% months(18))

#### 3. Rank referrals ----

data = data %>%
  dplyr::group_by( # group by child
    unique_child_id) %>%
  dplyr::arrange( # sort by referrals by date, from oldest to latest
    dr2_referral_date) %>% 
  dplyr::mutate( 
    referral_number = dplyr::dense_rank(dr2_referral_date)) %>% # assign a rank
  dplyr::ungroup() %>%
  dplyr::relocate(referral_number, .after = 'dr2_referral_date') %>%
  dplyr::arrange(unique_child_id, referral_number)

# How many have more than 1 referral? 
# The following code is using row_number and not dense_rank (to count duplicates)
data %>% 
  dplyr::group_by(referral_number) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

#referral_number count  percent
#<int> <int>    <dbl>
#1               1 27425 71.7    
#2               2  7771 20.3    
#3               3  2113  5.52   
#4               4   688  1.80   
#5               5   173  0.452  
#6               6    65  0.170  
#7               7    16  0.0418 
#8               8     7  0.0183 
#9               9     2  0.00523
#10              10     2  0.00523

data %>% 
  dplyr::filter(referral_number > 1) %>% 
  dplyr::group_by(referral_number) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

#referral_number count percent
#<int> <int>   <dbl>
#1               2  7771 71.7   
#2               3  2113 19.5   
#3               4   688  6.35  
#4               5   173  1.60  
#5               6    65  0.600 
#6               7    16  0.148 
#7               8     7  0.0646
#8               9     2  0.0185
#9              10     2  0.0185

#### 3. Rank CLA dates ----

data = data %>%
  dplyr::group_by( # group by child
    unique_child_id) %>%
  dplyr::arrange( # sort by referrals by date, from oldest to latest
    dr3_start_date_of_cla_period_of_care_start_date) %>% 
  dplyr::mutate( 
    cla_date_number = dplyr::dense_rank(dr3_start_date_of_cla_period_of_care_start_date)) %>% # assign a rank
  dplyr::ungroup() %>%
  dplyr::relocate(cla_date_number, .after = 'dr3_start_date_of_cla_period_of_care_start_date') %>%
  dplyr::arrange(unique_child_id, cla_date_number)

# How many have more than 1 cla date? 
# The following code is using row_number and not dense_rank (to count duplicates)
data %>% 
  dplyr::group_by(cla_date_number) %>% 
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

#cla_date_number count  percent
#<int> <int>    <dbl>
#1               1  1934  5.05   
#2               2   752  1.97   
#3               3   247  0.646  
#4               4    84  0.220  
#5               5    32  0.0836 
#6               6    18  0.0470 
#7               7     3  0.00784
#8               8     3  0.00784
#9              NA 35189 92.0 

data %>% 
  dplyr::filter(cla_date_number > 1) %>% 
  dplyr::group_by(cla_date_number) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

#cla_date_number count percent
#<int> <int>   <dbl>
#1               2   752  66.0  
#2               3   247  21.7  
#3               4    84   7.37 
#4               5    32   2.81 
#5               6    18   1.58 
#6               7     3   0.263
#7               8     3   0.263

#### 3. Intervention assignment ----
data = data %>%
  dplyr::mutate(
    la_implementation_start  = case_when(
      dr2_local_authority == 'walsall' ~ as.Date('2020-09-01'),
      dr2_local_authority == 'lancashire' ~ as.Date('2021-02-01'),
      dr2_local_authority == 'telford' ~ as.Date('2021-06-28'),
      dr2_local_authority == 'wandsworth' ~ as.Date('2022-01-24'),
      dr2_local_authority == 'swindon' ~ as.Date('2022-05-24')
    )) %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::mutate(
    intervention_group = ifelse(
      dr2_referral_date >= la_implementation_start, 1, 0)) %>%
  ungroup()

#### 4. Primary outcome: CLA ----
data = data %>%
  dplyr::mutate(
    cla_status  = case_when(
      is.na(dr3_start_date_of_cla_period_of_care_start_date) ~ 0,
      dr3_start_date_of_cla_period_of_care_start_date >= dr2_referral_date & 
        dr3_start_date_of_cla_period_of_care_start_date <= eosp ~ 1,
      dr3_start_date_of_cla_period_of_care_start_date >= dr2_referral_date & 
        dr3_start_date_of_cla_period_of_care_start_date > eosp ~ 0,
      TRUE ~ NA)) 

# Investigate NAs 
sum(is.na(data$cla_status)) # 57

data %>% 
  dplyr::group_by(dr2_local_authority, cla_status) %>%
  dplyr::summarise(n()) #%>%
  #View()

data %>% 
  dplyr::filter(is.na(cla_status)) %>%
  dplyr::select(dr2_referral_date, dr3_start_date_of_cla_period_of_care_start_date) %>%
  dplyr::mutate(is_cla_date_before_ref_date = dr3_start_date_of_cla_period_of_care_start_date < dr2_referral_date) %>%
  dplyr::group_by(is_cla_date_before_ref_date) %>%
  dplyr::summarise(n()) # 57 cases only where CLA date is before ref date 

data %>%
  dplyr::group_by(dr2_local_authority, cla_status) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

#### 6. Proportion White British ----

white_british_table = data %>%
  dplyr::mutate(white_british = ifelse(
    dr2_ethnicity_clean == 'White (British, Irish or other)', 1, 0)) %>%
  dplyr::group_by(dr2_local_authority, dr2_month_year_referral) %>%
  dplyr::summarise(
    total_referred = sum(n()), 
    total_wb = sum(as.numeric(white_british), na.rm = TRUE),
    prop_white_british = total_wb/total_referred) %>%
  dplyr::select(-total_referred, -total_wb)

data = dplyr::left_join(
  data, white_british_table, 
  join_by('dr2_local_authority', 'dr2_month_year_referral'))

#### 7. QA checks ----

nrow(data) # 38,262
length(unique(data$unique_child_id)) # 27,425 unique children 

# How many have multiple referrals on the same date
cols_for_checks = c(
  'dr2_local_authority', 'dr2_month_return', 'dr2_child_id', 'unique_child_id', 
  'dr2_referral_id_or_case_id', 'dr2_referral_date', 'referral_number', 
  'dr3_start_date_of_cla_period_of_care_start_date', 'cla_date_number',
  'cla_status', 'cla_status_across_referrals',
  'dr2_dob', 'dr2_age_at_referral_clean')

data %>%
  dplyr::group_by(unique_child_id, dr2_referral_date) %>%
  dplyr::mutate(dup_referral_date = n() > 1) %>%
  dplyr::ungroup() %>%
  dplyr::filter(dup_referral_date == TRUE) %>%
  dplyr::select(any_of(cols_for_checks)) #%>%
  #View()

# How many have multiple CLA dates for the same referral
data %>%
  dplyr::group_by(unique_child_id, dr3_start_date_of_cla_period_of_care_start_date) %>%
  dplyr::mutate(dup_cla_date = n() > 1) %>%
  dplyr::filter(dup_cla_date == TRUE) %>%
  dplyr::filter(!is.na(dr3_start_date_of_cla_period_of_care_start_date)) %>%
  dplyr::ungroup() %>%
  dplyr::select(any_of(cols_for_checks)) #%>%
  #View()

# Where there are many rows for the same unique ID is because of the many-to-many relationships 
# Selection of the unique records should be based on: 
# Earliest referral recorded + whether or not the child was looked after within 18-months for this referral
# Across all the CLA dates they have for this child 
# Thus filter should be ref_number == 1 and cla_date_number == 1 or is.na(cla_date_number)

#### 8. Keep first referral only ----

# Keep first referral in study period 
# Make sure that if multiple CLA dates are associated with this referral 
# That the earliest date is kept 

# scenario #1 child does not have CLA date 
# ALL their referral rows will have cla date number == NA 
# First referral will be kept

# scenario #2 child has 1 CLA date
# ALL their referral rows will have cla date number == 1 
# will keep the referral row that has the combination:
# referral number ==  & cla_number == 1 
# ie, just keeping first referral, so all good 

# scenario #3 child has multiple CLA dates (different dates)
# ALL their referral rows will have !is.na(cla date number) (ie 1 or more)
# will keep the referral row that has the combination:
# referral number ==  & cla_number == 1 
# Bc of the many-to-many linkage, this will be a unique combination
# that reflects the earliest CLA date for a child, tagged to its earliest referral date 
# All subsequent CLA dates for the same child are ignored 
# All subsequent referral dates for the same child are ignored 

nrow(data) # 38,262
length(unique(data$unique_child_id)) # 27,425

first_referral_data = data %>%
  dplyr::group_by( # group by child
    dr2_local_authority, unique_child_id) %>%
  dplyr::filter(
    referral_number == 1 & (cla_date_number == 1 | is.na(cla_date_number))) %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>% 
  dplyr::ungroup() 

nrow(first_referral_data) # 27,425
length(unique(first_referral_data$unique_child_id)) # 27,425

first_referral_data %>%
  dplyr::group_by(dr2_local_authority, cla_status) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

#### 9. Select col of interest only ----

colnames(first_referral_data)

analysis_vars = c(
  'dr2_local_authority',
  'la_implementation_start',
  'wedge',
  'unique_child_id',
  'dr2_referral_date',
  'referral_number',
  'dr2_month_year_referral',
  'intervention_group',
  'dr3_start_date_of_cla_period_of_care_start_date',
  'cla_date_number',
  'cla_status',
  'dr2_referral_no_further_action_clean',
  'dr2_dob',
  'dr2_age_at_referral_numeric_clean',
  'dr2_age_at_referral_clean',
  'dr2_gender',
  'dr2_gender_clean',
  'dr2_ethnicity',
  'dr2_ethnicity_clean',
  'dr2_disability_status_clean',
  'dr2_uasc_clean',
  'dr2_number_of_previous_child_protection_plans',
  'dr2_number_of_previous_cpp_clean',
  'dr2_outcome_of_single_assessment',
  "dr2_factors_identified_at_the_end_of_assessment_1b",
  "dr2_factors_identified_at_the_end_of_assessment_2b",          
  "dr2_factors_identified_at_the_end_of_assessment_3a",
  "dr2_factors_identified_at_the_end_of_assessment_3b",          
  "dr2_factors_identified_at_the_end_of_assessment_3c",
  "dr2_factors_identified_at_the_end_of_assessment_4b",
  'dr1_prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils',
  'prop_white_british'
)

analytical_dataset = first_referral_data %>%
  dplyr::select(all_of(analysis_vars))

analytical_dataset = analytical_dataset %>%
  rename_with(~ gsub("dr2_|dr1_|dr3_", "", .x))

### 10. Add labels for LAs ----
la_levels = c('walsall', 'lancashire', 'telford', 'wandsworth', 'swindon')

analytical_dataset = analytical_dataset %>%
  dplyr::mutate(local_authority = factor(local_authority, levels = la_levels))

### 11. Final checks ----
# Ref and age inclusion criteria checks
min(analytical_dataset$age_at_referral_numeric_clean, na.rm = TRUE)
max(analytical_dataset$age_at_referral_numeric_clean, na.rm = TRUE)

min(analytical_dataset$referral_date, na.rm = TRUE)
max(analytical_dataset$referral_date, na.rm = TRUE)

# CLA dates and Ref dates checks
in_care_before_first_referral = analytical_dataset %>%
  dplyr::filter(is.na(cla_status)) 

nrow(in_care_before_first_referral) # 19 people, will need to remove these records

# Removing cla date < ref date
analytical_dataset = analytical_dataset %>%
  dplyr::rename('cla_start_date' = 'start_date_of_cla_period_of_care_start_date') %>%
  dplyr::filter(cla_start_date >= referral_date | is.na(cla_start_date))
 
# Intervention assignment and outcome checks  
analytical_dataset %>%
  dplyr::group_by(local_authority, intervention_group) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

cla_table = analytical_dataset %>%
  dplyr::group_by(local_authority, cla_status) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

nrow(analytical_dataset)
colnames(analytical_dataset)

mean(cla_table[cla_table$cla_status == '1',]$percent) # 5.19%

## Save dataset ----
saveRDS(analytical_dataset, file = paste0(
  output_path,"/primary_outcome_sample_analytical_dataset_without_unborn.Rds")) 
