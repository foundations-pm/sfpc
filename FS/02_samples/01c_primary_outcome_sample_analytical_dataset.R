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
  data_path, "/primary_outcome_sample_raw.Rds")) 

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

#### 3. Treatment assignment ----
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

#### 4. Primary outcome ----
data = data %>%
  dplyr::mutate(
    cla_status  = case_when(
      is.na(dr3_start_date_of_cla_period_of_care_start_date) ~ 0,
      dr3_start_date_of_cla_period_of_care_start_date >= dr2_referral_date & 
        dr3_start_date_of_cla_period_of_care_start_date <= eosp ~ 1,
      dr3_start_date_of_cla_period_of_care_start_date >= dr2_referral_date & 
        dr3_start_date_of_cla_period_of_care_start_date > eosp ~ 0,
      TRUE ~ NA)) 

# only 53 NAs - these are because care started before referral 
# will deal with this during QA

# CLA status ACROSS referrals, within 18-months of ANY refs
data = data %>%
  dplyr::group_by(unique_child_id) %>%
  dplyr::mutate(
    cla_status_across_referrals = sapply(dr2_referral_date, function(ref_date) {
      any(
        !is.na(dr3_start_date_of_cla_period_of_care_start_date) &   # there is an outcome date
          dr3_start_date_of_cla_period_of_care_start_date >= ref_date &   # not before this event
          dr3_start_date_of_cla_period_of_care_start_date <= ref_date %m+% months(18)  # within 18 months of this event
      )
    })
  ) %>%
  ungroup()

# Checks 

# Investigate NAs 
sum(is.na(data$cla_status)) # 74 

data %>% 
  dplyr::group_by(dr2_local_authority, cla_status) %>%
  dplyr::summarise(n()) %>%
  View()

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

#### 7. Select col of interest only ----

colnames(data)

analysis_vars = c(
  'dr2_local_authority',
  'la_implementation_start',
  'wedge',
  'unique_child_id',
  'dr2_referral_date',
  'dr2_month_year_referral',
  'intervention_group',
  'cla_status',
  'dr2_referral_no_further_action_clean',
  'dr2_dob',
  'dr2_age_at_referral_numeric_clean',
  'dr2_age_at_referral_clean',
  'dr2_gender_clean',
  'dr2_ethnicity_clean',
  'dr2_disability_status_clean',
  'dr2_uasc_clean',
  'dr2_outcome_of_single_assessment',
  "dr2_factors_identified_at_the_end_of_assessment_1b",
  "dr2_factors_identified_at_the_end_of_assessment_2b",          
  "dr2_factors_identified_at_the_end_of_assessment_3a",
  "dr2_factors_identified_at_the_end_of_assessment_3b",          
  "dr2_factors_identified_at_the_end_of_assessment_3c",
  "dr2_factors_identified_at_the_end_of_assessment_4b",
  'dr1_prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils',
  'prop_white_british',
  'dr3_start_date_of_cla_period_of_care_start_date'
)

analytical_dataset = data %>%
  dplyr::select(all_of(analysis_vars))

analytical_dataset = analytical_dataset %>%
  rename_with(~ gsub("dr2_|dr1_|dr3_", "", .x))

#### 8. QA checks ----

nrow(analytical_dataset) # 38,262
length(unique(analytical_dataset$unique_child_id)) # 27,425 unique children 

# Ref and age inclusion criteria checks
min(analytical_dataset$age_at_referral_numeric_clean, na.rm = TRUE)
max(analytical_dataset$age_at_referral_numeric_clean, na.rm = TRUE)

min(analytical_dataset$referral_date, na.rm = TRUE)
max(analytical_dataset$referral_date, na.rm = TRUE)

#### 9. Keep first referral only ----

analytical_dataset = analytical_dataset %>%
  dplyr::group_by( # group by child
    unique_child_id) %>%
  dplyr::arrange( # sort by referrals by date, from oldest to latest
    referral_date) %>% 
  dplyr::mutate( 
    referral_number = dplyr::dense_rank(referral_date)) %>% # assign a rank
  dplyr::ungroup() %>%
  dplyr::relocate(referral_number, .after = 'referral_date') %>%
  dplyr::arrange(unique_child_id, referral_number)


## Save dataset ----

