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
data_path = paste0(sharepoint_path, '/Datasets/cleaning/All cleaned DR linked')
output_path = paste0(sharepoint_path, '/Datasets/samples')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

## Load data --------------------------------------------------------------------
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/all_dr_cleaned_data.Rds")) 

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

primary_sample_data = dplyr::mutate(
  primary_sample_data, 
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
primary_sample_data = primary_sample_data %>%
  dplyr::mutate( # eosp = end of study period
    eosp = as.Date( # add 18 months to referral date
      dr2_referral_date) %m+% months(18))

#### 3. Treatment assignment ----
primary_sample_data = primary_sample_data %>%
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
primary_sample_data = primary_sample_data %>%
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

#### 6. Proportion White British ----

white_british_table = primary_sample_data %>%
  dplyr::mutate(white_british = ifelse(
    dr2_ethnicity_clean == 'White (British, Irish or other)', 1, 0)) %>%
  dplyr::group_by(dr2_local_authority, dr2_month_year_referral) %>%
  dplyr::summarise(
    total_referred = sum(n()), 
    total_wb = sum(as.numeric(white_british), na.rm = TRUE),
    prop_white_british = total_wb/total_referred) %>%
  dplyr::select(-total_referred, -total_wb)

primary_sample_data = dplyr::left_join(
  primary_sample_data, white_british_table, 
  join_by('dr2_local_authority', 'dr2_month_year_referral'))

#### 7. Proportion FSM ----

# Already exists 
# Problem is Lancashire and Walsall have a fair few missing values 
primary_sample_data %>% 
  dplyr::summarise(
    min(dr1_prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils, na.rm = TRUE),
    max(dr1_prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils, na.rm = TRUE)) 


#### 8. Quality Assurance ----

#### 9. Keep first referral only ----



## Save dataset ----

