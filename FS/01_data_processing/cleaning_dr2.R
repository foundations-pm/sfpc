#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## DATA CLEANING: DATA RETURN 2 ----

## 1. Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/pre_processing/DR2')
output_path = paste0(sharepoint_path, '/Datasets/cleaning')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

### Load data ---- 
setwd(data_path)

data = readRDS(file = paste0(
  data_path,"/DR2_pre_processed_data.Rds")) 

### Clean data ----

#### 1. Check unique values for each column ----
columns_to_clean = data %>%
  dplyr::select(
    referral_no_further_action,
    contains('assessment'),
    number_of_previous_child_protection_plans,     
    gender,                                                 
    ethnicity,                                     
    disabled_status,                               
    unaccompanied_asylum_seeker, 
    -factors_identified_at_the_end_of_assessment,
    -contains('date')) %>%
  names()

count_unique_values_table = purrr::map_dfr(
  columns_to_clean, ~ 
    
    data %>% 
    dplyr::group_by(get(.x)) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(colname = .x) %>%
    dplyr::rename(value = `get(.x)`) %>%
    dplyr::relocate(colname)
)

#### 2. Clean referral NFA ----

data = dplyr::mutate(
    data,
    referral_no_further_action_clean = case_when(
      referral_no_further_action == '#MULTIVALUE' ~ NA,
      TRUE ~ referral_no_further_action))

#### 3. Gender NFA ----

# Question with re-coding gender 
# How to differentiate: (1) neither male or female (2) unborn (3) missing / unknown

# Dim data = 64,009 rows / referral dates
# There are 44,035 unique child IDs
# There are 364 missing DOBs

# Trial period is April 2020 to November 2022
# In this period, there was 36,613 children who got referred
data %>% 
  dplyr::filter(referral_date > '2020-03-31' & referral_date <= '2022-11-24') %>% 
  dplyr::distinct(child_id) %>%
  nrow()

all_unknown_or_unborn_gender_values = c(
  'Unborn', 'Not stated/recorded (or unborn)',
  'c) Not stated/recorded (or unborn)', 
  'Indeterminate', 'Gender Unspecified',
  'Unknown', 'Not Known',
  'c) Not Stated', 'Not stated/recorded'
)

# Add age variable 
# To investigate size of issue with unborns
data = data %>% 
  dplyr::mutate(age_at_referral = as.character(
    round((referral_date - year_and_month_of_birth_of_the_child)/365.25)))

count_of_unknown_gender_by_age = data %>%
  dplyr::filter(referral_date > '2020-03-31' & referral_date <= '2022-11-24') %>%
  dplyr::filter(gender %in% all_unknown_or_unborn_gender_values) %>%
  dplyr::group_by(gender, age_at_referral) %>%
  dplyr::summarise(n())

# Recode gender: clean categories
# Recoding / regrouping will happen when deriving analytical datasets
gender_levels = c(
  'Male', 'Female', 'Other',
  'Unborn', 'Unborn OR not stated or recorded',
  'Not stated or recorded', 'Unknown')

data = data %>% 
  dplyr::mutate(
    
  gender_clean = case_when(
    gender %in% c('Male', 'a) Male') ~ 'Male',
    gender %in% c('Female', 'b) Female') ~ 'Female', 
    gender %in% c('Neither', 'c) Neither', 'd) Neither') ~ 'Other', 
    gender %in% c('Not stated/recorded (or unborn)',
                  'c) Not stated/recorded (or unborn)') ~ 'Unborn OR not stated or recorded',
    gender %in% c('Unknown', 'Not Known') ~ 'Unknown',
    gender %in% c('c) Not Stated','Not stated or recorded') ~ 'Not stated or recorded',
    TRUE ~ gender
  ),
  
  gender_clean = factor(gender, levels = gender_levels)
)

#### 4. Ethnicity NFA ----

data = data %>% dplyr::mutate(
  
  ethnicity_clean = case_when(
    
    ethnicity %in% c(
      'WBRI', 'WIRI', 'WEUR',
      'a) WBRI', 'b) WIRI') ~ 
      'White British or Irish',
    
    ethnicity %in% c(
      'WIRT', 'WOTH', 'WROM', 
      'c) WIRT','d) WOTH', 'e) WROM') ~ 
      'White: Gypsy, Irish Traveller, Roma or Other White',
    
    ethnicity %in% c(
      'MWBC','MWBA','MWAS','MOTH','MWOE',
      'f) WMBC', 'g) MWAS',
      'f) MWBC','g) MWBA','h) MWAS','i) MOTH') ~ 
      'Mixed or Multiple ethnic groups',
    
    ethnicity %in% c(
      'AIND','APKN', 'AOPK','ABAN','AOTH', 'AOTA', 'CHNE',
      'j) AIND', 'k) APKN', 'l) ABAN', 'm) AOTH', 'q) CHNE',
      'APAK') ~ 
      'Asian, Asian British or Asian Welsh',
    
    ethnicity %in% c(
      'BCRB','BAFR','BOTH',
      'n) BCRB', 'o) BAFR', 'p) BOTH') ~ 
      'Black, Black British, Black Welsh, Caribbean or African',
    
    ethnicity %in% c(
      'OAFG', 'OIRN',
      'OOTH', 'r) OOTH') ~ 
      'Other ethnic group',
    
    ethnicity %in% c(
      'NOBT', 's) NOBT',
      's) NPBT', 't) NOBT') ~ 'Information not yet obtained',
    
    ethnicity %in% c(
      'To Code', 'Not Recorded', 'REFU', 'NULL',
      's) REFU') | is.na(ethnicity) ~ NA, 
    
    TRUE ~ ethnicity)
)

#### 5. Disability status NFA ----

data = dplyr::mutate(
  data,
  disabled_status_clean = case_when(
    disabled_status == 'Y' ~ 'Disabled',
    disabled_status %in% c('N', 'b) No') ~ 'Not disabled',
    TRUE ~ disabled_status))

#### 6. UASC status NFA ----



#### 7. Number of previous CP plans ----



#### 8. Outcome of single assessment ----



#### 9. Factors identified at the end of assessment ----




