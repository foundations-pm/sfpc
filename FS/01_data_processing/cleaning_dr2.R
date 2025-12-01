#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## DATA CLEANING: DATA RETURN 2 ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

## 1. Set-up  ----
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/pre_processing/DR2')
output_path = paste0(sharepoint_path, '/Datasets/cleaning')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

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

nfa_levels = c('Further action', 'No further action')

data = dplyr::mutate(
    data,
    referral_no_further_action_clean = case_when(
      referral_no_further_action == '0' ~ 'No further action',
      referral_no_further_action == '1' ~ 'Further action',
      referral_no_further_action == '#MULTIVALUE' ~ NA,
      TRUE ~ referral_no_further_action),
    referral_no_further_action_clean = factor(
      referral_no_further_action_clean, levels = nfa_levels)
    )

#### 3. Add age ----

# Turn 'year_and_month_of_birth_of_the_child' into dob
data = data %>% 
  dplyr::rename('dob' = 'year_and_month_of_birth_of_the_child')

# Add age variable 
# To investigate size of issue with unborns
data = data %>% 
  dplyr::mutate(age_at_referral_clean = as.character(
    round(
      (referral_date - dob)/365.25)
    )
  )

# rounded at 2 decimal
data = data %>%
  
  dplyr::mutate(
    age_at_referral_numeric_clean = as.numeric(
      round((referral_date - dob)/365.25, 2)), 
    
    age_at_referral_clean = as.character(
      floor(age_at_referral_numeric_clean)
    ))

data = data %>%
  dplyr::relocate(
    c(age_at_referral_numeric_clean, age_at_referral_clean),
    .after = 'dob')

# not adding levels yet until data is linked and 
# analytical samples are derived 
# since this will change the number of levels in the age var 

#### 4. Gender ----

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

count_of_unknown_gender_by_age = data %>%
  dplyr::filter(referral_date > '2020-03-31' & referral_date <= '2022-11-24') %>%
  dplyr::filter(gender %in% all_unknown_or_unborn_gender_values) %>%
  dplyr::group_by(gender, age_at_referral_clean) %>%
  dplyr::summarise(n())

# Recode gender: clean categories
# Recoding / regrouping will happen when deriving analytical datasets
gender_levels = c(
  'Male', 'Female', 'Other',
  'Unborn', 'Unborn OR not stated or recorded',
  'Unknown or not stated/recorded')

data = data %>% 
  dplyr::mutate(
    
  gender_clean = case_when(
    
    gender %in% c('Male', 'a) Male') ~ 'Male',
    
    gender %in% c('Female', 'b) Female') ~ 'Female', 
    
    gender %in% c('Neither', 'c) Neither', 'd) Neither') ~ 'Other', 
    
    gender %in% c(
      'Not stated/recorded (or unborn)',
      'c) Not stated/recorded (or unborn)') ~ 'Unborn OR not stated or recorded',
    
    gender %in% c(
      'Unknown', 'Not Known', 'Indeterminate', 'Gender Unspecified',
      'c) Not Stated','Not stated or recorded', 'Not stated/recorded') ~ 'Unknown or not stated/recorded',

    TRUE ~ gender),
  
  gender_clean = factor(gender_clean, levels = gender_levels)
  
)

#### 5. Ethnicity ----

ethnicity_levels = c(
  'White (British, Irish or other)',
  'Asian, Asian British or Asian Welsh',
  'Black, Black British, Black Welsh, Caribbean or African',
  'Mixed or Multiple ethnic groups',
  'Other ethnic group',
  'Information not yet obtained'
)

data = data %>% dplyr::mutate(
  
  ethnicity_clean = case_when(
    
    ethnicity %in% c(
      'WBRI', 'WIRI', 'WEUR',
      'a) WBRI', 'b) WIRI',
      'WIRT', 'WOTH', 'WROM', 
      'c) WIRT','d) WOTH', 'e) WROM') ~ 
      'White (British, Irish or other)',
    
    #ethnicity %in% c(
    #  'WIRT', 'WOTH', 'WROM', 
    #  'c) WIRT','d) WOTH', 'e) WROM') ~ 
    #  'White: Gypsy, Irish Traveller, Roma or Other White',
    
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
    
    TRUE ~ ethnicity),
  
  ethnicity_clean = factor(ethnicity_clean, levels = ethnicity_levels)
  
)

#### 6. Disability status ----
disability_levels = c('Not disabled', 'Disabled')

data = dplyr::mutate(
  data,
  disability_status_clean = case_when(
    disabled_status == 'Y' ~ 'Disabled',
    disabled_status %in% c('N', 'b) No') ~ 'Not disabled',
    TRUE ~ disabled_status),
  disability_status_clean = factor(
    disability_status_clean, levels = disability_levels)
  )

#### 7. UASC status  ----

uasc_levels = c('Not UASC or refugee','UASC or refugee')

data = dplyr::mutate(
  data,
  uasc_clean = case_when(
    
    unaccompanied_asylum_seeker %in% c(
      'Y',  '1', 'Unaccompanied Asylum Seeking Child',
      'Refugee Status') ~ 'UASC or refugee',
    
    unaccompanied_asylum_seeker %in% c(
      'N', '0','British Citizen') ~ 'Not UASC or refugee',
    
    unaccompanied_asylum_seeker %in% c(
      'No Immigration Status Recorded', '#MULTIVALUE') ~ NA,
    
    TRUE ~ unaccompanied_asylum_seeker),
  
  uasc_clean = factor(uasc_clean, levels = uasc_levels)
)


#### 8. Number of previous CP plans ----
number_previous_cp_levels = c('0', '1', '2', '3+')

data = dplyr::mutate(
  data,
  number_of_previous_cpp_clean = case_when(
    number_of_previous_child_protection_plans %in% c('3', '4', '5') ~ '3+',
    !(number_of_previous_child_protection_plans %in% c('0','1', '2', '3+')) ~ '0', # Nas and ' ' recoded as 0
    TRUE ~ number_of_previous_child_protection_plans), 
  number_of_previous_cpp_clean = factor(
    number_of_previous_cpp_clean, levels = number_previous_cp_levels)
)


#### 9. Outcome of single assessment ----
outcome_of_single_assessment_levels = c('No', 'Yes')

data = dplyr::mutate(
  data,
  
  outcome_of_single_assessment_clean = case_when(
    
    outcome_of_single_assessment %in% c(
      'Yes', 'a) Yes', 'yes', 'a) yes') ~ 'Yes',
    
    outcome_of_single_assessment %in% c(
      'No', 'no', 'b) No', 'B) No', 'b) no', 'B) no') ~ 'No',
    
    outcome_of_single_assessment %in% c(
      'NULL', 'N/A', 'Not recorded', 'Incomplete') ~ NA,
    
    TRUE ~ outcome_of_single_assessment),
  
  outcome_of_single_assessment_clean = factor(
    outcome_of_single_assessment_clean, 
    levels = outcome_of_single_assessment_levels)
  
  )

### Checks ----
clean_column = data %>% dplyr::select(
  contains('clean'), - age_at_referral_numeric_clean) %>% names()

count_unique_clean_values_table = purrr::map_dfr(
  clean_column, ~ 
    
    data %>% 
    dplyr::group_by(get(.x)) %>%
    dplyr::summarise(count = n()) %>%
    dplyr::mutate(colname = .x) %>%
    dplyr::rename(value = `get(.x)`) %>%
    dplyr::relocate(colname)
)

tables = c('count_unique_clean_values_table', 'count_unique_values_table')

mget(tables) %>% # fetch objects safely by name
  purrr::iwalk(~ write_xlsx(
    .x,
    file.path(output_path, "DR2/QA", paste0(.y, file_date, ".xlsx"))
  ))

### Save data ----

# append dr2 prefix to identify columns coming from dr2
colnames(data) <- paste0("dr2_", colnames(data))

saveRDS(data, file = paste0(
  output_path,"/DR2/DR2_cleaned_data.Rds")) 
