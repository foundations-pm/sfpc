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
data_path = paste0(sharepoint_path, '/Datasets/analytical_datasets')
output_path = data_path

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

## Load data --------------------------------------------------------------------

# Main sample: children referred to CSC during trial period
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/primary_outcome_sample_analytical_dataset_without_unborn.Rds")) 

## Unborn analysis --------------------------------------------------------------

### Workplan ----
# Summary of issue: 
# Unborn babies can be referred to CSC
# Data collection for unborn babies is variable 
# It's hard to distinguish between unborn missing, and missing missing 
# Want to create a flag for unborn babies

# Workplan 
# 1 - Check values that could be unborn for:
# Age
# Gender
# Ethnicity 

# 2 - Then understand whether combinations of these values indicate
# unborn referrals with any certainty 

#### Check values in age, gender, ethnicity ----

# Unborn values for age:
# NAs (279)
# -1, -2, 0, 122
data %>% 
  dplyr::group_by(age_at_referral_clean) %>%
  dplyr::summarise(n())

# Unborn values for gender:
# Unborn OR not stated or recorded
# Unknown or not stated/recorded
# To note, there was 0 'true' NAs in the gender field in DR2
data %>% 
  dplyr::group_by(gender, gender_clean) %>%
  dplyr::summarise(n()) #%>%
  #View()

data %>% 
  dplyr::group_by(gender_clean) %>%
  dplyr::summarise(n())

# Unborn values for ethnicity:
# NA (= NA or refused)
# 'Information not yet obtained'
data %>% 
  dplyr::group_by(ethnicity, ethnicity_clean) %>%
  dplyr::summarise(n()) #%>%
  #View()

data %>% 
  dplyr::group_by(ethnicity_clean) %>%
  dplyr::summarise(n())

# 3-way analysis 
data %>% 
  dplyr::filter(
    age_at_referral_clean %in% c('-1', '-2', '0', '122', NA)) %>%
  nrow() # 4,580

data %>% 
  dplyr::filter(gender_clean %in% c(
    'Unborn OR not stated or recorded', 
    'Unknown or not stated/recorded')) %>%
  nrow()  # 764

data %>% 
  dplyr::filter(ethnicity_clean %in% c(
    'Information not yet obtained', NA)) %>%
  nrow() # 2,652

unborn_analysis_table = data %>% 
  dplyr::group_by(
    age_at_referral_clean, gender_clean, ethnicity_clean) %>%
  dplyr::summarise(count = n()) 

sum(unborn_analysis_table$count) # 479

#### Unborn flag ----

##### Flag rules ----

# Where 
# scenario #1 
# where age < 0 == UNBORN 
# regardless of values for gender and ethnicity 

# scenario #2
# where age == 122 == UNBORN 
# regardless of values for gender and ethnicity 

# scenario #3
# where age >= 0 and <= 12
# if gender %in% c(
# 'Unborn OR not stated or recorded', 
# 'Unknown or not stated/recorded') == child is born BUT gender is missing 
# if ethnicity %in% c(
# 'Information not yet obtained', NA) == child is born BUT ethnicity is missing 

# scenario #4 
# where is.na(age) = 279 rows

# Most NAs coming from Swindon (205 = 73%)
data %>% 
  dplyr::filter(is.na(dob)) %>%
  dplyr::group_by(local_authority, cla_status) %>%
  dplyr::summarise(count = n())

# Most NAs are for non-CLA children 
# 9.41% NAs have become CLA 
data %>% 
  dplyr::group_by(cla_status) %>%
  dplyr::summarise(count = n())
# cla_status count
#1          0   255
#2          1    24 (24/1,579 [total cla over period])

# It looks like 90% of cases are genuine unborn children 
# Leaving a few cases up for debate 
# Will assume is.na()
data %>% 
  dplyr::filter(is.na(dob)) %>%
  dplyr::group_by(age_at_referral_clean, gender_clean, ethnicity_clean) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(percent = count/sum(count)*100) %>%
  dplyr::arrange(desc(count)) #%>%
  #View()

##### Derive unborn flag ----

# When age < 0, age == 122 or is.na(age) = UNBORN 
unborn_levels = c('born', 'unborn')

data = data %>%
  dplyr::mutate(
  unborn_flag = case_when(
    c(age_at_referral_clean < 0 | 
        age_at_referral_clean == 122 | 
        is.na(age_at_referral_clean)) ~ 'unborn',
    TRUE ~ 'born'),
  unborn_flag = factor(unborn_flag, levels = unborn_levels)) %>%
  dplyr::relocate(unborn_flag, .after = dob)

# Checks
unborn_analysis_table = data %>% 
  dplyr::group_by(
    unborn_flag, age_at_referral_clean, gender_clean, ethnicity_clean) %>%
  dplyr::summarise(count = n()) 

#### Assign true NA for age, gender and ethnicity ----

#1 if unborn_flag == unborn, then age == -1 
age_levels = c(
  '-1', '0', '1', '2', '3','4', '5',
  '6', '7', '8', '9', '10', '11','12')

data = data %>%
  dplyr::mutate(
    age_at_referral_final = case_when(
      unborn_flag == 'unborn' | age_at_referral_clean == '-2' ~ '-1',
      TRUE ~ age_at_referral_clean),
    age_at_referral_numeric_final = case_when(
      unborn_flag == 'unborn'| age_at_referral_numeric_clean == -2 ~ -1,
      TRUE ~ age_at_referral_numeric_clean)) %>%
  dplyr::mutate(age_at_referral_final = factor(age_at_referral_final, levels = age_levels)) %>%
  dplyr::relocate(age_at_referral_final, .after = age_at_referral_clean) %>%
  dplyr::relocate(age_at_referral_numeric_final, .after = age_at_referral_numeric_clean)

# Checks
data %>%
  dplyr::group_by(age_at_referral_final, age_at_referral_clean) %>%
  dplyr::summarise(n())

# Note: All numeric ages below 0 now are -1 
# See below:
data %>%
  dplyr::group_by(age_at_referral_numeric_final, age_at_referral_numeric_clean) %>%
  dplyr::summarise(n()) #%>%
  #View()

#2 all the possible 'unborn values' for gender and ethnicity to be coded NA 
# and to be imputed 

gender_levels = c(
  'Male', 'Female', 'Other')

ethnicity_levels = c(
  'White (British, Irish or other)',
  'Asian, Asian British or Asian Welsh',
  'Black, Black British, Black Welsh, Caribbean or African',
  'Mixed or Multiple ethnic groups',
  'Other ethnic group')

data = data %>%
  dplyr::mutate(
    
    gender_final = case_when(
      gender_clean %in% c(
        'Unborn',
        'Unborn OR not stated or recorded', 
        'Unknown or not stated/recorded') ~ NA,
      TRUE ~ gender_clean),
    
    ethnicity_final = case_when(
      ethnicity_clean == 'Information not yet obtained' ~ NA,
      TRUE ~ ethnicity_clean)) %>%
  
  dplyr::mutate(
    gender_final = factor(gender_final, levels = gender_levels),
    ethnicity_final = factor(ethnicity_final, levels = ethnicity_levels)) %>%
  
  dplyr::relocate(
    gender_final, .after = gender_clean) %>%
  
  dplyr::relocate(
    ethnicity_final, .after = ethnicity_clean)

#### Missingness checks ----

# 3.45% missingness in gender final 
data %>% dplyr::filter(is.na(gender_final)) %>% nrow() / nrow(data) *100

# 9.68% missingness in ethnicity final 
data %>% dplyr::filter(is.na(ethnicity_final)) %>% nrow() / nrow(data) *100

# Checks
unborn_analysis_table = data %>% 
  dplyr::group_by(
    local_authority, unborn_flag, 
    age_at_referral_final, gender_final, ethnicity_final) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::arrange(desc(unborn_flag))

nrow(data)

# Intervention assignment and outcome checks  
intervention_group_table = data %>%
  dplyr::group_by(local_authority, intervention_group) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

cla_table = data %>%
  dplyr::group_by(local_authority, cla_status) %>%
  dplyr::summarise(count = n()) %>%
  dplyr::mutate(percent = count/sum(count)*100)

mean(cla_table[cla_table$cla_status == '1',]$percent) # 5.19%

# Save analytical dataset ----
saveRDS(data, file = paste0(
  output_path,"/primary_outcome_sample_analytical_dataset_final.Rds")) 
