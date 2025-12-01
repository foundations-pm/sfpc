#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## ANALYSIS: PRIMARY OUTCOME SAMPLE ----

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

## Quality assurance --------------------------------------------------------------

### Missing referral dates ----

# check NAs in referral date 
# to assess whether we're dropping NAs at this tage
sum(is.na(data$dr2_referral_date)) # 7 NAs 

data %>% 
  dplyr::filter(is.na(dr2_referral_date)) 
# all from Wandsworth, 1 in april 2023 and 6 in Nov 22 return
# dropping these NAs - neligeable numbers 

### Missing DOB ----
sum(is.na(data$dr2_dob)) #365 NAs 

data %>% 
  dplyr::filter(is.na(dr2_dob)) %>%
  #dplyr::distinct(dr2_child_id, .keep_all = TRUE) %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(count_missing = n()) # total 365 records, 329 unique IDs

#dr2_local_authority count_missing
#1 swindon                    250
#2 walsall                     17
#3 wandsworth                  98

# Missing within trial period
data %>% 
  dplyr::filter(is.na(dr2_dob)) %>%
  dplyr::filter(dr2_referral_date >= '2020-03-01' & dr2_referral_date <= '2022-11-24') %>%
  #dplyr::distinct(dr2_child_id, .keep_all = TRUE) %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(count_missing = n()) # total 309, 279 unique IDs

# Could be unborn children
# Check: 
data %>% 
  dplyr::filter(is.na(dr2_dob)) %>%
  dplyr::filter(dr2_referral_date >= '2020-03-01' & dr2_referral_date <= '2022-11-24') %>%
  dplyr::group_by(dr2_gender_clean) %>%
  dplyr::summarise(count_potential_unborn = n())

# dr2_gender_clean                 count_potential_unborn
#<fct>                                             <int>
#1 Male                                                  6
#2 Female                                                7
#3 Unborn                                               43
#4 Unborn OR not stated or recorded                    234
#5 Unknown                                              19

# 89.9% with DOB missing are registered as unborn, unborn or not stated/recorded

### Unique IDs ----
unique_IDs = length(unique(data$dr2_child_id)) # 44,035

### Same IDs, different children ----

# Total nb non-unique IDs
non_unique_child_ids = data %>%
  dplyr::group_by(dr2_child_id) %>%
  dplyr::summarise(has_inconsistent_DOB = n_distinct(dr2_dob) > 1) %>%
  dplyr::filter(has_inconsistent_DOB) 

nrow(non_unique_child_ids) # 159 non-unique IDs
nrow(non_unique_child_ids)/unique_IDs*100  

# Total nb non-unique IDs within LAs
non_unique_child_ids_by_la = data %>%
  dplyr::group_by(dr2_local_authority, dr2_child_id) %>%
  dplyr::summarise(has_inconsistent_DOB = n_distinct(dr2_dob) > 1) %>%
  dplyr::filter(has_inconsistent_DOB) 

nrow(non_unique_child_ids_by_la) # 105 non-unique IDs within LAs
nrow(non_unique_child_ids_by_la)/unique_IDs*100 

# Shared IDs between LAs should not be a problem
# We're always grouping within LAs - and filtering will be based on first referral
# within the LA

# Number IDs shared between LAs
nrow(non_unique_child_ids) - nrow(non_unique_child_ids_by_la) # 54

# Non-unique IDs within LAs 
# For referrals during trial period 
data %>%
  dplyr::filter(dr2_referral_date >= '2020-03-01' & dr2_referral_date <= '2022-11-24') %>%
  dplyr::group_by(dr2_local_authority, dr2_child_id) %>%
  dplyr::summarise(has_inconsistent_DOB = n_distinct(dr2_dob) > 1) %>%
  dplyr::filter(has_inconsistent_DOB) %>%
  View() # 79 IDs are shared

# What to do with non-unique IDs?
# It means, one of 3 things:
# Scenario 1 - Either the ALL children sharing the same ID are meant to be included - that's fine 
# Under scenario 1, it doesn't impact analyses 
# OR, a fraction of these children are not meant to be included - in which case
# we need to remove those who aren't meant to be included 

# Solution is to assign truly unique IDs and group by these 
# Unique IDs = child ID + same DOB

### Same children, different IDs ----
# This will be impossible to identify, as we cannot distinguish 
# between 2 children born on the same day with different child IDs 
# or whether this is the same children but assigned 2 different IDs 

### Assign new unique IDs ----

# Rules: 
# If 1 single record for this child ID > Child ID does not change 
# If multiple records with same child ID:
# scenario 1 - same DOB across ALL records > Child ID does not change 
# scenario 2 - different DOB across records, no NAs > 1 unique ID per DOB with OG ID 
# scenario 3 - 1 DOB and 1 or more NAs > Child ID does not change, considered the same child (but was unborn at some point)
# scenario 4 - multiple DOBs and 1 or more NAs > flag as messy and check records manually

cleaned_id_data <- data %>%
  dplyr::group_by(dr2_child_id) %>%
  dplyr::mutate(
    
    # how many distinct non-missing DOBs in this ID?
    n_dob_non_na = n_distinct(dr2_dob[!is.na(dr2_dob)]),
    
    dob_rank = dense_rank(dr2_dob),   # same DOB = same rank, NA = NA
    
    # new “true person” ID
    unique_child_id = case_when(
      n_dob_non_na <= 1 ~ as.character(dr2_child_id),        # scenarios 1 & 3
      !is.na(dr2_dob) ~ paste0(dr2_child_id, "_", dob_rank),     # scenario 2 & non-NA part of 4
      TRUE ~ paste0(dr2_child_id, "_UNK")                    # NA rows in scenario 4
    ),
    
    # ID-level messy flag (scenario 4)
    messy_id = n_dob_non_na > 1 & any(is.na(dr2_dob))
  ) %>%
  ungroup()

length(unique(cleaned_id_data$unique_child_id)) # 44,137 instead of 44,035 

# Checks
cleaned_id_data %>%
  dplyr::filter(str_detect(unique_child_id, '_')) %>%
  dplyr::select(dr2_local_authority, dr2_month_return, dr2_referral_date,
                dr2_dob, dr2_gender_clean, unique_child_id, messy_id) %>%
  nrow() # 298 rows with a changed ID

cleaned_id_data %>%
  dplyr::filter(str_detect(unique_child_id, '_')) %>%
  dplyr::distinct(unique_child_id) %>%
  nrow() # 203 unique children that have had their IDs changed

## Identify sample population -----------------------------------------------------

# OUTCOME DATA CHECKS
cleaned_id_data %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(sum(is.na(dr3_start_date_of_cla_period_of_care_start_date)))

cleaned_id_data %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(n())

# Sample population: 
# Children aged 0 to 12 
# referred to CSC during the trial period
# Trial period: 1st March 2020 to 24th November 2022
# 2020-03-01 to 2022-11-24

#### Referral period ----
# Keep referrals during study period only
trial_period_data = cleaned_id_data %>% 
  dplyr::ungroup() %>%
  dplyr::filter(
    dr2_referral_date >= '2020-03-01' & dr2_referral_date <= '2022-11-24')

# check number of children in each
length(unique(cleaned_id_data$unique_child_id)) # 44,137
length(unique(trial_period_data$unique_child_id)) # 37,712

#### Age range ----

# Define the age parameters: what values to filter on? 
# (1) children aged 0-12 
# (2) but also, keep children with no birth dates bc they might be unborn 

# CHECKS  before filtering on age range

##### Check age distribution ----
# Check age distribution of children
# referred during trial period
age_distribution_tb = trial_period_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  dplyr::group_by(dr2_age_at_referral_clean) %>%
  dplyr::summarise(total_children = n())

# -1 to -2, 122, and NAs could be unborn children
# there are  2,372 children with NAs, values <0 or 122 
# there are 276 children with missing age 

##### Check unborn characteristics ----

# Check characteristics for these children with age = NA, <0 or 122
trial_period_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  dplyr::filter(dr2_age_at_referral_clean %in% c('-1', '-2', '122') | is.na(dr2_age_at_referral_clean)) %>%
  dplyr::group_by(dr2_gender_clean) %>%
  dplyr::summarise(n()) %>%
  dplyr::summarise(sum(`n()`)) # 2,372 unique children

# Are NAs in DOB unborn children? 
trial_period_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  dplyr::filter(is.na(dr2_age_at_referral_clean)) %>%
  dplyr::group_by(dr2_gender_clean, dr2_ethnicity_clean) %>%
  dplyr::summarise(n())

# Are values <0 or 122 in DOB unborn children? 
trial_period_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  #dplyr::filter(dr2_age_at_referral_numeric_clean < 0 | dr2_age_at_referral_numeric_clean > 100) %>%
  dplyr::filter(dr2_ethnicity_clean == 'Information not yet obtained') %>%
  dplyr::group_by(dr2_age_at_referral_clean, dr2_gender_clean) %>%
  dplyr::summarise(n()) %>%
  View()

# Bit of a complex pattern, NA in DOB could indicate being unborn, or genuinely missing DOB
# Most records seem to be unborn children
# I.e., age is NA, gender is 'unborn', or ' Unborn OR not stated or recorded', and 
# ethnicity is 'information not obtained'

##### Keep unborn & 0-12 year-olds ----

# FILTER AGE RANGE 
# For now, to keep referrals when children were aged 0 to 12, including NAs, values < 0 and 122
primary_sample_data = trial_period_data %>%
  dplyr::filter(
      dr2_age_at_referral_numeric_clean < 13 | # you can be 12.99 and still be 12
        dr2_age_at_referral_numeric_clean > 100 |
        is.na(dr2_age_at_referral_numeric_clean))

##### Quality assurance ----

# PRE FILTERING NUMBERS
# Nb unique children pre filtering
children_referred_during_trial_period = trial_period_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  nrow()

children_not_in_age_range = trial_period_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>% 
  dplyr::filter(dr2_age_at_referral_numeric_clean >= 13 & dr2_age_at_referral_numeric_clean <= 100) %>%
  nrow()

children_referred_during_trial_period - children_not_in_age_range 
# 27,378 unique children who are 0-12 or possibly unborn (NA DOB, age at referral = 122 and values <0)

# POST FILTERING NUMBERS
# Nb unique children post filtering
primary_sample_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>% 
  nrow() # 27,425 unique children 

View(primary_sample_data)

# 47 different than expected... 
# Probably bc they were referred at 2 different ages, on the cusp of turning 13 
# Check for this 
trial_period_data %>%
  dplyr::group_by(unique_child_id) %>%   # replace with your group column, e.g. city
  dplyr::filter(any(dr2_age_at_referral_numeric_clean > 13) & any(dr2_age_at_referral_numeric_clean < 13)) %>%
  dplyr::ungroup() %>%
  #dplyr::select(dr2_local_authority, dr2_month_return, dr2_referral_date,
  #               dr2_dob, dr2_age_at_referral_numeric_clean, dr2_gender_clean, unique_child_id, messy_id) %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  nrow() # 447 children 

# QA AGE DISTRIBUTION
# Now check age distribution 
age_distribution_updated_tb = primary_sample_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  dplyr::group_by(dr2_age_at_referral_clean) %>%
  dplyr::summarise(total_children = n()) 

# min and max age
primary_sample_data %>%
  dplyr::ungroup() %>%
  dplyr::summarise(
    min(dr2_age_at_referral_numeric_clean, na.rm = TRUE), # -1.14
    max(dr2_age_at_referral_numeric_clean, na.rm = TRUE)) # 122.8 

# TOTAL NB OF CHILDREN IN SAMPLE
nrow(primary_sample_data) # 38,262
length(unique(primary_sample_data$unique_child_id)) 
# 27,425 unique children
# of which 2,372 children have a DOB that could indicate they are unborn 
# 8.64% of sample

count_children_per_la = primary_sample_data %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(total_children = n())

#dr2_local_authority total_children
# lancashire                   10947
# swindon                       4345
# telford                       2829
# walsall                       5468
# wandsworth                    3836

# Some of these children are unborn, 
# or could be children without a DOB and either meeting sample inclusion criteria
# or not meeting sample inclusion criteria

# UNBORN DISTRIBUTION CHECKS
count_unborn_children_per_la = primary_sample_data %>%
  dplyr::filter(dr2_age_at_referral_clean %in% c('-1', '-2', '122') | is.na(dr2_age_at_referral_clean)) %>%
  dplyr::distinct(unique_child_id, .keep_all = TRUE) %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(total_unborn_children = n())

#dr2_local_authority total_children
# lancashire                    1013
# swindon                        323
# telford                        320
# walsall                        418
# wandsworth                     331

total_children_incl_unborn_per_la = dplyr::left_join(
  count_children_per_la, count_unborn_children_per_la,
  by = join_by('dr2_local_authority'))

total_children_incl_unborn_per_la %>%
  dplyr::mutate(percent_unborn = (total_unborn_children/total_children)*100) 
# Between 7.64% (swindon) and 11.3% (telford)

#dr2_local_authority total_children total_unborn_children percent_unborn
# lancashire                   10947                  1013           9.25
# swindon                       4345                   323           7.43
# telford                       2829                   320           11.3 
# walsall                       5468                   418           7.64
# wandsworth                    3836                   331           8.63

# OUTCOME DATA CHECKS
primary_sample_data %>%
 dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(sum(is.na(dr3_start_date_of_cla_period_of_care_start_date)))

primary_sample_data %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(n())

## Missingness analysis ---------------------------------------------------------

#### Total NAs ----

print_percent_nas = function(data){
  
  total_cells = dim(data)[1]*dim(data)[2]
  
  print(paste(round(sum(is.na(data))/total_cells*100, 2), '% cases are missing')) 
}

# On entire dataset
print_percent_nas(data = primary_sample_data) # "25.4 % cases are missing"

# on cols needed for analysis
analysis_cols = primary_sample_data %>%
  dplyr::select(contains('clean'), contains('date')) 

print_percent_nas(data = analysis_cols)  # "10.99 % cases are missing"

# without care start date
analysis_no_care_date_cols = analysis_cols %>%
  dplyr::select(-dr3_start_date_of_cla_period_of_care_start_date) 

print_percent_nas(data = analysis_no_care_date_cols)  # "3.62 % cases are missing"

#### NAs per column ----

colSums(is.na(primary_sample_data))

get_nas_per_column = function(data) {
  
  dplyr::tibble(
    column = names(data),
    missing = colSums(is.na(data)),
    percent_missing = round(missing/nrow(data) * 100, 2)
  )}

nas_per_col_table = get_nas_per_column(primary_sample_data)

#### NAs per column per LA ----
la_string = unique(
  primary_sample_data$dr2_local_authority)

names(la_string) = la_string

nas_per_la_table = purrr::map_dfr(la_string, \(la){
  
  data = primary_sample_data %>%
    dplyr::filter(dr2_local_authority == la)
  
  nas_per_col_table = get_nas_per_column(data)
  
}, .id = 'la')

# Save data -----
saveRDS(primary_sample_data, file = paste0(
  output_path,"/primary_outcome_sample_raw.Rds")) 
