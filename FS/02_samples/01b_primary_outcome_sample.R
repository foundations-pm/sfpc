#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## ANALYSIS: PRIMARY OUTCOME SAMPLE ----

## Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/cleaning/All cleaned DR linked')
output_path = paste0(sharepoint_path, '/Datasets/samples')

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

## Load data --------------------------------------------------------------------
setwd(data_path)

data = readRDS(file = paste0(
  data_path, "/all_dr_cleaned_data.Rds")) 

## Define sample population -----------------------------------------------------

# Children aged 0 to 12 
# referred to CSC during the trial period
# Trial period: 1st March 2020 to 24th November 2022
# 2020-03-01 to 2022-11-24

# check NAs in referral date 
# to assess whether we're dropping NAs at this tage
sum(is.na(data$referral_date)) # 7 NAs 

data %>% 
  dplyr::filter(is.na(referral_date)) %>%
  View() 
# all from Wandsworth, 1 in april 2023 and 6 in Nov 22 return
# dropping these NAs - neligeable numbers 

# Keeping children referred during study period only
trial_period_data = data %>% 
  dplyr::ungroup() %>%
  dplyr::filter(
    referral_date >= '2020-03-01' & referral_date <= '2022-11-24')

# check number of children in each
length(unique(data$child_id)) # 44,035
length(unique(trial_period_data$child_id)) # 37,634

# Check age distribution 
data %>%
  dplyr::group_by(age_at_referral_clean) %>%
  dplyr::summarise(n()) %>%
  View()

# Check number of NAs 
sum(is.na(data$year_and_month_of_birth_of_the_child)) # 308 missing 
# these could be unborn children

# keep children aged 0 to 12, including Nas and
# other records that indicate a child may be unborn at the time of
# the referral
# using numeric var for age 
# keeping negative values for unborn children who might've been referred
# to CSC before they were born 

# Create a numeric var for age 
# rounded at 2 decimal
primary_sample_data = trial_period_data %>%
  dplyr::mutate(
    age_at_referral_numeric_clean = 
      as.numeric(
        round(
          (referral_date - year_and_month_of_birth_of_the_child)/365.25, 2
        )
      ),
  ) %>% 
  dplyr::relocate(
    age_at_referral_numeric_clean,
    .after = 'age_at_referral_clean')

sum(is.na(primary_sample_data$age_at_referral_numeric_clean)) #308

# Filter based on this numeric var 
# Keeping age > 100 - these records are possibly unborn children
# There are 13 records with age = 122 or 123
# Keeping until I investigate whether they're unborn 
primary_sample_data = primary_sample_data %>%
  dplyr::filter(
      age_at_referral_numeric_clean <= 12 |
        age_at_referral_numeric_clean > 100 |
        is.na(age_at_referral_numeric_clean))

# checks 
sum(
  is.na
    (primary_sample_data$year_and_month_of_birth_of_the_child)) #308

primary_sample_data %>%
  dplyr::ungroup() %>%
  dplyr::summarise(
    min(age_at_referral_numeric_clean, na.rm = TRUE), # -.538
    max(age_at_referral_numeric_clean, na.rm = TRUE)) # 123

# Dimensions and nb of children
nrow(primary_sample_data) #39,369
length(unique(primary_sample_data$child_id)) # 27,586 unique children

count_children_per_la = primary_sample_data %>%
  dplyr::distinct(child_id, .keep_all = TRUE) %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(total_children = n())

# local_authority total_children
#1 lancashire               10,138
#2 swindon                   3,999
#3 telford                   3,133
#4 walsall                   5,112
#5 wandsworth                5,204

## Missingness analysis ---------------------------------------------------------

### Total NAs ----

print_percent_nas = function(data){
  
  total_cells = dim(data)[1]*dim(data)[2]
  
  print(paste(round(sum(is.na(data))/total_cells*100, 2), '% cases are missing')) 
}

# On entire datasets
print_percent_nas(data = primary_sample_data) # 28.46 %

# on cols needed for analysis
analysis_cols = primary_sample_data %>%
  dplyr::select(contains('clean'), contains('date')) 

print_percent_nas(data = analysis_cols)  # 14.43%

# without care start date
analysis_no_care_date_cols = analysis_cols %>%
  dplyr::select(-start_date_of_cla_period_of_care_start_date) 

print_percent_nas(data = analysis_no_care_date_cols)  # 6.65%

### NAs per column ----

colSums(is.na(primary_sample_data))

nas_per_column = dplyr::tibble(
  column = names(primary_sample_data),
  missing = colSums(is.na(primary_sample_data)),
  percent_missing = round(missing/nrow(primary_sample_data) * 100, 2)
)

### NAs per column per LA ----

### NAs per column, LA and data return ----

local_authority_string = unique(
  primary_sample_data$local_authority)


## Derive analytical variables ---------------------------------------------------

### 1. Trial periods ----




### 2. End of study period (eosp) ----

### 3. Treatment assignment ----

### 4. Exposure time t ----

### 5. Primary outcome ----

### 6. Cross-contamination checks ----

### 7. Rate of CLA/CIN/CPP per 10,000 ----

### 8. Proportion White British ----

### 9. Proportion FSM ----

### 10. QA Filters ----

## Save dataset ----

