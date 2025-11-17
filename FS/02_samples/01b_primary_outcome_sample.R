#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## ANALYSIS: PRIMARY OUTCOME SAMPLE ----

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

## Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
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

## Define sample population -----------------------------------------------------

# Children aged 0 to 12 
# referred to CSC during the trial period
# Trial period: 1st March 2020 to 24th November 2022
# 2020-03-01 to 2022-11-24

# check NAs in referral date 
# to assess whether we're dropping NAs at this tage
sum(is.na(data$dr2_referral_date)) # 7 NAs 

data %>% 
  dplyr::filter(is.na(dr2_referral_date)) %>%
  View() 
# all from Wandsworth, 1 in april 2023 and 6 in Nov 22 return
# dropping these NAs - neligeable numbers 

# Keeping children referred during study period only
trial_period_data = data %>% 
  dplyr::ungroup() %>%
  dplyr::filter(
    dr2_referral_date >= '2020-03-01' & dr2_referral_date <= '2022-11-24')

# check number of children in each
length(unique(data$dr2_child_id)) # 44,035
length(unique(trial_period_data$dr2_child_id)) # 37,634

# Check age distribution 
data %>%
  dplyr::group_by(dr2_age_at_referral_clean) %>%
  dplyr::summarise(n()) %>%
  View()

# Check number of NAs 
sum(is.na(data$dr2_year_and_month_of_birth_of_the_child)) # 308 missing 
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
    dr2_age_at_referral_numeric_clean = 
      as.numeric(
        round(
          (dr2_referral_date - dr2_year_and_month_of_birth_of_the_child)/365.25, 2
        )
      ),
  ) %>% 
  dplyr::relocate(
    dr2_age_at_referral_numeric_clean,
    .after = 'dr2_age_at_referral_clean')

sum(is.na(primary_sample_data$dr2_age_at_referral_numeric_clean)) #309

# Filter based on this numeric var 
# Keeping age > 100 - these records are possibly unborn children
# There are 13 records with age = 122 or 123
# Keeping until I investigate whether they're unborn 
primary_sample_data = primary_sample_data %>%
  dplyr::filter(
      dr2_age_at_referral_numeric_clean <= 12 |
        dr2_age_at_referral_numeric_clean > 100 |
        is.na(dr2_age_at_referral_numeric_clean))

# checks 
sum(
  is.na
    (primary_sample_data$dr2_year_and_month_of_birth_of_the_child)) #309

primary_sample_data %>%
  dplyr::ungroup() %>%
  dplyr::summarise(
    min(dr2_age_at_referral_numeric_clean, na.rm = TRUE), # -.538
    max(dr2_age_at_referral_numeric_clean, na.rm = TRUE)) # 123

# Dimensions and nb of children
nrow(primary_sample_data) # 39,475
length(unique(primary_sample_data$dr2_child_id)) # 27,586 unique children

count_children_per_la = primary_sample_data %>%
  dplyr::distinct(dr2_child_id, .keep_all = TRUE) %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(total_children = n())

# dr2_local_authority total_children
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

print_percent_nas(data = analysis_cols)  # 13.81 %

# without care start date
analysis_no_care_date_cols = analysis_cols %>%
  dplyr::select(-dr3_start_date_of_cla_period_of_care_start_date) 

print_percent_nas(data = analysis_no_care_date_cols)  # 6.65%

### NAs per column ----

colSums(is.na(primary_sample_data))

get_nas_per_column = function(data) {
  
  dplyr::tibble(
    column = names(data),
    missing = colSums(is.na(data)),
    percent_missing = round(missing/nrow(data) * 100, 2)
  )}

nas_per_col_table = get_nas_per_column(primary_sample_data)

### NAs per column per LA ----
la_string = unique(
  primary_sample_data$dr2_local_authority)

names(la_string) = la_string

nas_per_la_table = purrr::map_dfr(la_string, \(la){
  
  data = primary_sample_data %>%
    dplyr::filter(dr2_local_authority == la)
  
  nas_per_col_table = get_nas_per_column(primary_sample_data)
  
  nas_per_col_table %>%
    dplyr::arrange(desc(percent_missing))
  
}, .id = 'la')

## Derive analytical variables ---------------------------------------------------

### 1. Trial periods ----

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

### 2. End of study period (eosp) ----
# For primary outcome == 18-months
primary_sample_data = primary_sample_data %>%
  dplyr::mutate( # eosp = end of study period
    eosp = as.Date( # add 18 months to referral date
      dr2_referral_date) %m+% months(18))

### 3. Treatment assignment ----
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

### 4. Primary outcome ----
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

### 6. Proportion White British ----

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

### 7. Proportion FSM ----

primary_sample_data %>% 
  dplyr::summarise(
    min(dr1_proportion_of_children_young_people_eligible_and_claiming_for_free_school_meals_for_all_school_age_children_and_young_people_in_the_la_out_of_all_pupils, na.rm = TRUE),
    max(dr1_proportion_of_children_young_people_eligible_and_claiming_for_free_school_meals_for_all_school_age_children_and_young_people_in_the_la_out_of_all_pupils, na.rm = TRUE)) 

primary_sample_data %>% 
  dplyr::filter(dr1_proportion_of_children_young_people_eligible_and_claiming_for_free_school_meals_for_all_school_age_children_and_young_people_in_the_la_out_of_all_pupils > 1) %>%
  nrow()

### 8. Rate of CLA/CIN/CPP per 10,000 ----

### 9. Keep first referral only ----

## Save dataset ----

