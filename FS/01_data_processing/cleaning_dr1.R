#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## DATA CLEANING: DATA RETURN 2 ----

## Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets/pre_processing/DR1')
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

## Load data ---- 
setwd(data_path)

data = readRDS(file = paste0(
  data_path,"/DR1_pre_processed_data.Rds")) 

## Clean data ----

### 01-mm-yyyy month column ----

# Add a standardised 01-mm-yyyy column c
# called month_std
data = data %>% 
  dplyr::mutate(
    month_only = lubridate::month(month),
    year_only = lubridate::year(month),
    month_std = as.Date(
      paste0(year_only,'-', month_only, '-01'),
      format = '%Y-%m-%d')) %>%
  dplyr::select(- month_only, - year_only) %>% 
  dplyr::relocate(month_std, .after = 'month')

### Drop duplicates ----

# Check min and max month supplied by each LA 
data %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(
    min_month = min(month_std, na.rm = TRUE),
    max_month = max(month_std, na.rm = TRUE),
    total_month = n())
# Overall good coverage, some months are missing:
# Between 2023-03-01 and 2023-09-01 for Telford and Wandsworth
# But these months are not needed in analyses 
# Coverage needed is 31st March 2020 to 24th November 2022 (trial period)

# Check there is 1 value per month for each LA 
data %>% 
  dplyr::group_by(local_authority, month_std) %>% 
  dplyr::summarise(n()) %>%
  View() 
# Lancashire has many double records for each months 
# only 1 of these monthly records is populated with a value

# Create a is.duplicated column 
is_duplicated = data %>% 
  dplyr::group_by(local_authority, month_std) %>%
  dplyr::summarise(is_duplicated = n()) 

# Left join to DR1 to identify the duplicated columns
data = data %>%
  dplyr::left_join(., is_duplicated, by = c(
    'local_authority', 'month_std')) %>% 
  dplyr::relocate(is_duplicated, .after = 'month_std') %>%
  dplyr::arrange(local_authority, is_duplicated, month_std) 

# Discard NA columns among duplicated records only
data = data %>%
  dplyr::group_by(is_duplicated) %>%
  dplyr::filter(!is.na(number_of_assessments_completed_by_csc)) %>%
  dplyr::arrange(local_authority, is_duplicated, month_std) 
  
# Check number of unique months again
data %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(
    min_month = min(month_std, na.rm = TRUE),
    max_month = max(month_std, na.rm = TRUE),
    total_month = n()) 
# all fixed, all LAs have 42 months; from 2019-10-01 to 2023-03-01
# Meaning removing NAs from number_of_assessments_completed_by_csc did not 
# Remove missing values for months that were not duplicated 
  
# drop the duplicated column - not needed
data = data %>%
  dplyr::ungroup() %>%
  dplyr::select(-is_duplicated)

### Rename columns ----

new_colnames = c(
  "nb_of_assessments_completed",                                                                                                                           
  "prop_cyp_eligible_and_claiming_for_fsm_out_of_all_pupils",         
  "cla_rate_per_10_000_children",                                                                                                                                     
  "nb_cla_at_the_end_of_the_month",                                                                                                
  "nb_newly_cla_after_this_month",                                                                                            
  "nb_cin_plans_started_this_month",                                                                                                            
  "nb_open_cin_cases_this_month",                                                                                                                    
  "nb_cpps_started_this_month",                                                                                                                
  "nb_open_cpps_this_month",                                                                                                                        
  "nb_of_new_referrals_this_month",                                                                                                                     
  "average_nb_cases_per_social_worker_fte_counts",                                                                                         
  "prop_all_plans_where_cyp_visiting_schedule_not_met",    
  "turnover_rate_caseholder_staff",                                                                                                     
  "prop_cin_plans_where_cyp_visiting_schedule_not_met",
  "prop_cpps_where_cyp_visiting_schedule_not_met", 
  "prop_cla_where_cyp_visiting_schedule_not_met",      
  "nb_open_cin_cases_this_month_all",                                                                                                                
  "nb_open_cin_cases_this_month_long_term",                                                                                                          
  "nb_open_cin_cases_this_month_narrow_definition_provided_june_2021",                                                                               
  "nb_open_cin_cases_this_month_wider_definition",                                                                                                   
  "nb_cin_plans_that_started_this_month_exc_cpp",                                                                                                   
  "nb_open_cin_cases_this_month_narrow_definition",                                                                                                 
  "nb_cin_plans_that_started_this_month_inc_cpp")

ncol(data) #27
# Rename columns
colnames(data)[5:27] = new_colnames

# Adjust col order for clarity
data = data %>% 
  dplyr::relocate(
  nb_cin_plans_that_started_this_month_exc_cpp,
  .after = c('nb_open_cin_cases_this_month_narrow_definition')) %>%
  dplyr::relocate(
    nb_open_cin_cases_this_month_narrow_definition, 
    .after = c('nb_open_cin_cases_this_month_long_term')) %>%
  dplyr::relocate(
    turnover_rate_caseholder_staff, 
    .after = c('average_nb_cases_per_social_worker_fte_counts'))

# append dr1 prefix to identify columns coming from dr1
colnames(data) <- paste0("dr1_", colnames(data))

## Save data ----
saveRDS(data, file = paste0(
  output_path,"/DR1/DR1_cleaned_data.Rds")) 
