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

### Load data ---- 
setwd(data_path)

data = readRDS(file = paste0(
  data_path,"/DR1_pre_processed_data.Rds")) 

### 1. Clean records ----

# Add a standardised 01-mm-yyyy column c
# called month_year_dr1
data = data %>% 
  dplyr::mutate(
    month_only = lubridate::month(month),
    year_only = lubridate::year(month),
    month_year_dr1 = as.Date(
      paste0(year_only,'-', month_only, '-01'),
      format = '%Y-%m-%d')) %>%
  dplyr::select(- month_only, - year_only) %>% 
  dplyr::relocate(month_year_dr1, .after = 'month')

# Check min and max month supplied by each LA 
data %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(
    min_month = min(month_year_dr1, na.rm = TRUE),
    max_month = max(month_year_dr1, na.rm = TRUE),
    total_month = n())
# Overall good coverage, some months are missing:
# Between 2023-03-01 and 2023-09-01 for Telford and Wandsworth
# But these months are not needed in analyses 
# Coverage needed is 31st March 2020 to 24th November 2022 (trial period)

# Check there is 1 value per month for each LA 
data %>% 
  dplyr::group_by(local_authority, month_year_dr1) %>% 
  dplyr::summarise(n()) %>%
  View() 
# Lancashire has many double records for each months 
# only 1 of these monthly records is populated with a value

# Create a is.duplicated column 
is_duplicated = data %>% 
  dplyr::group_by(local_authority, month_year_dr1) %>%
  dplyr::summarise(is_duplicated = n()) 

# Left join to DR1 to identify the duplicated columns
data = data %>%
  dplyr::left_join(., is_duplicated, by = c(
    'local_authority', 'month_year_dr1')) %>% 
  dplyr::relocate(is_duplicated, .after = 'month_year_dr1') %>%
  dplyr::arrange(local_authority, is_duplicated, month_year_dr1) 

# Discard NA columns amongst duplicated records only
data = data %>%
  dplyr::group_by(is_duplicated) %>%
  dplyr::filter(!is.na(number_of_assessments_completed_by_csc)) %>%
  dplyr::arrange(local_authority, is_duplicated, month_year_dr1) 
  
# Check number of unique months again
data %>%
  dplyr::group_by(local_authority) %>%
  dplyr::summarise(
    min_month = min(month_year_dr1, na.rm = TRUE),
    max_month = max(month_year_dr1, na.rm = TRUE),
    total_month = n()) 
# all fixed, all LAs have 42 months; from 2019-10-01 to 2023-03-01
# Meaning removing NAs from number_of_assessments_completed_by_csc did not 
# Remove missing values for months that were not duplicated 
  
# drop the duplicated column - not needed
data = data %>%
  dplyr::ungroup() %>%
  dplyr::select(-is_duplicated)

### 2. Save data ----
saveRDS(data, file = paste0(
  output_path,"/DR1/DR1_cleaned_data.Rds")) 
