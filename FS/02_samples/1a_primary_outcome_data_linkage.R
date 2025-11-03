#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## DATA CLEANING: DATA RETURN 3 ----

## 1. Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets')
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

### Load data ---- 
setwd(data_path)

dr2_data = readRDS(file = paste0(
  data_path,"/cleaning/DR2/DR2_pre_processed_data.Rds")) 

dr3_data = readRDS(file = paste0(
  data_path,"/pre_processing/DR3/DR3_pre_processed_data.Rds")) 

dr1_data = readRDS(file = paste0(
  data_path,"/cleaning/DR1/DR1_cleaned_data.Rds")) 

### Link data: DR2 to DR3 --- 

# Check number of unique child IDs in each returns
length(unique(dr2_data$child_id)) # 44,035
length(unique(dr2_data$child_id)) # 37,772

linked_dr2_dr3_data = dr2_data %>% 
  dplyr::group_by(local_authority, month_return) %>% 
  dplyr::left_join(
    ., dr3_data, by = c('local_authority','child_id', 'month_return'),
    relationship = 'many-to-many') 

# Check row number
nrow(dr2_data) # 64,009
nrow(linked_dr2_dr3_data) # 64,009

### Link data: DR1 to DR2-DR3 

# DR1 should link to DR2 by LA and month & year of referral of the child 
# Need to create a month_referral var first
linked_dr2_dr3_data = linked_dr2_dr3_data %>%
  dplyr::mutate(
    month_referral = lubridate::month(referral_date),
    year_referral = lubridate::year(referral_date),
    month_year_referral = as.Date(
      paste0(year_referral,'-', month_referral, '-01'),
      format = '%Y-%m-%d')) %>%
  dplyr::select(- month_referral, - year_referral) %>%
  dplyr::relocate(month_year_referral, .after = 'referral_date')
      
# Link records 
linked_all_dr_data = linked_dr2_dr3_data %>% 
  dplyr::group_by(local_authority, month_year_referral) %>% 
  dplyr::left_join(
    ., dr1_data, by = join_by( 
      'local_authority' == 'local_authority',
      'month_year_referral' == 'month_year_dr1')) 

