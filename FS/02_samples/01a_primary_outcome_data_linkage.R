#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#

## DATA CLEANING: DATA RETURN 3 ----

r_directory = 'C:/Users/PerrineMachuel/'

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

## 1. Set-up  ----
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Datasets')
output_path = paste0(sharepoint_path, '/Datasets/cleaning')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

## 2. Load data ---- 
setwd(data_path)

dr2_data = readRDS(file = paste0(
  data_path,"/cleaning/DR2/DR2_cleaned_data.Rds")) 

dr3_data = readRDS(file = paste0(
  data_path,"/pre_processing/DR3/DR3_pre_processed_data.Rds")) 

dr1_data = readRDS(file = paste0(
  data_path,"/cleaning/DR1/DR1_cleaned_data.Rds")) 

## 3. Link data ----

### DR2 to DR3 ----

# Check number of unique child IDs in each returns
length(unique(dr2_data$dr2_child_id)) # 44,035
length(unique(dr3_data$dr3_child_id)) # 37,772

# OUTCOME DATA CHECKS
dr3_data %>%
  dplyr::group_by(dr3_local_authority) %>%
  dplyr::summarise(sum(!is.na(dr3_start_date_of_cla_period_of_care_start_date)))

dr3_data %>%
  dplyr::group_by(dr3_local_authority) %>%
  dplyr::summarise(n())

linked_dr2_dr3_data = dr2_data %>% 
  dplyr::left_join(
    ., dr3_data, 
    by = join_by(
      'dr2_local_authority' == 'dr3_local_authority',
      'dr2_child_id' == 'dr3_child_id'), # linking unique IDs within LAs
    relationship = 'many-to-many') 
# Many to many relationship:
# > allowing 1 child in dr2 to have multiple care entry dates in dr3 (duplicates rows for this child in the linked data)
# > allowing 1 child in dr3 (with a single care entry recorded) to have multiple referrals in dr2 

# Check row number
nrow(dr2_data) # 64,009
nrow(linked_dr2_dr3_data) # 64,279 - because of the many to many relationship 

# OUTCOME DATA CHECKS
linked_dr2_dr3_data %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(sum(!is.na(dr3_start_date_of_cla_period_of_care_start_date)))

linked_dr2_dr3_data %>%
  dplyr::group_by(dr2_local_authority) %>%
  dplyr::summarise(n())


### DR1 to DR2-DR3 ----

# DR1 should link to DR2 by LA and month & year of referral of the child 
# Need to create a month_referral var first
linked_dr2_dr3_data = linked_dr2_dr3_data %>%
  dplyr::mutate(
    month_referral = lubridate::month(dr2_referral_date),
    year_referral = lubridate::year(dr2_referral_date),
    dr2_month_year_referral = as.Date(
      paste0(year_referral,'-', month_referral, '-01'),
      format = '%Y-%m-%d')) %>%
  dplyr::select(- month_referral, - year_referral) %>%
  dplyr::relocate(dr2_month_year_referral, .after = 'dr2_referral_date')
      
# Link records 
linked_all_dr_data = linked_dr2_dr3_data %>% 
  dplyr::left_join(
    ., dr1_data, by = join_by( 
      'dr2_local_authority' == 'dr1_local_authority',
      'dr2_month_year_referral' == 'dr1_month_std')) 
# Checks
nrow(linked_dr2_dr3_data) #64,279
nrow(linked_all_dr_data) #64,279

## 4. Save data ----
saveRDS(linked_all_dr_data, file = paste0(
  output_path,"/All cleaned DR linked/all_dr_cleaned_data.Rds")) 
