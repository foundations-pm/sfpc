# Primary outcome analysos for No Wrong Doors RCT DR1 ----

# Paths  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 
                   'QA/processing/processed_data/')

output_path = paste0(sharepoint_path, 'QA/processing/linked_data/')

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries ----
{ source(paste0(wd, "config.R")) }

# Functions ----
{ source(paste0(wd, "functions.R"))}

# Load data ----

# DR1: LA-level characteristics
# DR2: baseline, referrals 
# DR3: outcome (whether in care or not)

dr1_path = paste0(data_path, "DR1/DR1_processed.xlsx")
dr1_data = readxl::read_excel(dr1_path)

dr2_path = paste0(data_path, "DR2/DR2_processed_referrals.xlsx")
dr2_referrals = readxl::read_excel(dr2_path)

dr3_path = paste0(data_path, "DR3/DR3_processed_cla.xlsx")
dr3_cla = readxl::read_excel(dr3_path)

# Derive eligible population from DR2 ----

# Those referred during trial period: 01/10/2019 to 31/03/2022
# Of those, those aged [12;17] during first referral
# TO THINK ABOUT: censoring/dosage ----
# variability for those who turn 18 during trial period

# Step 0: find out how many records are missing DOB 
nrow(dr2_referrals)
sapply(dr2_referrals, function(x) sum(is.na(x)))
# 244 missing values for ages

# Step 1: find out first referral
dr2_referrals = dr2_referrals %>%
  dplyr::group_by( # group by child
    child_id) %>%
  dplyr::arrange( # 
    referral_date) %>% 
  dplyr::mutate(referral_number = dense_rank(referral_date))

# Step 2: find out age at referral
dr2_referrals = dr2_referrals %>%
  dplyr::filter( 
    !is.na( # remove missing age values
      year_and_month_of_birth_of_the_child)) %>%
  dplyr::mutate(age_at_referral = eeptools::age_calc(
    dob = as.Date(year_and_month_of_birth_of_the_child),
    enddate = as.Date(referral_date),
    units = "years")) %>%
  dplyr::relocate(
    local_authority, month_return, referral_id_or_case_id,
    child_id, referral_date, referral_number,
    year_and_month_of_birth_of_the_child, age_at_referral) %>%
  dplyr::arrange(local_authority, child_id, desc(referral_number))

# step 2: derive eligible population
trial_period = seq(from = as.Date('2019-10-01'),
                   to =  as.Date('2022-03-31'), 
                   by = "day")

dr2_referrals = dr2_referrals %>%
  mutate(eligibility = ifelse(
    referral_date %in% trial_period &
    
    
  ))


# Link data ----

# DR2 to DR3 via LA, Child ID and Referral ID

# Left join: keep all baseline records, join outcome records 
linked_data = dplyr:left_join(
  dr2_referrals, dr3_cla,
  by = c("local_authority",
         "child_id",
         "referral_id_or_case_id")) 

nrow(linked_data)
nrow(dr2_referrals)
nrow(dr3_cla)

unique()

# Linkage rate: how many records in DR3 could be linked to DR2?


# DR2/3 to DR1 via LA 


