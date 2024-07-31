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

# DATA LINKAGE ----

## 1. Derive eligible population from DR2 ----

# Eligibility criteria:
# 1) Referred to CSC during trial period: 01/10/2019 to 31/03/2022
# 2) aged [12;17] during first referral
## TO THINK ABOUT: censoring/dosage ----
# Those who turn 18 during trial period: can no longer be at risk of CLA

# Check missingness
nrow(dr2_referrals)
sapply(dr2_referrals, function(x) sum(is.na(x)))

### Step 0: missing/incoherent DOB & referral dates ----
# or how many records are DOB > referral

# missing
sum(
  is.na(
    dr2_referrals[[
      'year_and_month_of_birth_of_the_child']])) # 244

# incoherent
dr2_referrals %>% filter(
  year_and_month_of_birth_of_the_child >= referral_date) %>%
  nrow() #2,191 

# 5.53% missing total; 
# 244 ppl (0.5%): missing amongst the possibly eligible population

### Step 1: find out age at referral ----
## TO THINK ABOUT: impact of deriving age at the start of the month ----
date_cols = dr2_referrals %>% 
  select(
    year_and_month_of_birth_of_the_child,
    contains('date')) %>%
  colnames()

dr2_referrals = dr2_referrals %>%
  dplyr::filter( 
    !is.na(year_and_month_of_birth_of_the_child), # remove missing age values
    year_and_month_of_birth_of_the_child < referral_date) %>% # remove incoherent values
  dplyr::mutate( # standardise date format
    across(.cols = all_of(date_cols),
           .fns = as.Date)) %>%
  dplyr::mutate(
    age_at_referral = floor(
      eeptools::age_calc(
        dob = as.Date(
          year_and_month_of_birth_of_the_child),
        enddate = as.Date(
          referral_date),
        units = "years"))
    ) 

### Step 2: find out date turned 18 ----
## TO THINK ABOUT: impact of deriving age at the start of the month ----
dr2_referrals = dr2_referrals %>%
 dplyr::mutate(
    date_turned_18 = as.Date( # add 18 years to DOB
      year_and_month_of_birth_of_the_child) %m+% years(18))  

### Step 3: rank referrals ----
dr2_referrals = dr2_referrals %>%
  dplyr::group_by( # group by child
    child_id) %>%
  dplyr::arrange( # sort by referrals by date, from oldest to latest
    referral_date) %>% 
  dplyr::mutate( 
    referral_number = dplyr::dense_rank(referral_date)) %>% # assign a rank
  dplyr::ungroup()

### Step 4: Define 18 months post referral  ----
## TO CHECK WITH OANA WHEN MONTH CALC SHOULD START ----
# = month referred == month 0 or 1? 
dr2_referrals = dr2_referrals %>%
 dplyr::mutate( # eosp = end of study period
    eosp = as.Date( # add 18 months to DOB
      referral_date) %m+% months(17))

### Step 5: Define time at risk t ----
# Exposure time:
# Let End of study period (EOSP) be referral date + 18 months for each child
# (1) for those who did not turn 18 before EOSP:
# t = EOSP - referral_date
# (2) for those who turned 18 before EOSP:
# t = date_turned_18 - referral_date
dr2_referrals = dr2_referrals %>%
  dplyr::mutate( # create a dummy for whether child turned 18 within study period
    is_censored = ifelse(
      date_turned_18 <= eosp, 1,0),
    t = case_when(
      is_censored == 1 ~ as.numeric(as.Date(date_turned_18) - as.Date(referral_date)),
      is_censored == 0 ~ as.numeric(as.Date(eosp) - as.Date(referral_date))))

### Step 6: Define eligible population ----

# Define trial period
trial_period = seq(from = as.Date('2019-10-01'),
                   to =  as.Date('2022-03-31'), 
                   by = "day")

# Eligible population:
# 1 - referrals within trial period
# 2 - age 12 to 17 at referral 
# (3 - first referral only (will filter as a last step below))
dr2_referrals = dr2_referrals %>%
  mutate(
    eligibility = ifelse(
      referral_date %in% trial_period & # referral date is within trial period 
        age_at_referral %in% c(12:17), # age when referred is between 12 and 17
      1, 0))

# checks 
dr2_referrals %>% filter(
  referral_date %in% trial_period & 
    age_at_referral %in% c(12:17) &
    referral_number == 1) %>% 
  nrow()

# Arrange / clean up dataset
dr2_referrals = dr2_referrals %>% 
  dplyr::relocate(
    local_authority, month_return,
    referral_id_or_case_id, child_id,
    referral_date, referral_number, eosp,
    year_and_month_of_birth_of_the_child, age_at_referral, date_turned_18,
    is_censored, t, eligibility) %>%
  dplyr::arrange(
    local_authority, child_id, desc(referral_number))

# Check missingness
nrow(dr2_referrals)
sapply(dr2_referrals, function(x) sum(is.na(x)))

### Step 7: Checks ----

#### Check 1: QA duplicated 1st referrals with dense_rank() ----
# Check nb of duplicate 1st referrals 
dr2_referrals %>% 
  filter(referral_number == 1) %>% 
  nrow() # 32,484 first referrals

length(
  unique(
    dr2_referrals$child_id)) # 32,153 unique children

#### Check 2: Investigate dense_rank() duplicates ----
# check whether we can drop any rows and use the row_number() method
duplicated_referrals = dr2_referrals %>% 
  group_by(child_id, referral_id_or_case_id) %>% 
  mutate(duplicate_referrals = any(duplicated(referral_date))) %>%
  ungroup() %>%
  filter(duplicate_referrals == TRUE,
         referral_number == 1) 

# Most of the duplicates are from Leicester: 618/626
duplicated_referrals %>%
  group_by(local_authority) %>%
  summarise(n())
# Drop duplicates OK: will use row_number as rank

#### Check 3: QA eligibility filter using row_number() ----

# Re-define row ranks using the row_number method
# > gives unique rank even to duplicates
# Use this dataset to select referral_number == 1 to describe eligible population
eligible_population = dr2_referrals %>% 
  dplyr::group_by(child_id) %>%
  dplyr::arrange(referral_date) %>% 
  dplyr::mutate(referral_number = row_number(referral_date)) %>% 
  dplyr::ungroup() 

qa_eligibility = eligible_population %>%
  mutate(
    is_referred_in_trial = ifelse(referral_date %in% trial_period, 1,0),
    is_of_age = ifelse(age_at_referral %in% c(12:17), 1, 0))

qa_eligibility %>% 
  filter(is_referred_in_trial == 1 &
         is_of_age == 1) %>%
  nrow() # 14,325 referrals are eligible

qa_eligibility %>% 
  filter(is_referred_in_trial == 1 &
         is_of_age == 1) %>%
  distinct(child_id) %>%
  nrow() # 11,307 unique children are eligible

qa_eligibility %>% 
  filter(referral_number == 1 & 
           is_referred_in_trial == 1 &
           is_of_age == 1) %>%
  distinct(child_id) %>%
  nrow() # 10,871 unique children are included in the trial analysis

## SAMPLE FRAME DESCRIPTIVES ----

# 1) Total children eligible out of sample frame 
eligible_population %>%  
  filter(referral_number == 1) %>% 
  group_by(eligibility) %>% 
  summarise(n())

# ISSUE 1: sample size -----
# 10,871 children are eligible 
# Protocol states 18,000 

# by LA:
eligible_population %>% 
  filter(referral_number == 1) %>% 
  group_by(local_authority, eligibility) %>% 
  summarise(n())

# Norfolk: 4,392
# Redcar: 1,173
# Rochdale: 2,219
# Warrington: 1,206

# Protocol states 3,600 average cluster size
# to check CYP population within each LA

# 2) Mean nb referrals for eligible children 

eligible_population %>% 
  filter(eligibility == 1) %>% 
  summarise(mean = mean(referral_number),
            median = quantile(referral_number, .5),
            sd = sd(referral_number),
            min = min(referral_number),
            max = max(referral_number))

eligible_population %>% 
  filter(eligibility  == 1) %>% 
  group_by(referral_number) %>%
  summarise(count = n())

# 3) Total nb of children turning 18 during study period 

eligible_population %>% 
  filter(referral_number == 1 & eligibility == 1) %>%
  group_by(is_censored) %>%
  summarise(count = n())

# 2,127 children turned 18 during the study period 
# 19% of sample

# excluding Leicester:
eligible_population %>% 
  filter(local_authority != 'leicester',
         referral_number == 1 & eligibility == 1) %>%
  group_by(is_censored) %>%
  summarise(count = n()) # still 19%

# 4) Mean, sd, min, max time at risk 

# Amongst those turning 18, check distribution of time at risk
eligible_population %>% 
  filter(local_authority != 'leicester',
         referral_number == 1 & eligibility == 1,
         is_censored == 1) %>%
  summarise(mean = mean(t),
            median = quantile(t, .5),
            sd = sd(t),
            min = min(t),
            max = max(t)) 

# time at risk during study period = 518 days
# mean t = 256 (half the study period)

# Amongst those who did not turn 18, check distribution of time at risk
eligible_population %>% 
  filter(local_authority != 'leicester',
         referral_number == 1 & eligibility == 1,
         is_censored == 0) %>%
  summarise(mean = mean(t),
            median = quantile(t, .5),
            sd = sd(t),
            min = min(t),
            max = max(t)) # looks good 

# 4) NAs amongst eligible population
eligible_population %>% 
  filter(local_authority != 'leicester',
         referral_number == 1 & eligibility == 1,
         is_censored == 0) %>%
  sapply(., function(x) sum(is.na(x)))


## 2. Link data: DR2+DR3 ----

### Step 1: derive analytical population ----
analytical_dataset_dr2 = eligible_population %>% 
  filter(
    local_authority != 'leicester', # drop Leicester
    referral_number == 1 & # select 1st referral 
      eligibility == 1) 

# eligibility = referral date within study period 
# + age at referral is 12 to 17

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


