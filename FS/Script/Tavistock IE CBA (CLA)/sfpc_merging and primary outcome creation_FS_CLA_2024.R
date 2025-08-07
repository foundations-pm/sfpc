################################################################################
#
#                     Merging DR3 and DR2 (CLA) 2024 
#                               SFPC 
#                      Family Safeguarding Model 
#                       Tavistock Analysis
#                        Emily Walker 2024
#
################################################################################

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

# Script to merge the July 2024 DR3 CLA data return to DR2.
# For Tavistick CBA impact estimates for CLA primary outcome. 

################################################################################

# Clearing R -------------------------
# rm(list = ls())

# Loading libraries
library(tidyverse)
library(dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(data.table)
library(arsenal)
library(pacman)
library(naniar)
library(visdat)
library(VIM)
library(vcd)
library(cobalt)
library(tableone)

# Reading in the data 
# Read in DR1 ----
load ("Output/DR1_bind.RData")

# Reading in DR2 ----
load("Output/DR2_bind.RData")

# Reading in subset DR2
# This dataframe includes information only for the first referral for each child
# It includes a variable for 18 months after the first referral 
load("Output/subset_DR2_pre.RData")

# Reading in DR3 2024 (CLA)----
load("Output/DR3_24_bind_cla.RData")

################################################################################


# Merging DR2 and DR3 CLA ---------
cla_merge <- left_join(subset_dr2, all_dr3_cla_bind, by = "child la id")

# Exploring which DR3 ids appear to be not match with DR2 
# How many unique variables are there in DR2 which are not in DR3
missing_ids <- setdiff(subset_dr2$`child la id`, all_dr3_cla_bind$`child la id`)
length(missing_ids)#1
#Indicates there are 1 IDs in DR2 that do not have a match in DR3.

# How many unique variables are there in DR3 which are not in DR2.
missing_ids_2 <- setdiff(all_dr3_cla_bind$`child la id`, subset_dr2$`child la id`)
length(missing_ids_2)#12363

table(all_dr3_cla_bind$la)
table(subset_dr2$LA)

# Exploring what % of children from the sample (DR2) when into care. 
CLAtotal_observations <- length(cla_merge$`cla date`)
print(CLAtotal_observations)
# 25,527
# Count number of non-missing observations in the DateInCare variable
CLAnon_missing_observations <- sum(!is.na(cla_merge$`cla date`))
print(CLAnon_missing_observations)
# 1984

# Proportion of cohort that went into care 
(CLAnon_missing_observations/CLAtotal_observations)*100
# 7.772163

# Exploring the merged dataframe 
# Age 
age_counts <- table(cla_merge$age_group, useNA = "ifany")
age_percentage <- prop.table(age_counts)*100
print(age_percentage)
#  3 and under        4-12         unborn         <NA> 
#  27.92337525       62.53378775   9.49191053     0.05092647 

# Gender 
gender_counts <- table(cla_merge$gender1, useNA = "ifany")                              
gender_percentage <- prop.table(gender_counts) * 100
print(gender_percentage)
#     Male         Female       Unknown/unborn           <NA> 
#  50.142985858   46.053198574    3.795980726    0.007834842  

# Look at missingness
# Removing from running, as they take a while to run.
# Save as graphics instead 

#vis_dat(cla_merge)
#vis_miss(cla_merge)

# Visualize missingness in your dataframe
#aggr(cla_merge, col = c('navyblue', 'yellow'),
#          numbers = TRUE, sortVars = TRUE,
#     labels = names(cla_merge), cex.axis = .7,
#     gap = 3, ylab = c("Missing data", "Pattern"))

missing_ethnicity_by_la <- cla_merge %>%
  group_by(la) %>%                                  
  summarise(
    total = n(),                                                 
    missing = sum(is.na(ethnicity1)),                              
    missing_percent = (missing / total) * 100,                    
    unborn_in_missing = sum(is.na(ethnicity1) & unborn == 1),
    unborn_in_missing_percent = ifelse(missing > 0, (unborn_in_missing / missing) * 100, 0)
  )

ethnicity_unborn <- table(cla_merge$ethnicity1, cla_merge$unborn, useNA="ifany")
View(ethnicity_unborn)  

# View the result
View(missing_ethnicity_by_la)

mean(is.na(cla_merge$ethnicity1)) * 100 #6.722294
mean(is.na(all_dr2_bind$ethnicity)) *100 #5.392992

# Dropping the rows with only NA observations
cla_merge <- cla_merge[1:(nrow(cla_merge)-2),]

# How many children by LA
unique_child_ids_by_la <- cla_merge %>%
  group_by(la) %>%              
  summarise(unique_child_ids = n_distinct(`child la id`)) %>% 
  mutate(percentage = (unique_child_ids / sum(unique_child_ids)) * 100)

# View the result
unique_child_ids_by_la

################################################################################
#
#                Constructing primary outcome variable (CLA) 2024 
#                     From merged dataset DR2 and DR3 
#                               SFPC 
#                      Family Safeguarding Model 
#                       Tavistock Analysis
#                        Emily Walker 2024
#
################################################################################

# Constructing the primary outcome variable 
# Whether or not the child has become looked after
# measure: Coded 1 if the child has become looked after at any
# point within 18 months of the referral. Coded 0 if the
# child has not become looked after within this period.

cla_merge$primary_outcome <- ifelse(
  is.na(cla_merge$`cla date`), 
  0,
  ifelse(
    cla_merge$`cla date` <= cla_merge$`ref date 18months`,
    1,
    0
  )
)

table(cla_merge$primary_outcome)
#  0          1 
# 24022     1503  

# Create treatment/control variable ----

# Dates----
# Trial period began: March 2020
# Trial period ended: November 2022
# (subset to only include referal dates within this time period)

# Lanc go live: 01/02/2021 (1 Feb 2021)
# Swind go live: 24/05/2022 (24 May 2022)
# Telf go live: 28/06/2021 (28 June 2021)
# Wands go live: 24/01/22 (24 jan 2022)
# Walsall go live: 01/09/2020 (1 sept 2020)

# ASSIGNMENT INFO ----
# CONTROL: Children whose first referral in the trial period took place when
# the local authority was running their business as usual model.

# TREATMENT: Children whose first referral in the trial period took place when the
# local authority was running the Family Safeguarding model.

# randomise the order in which local authorities implement the programme, 
#in six month intervals, rather than which local authority implements the model.

# Create a dataframe of go-live dates----

# Define variables----
la <- c("Lancashire", "Swindon", "Telford", "Walsall", "Wandsworth")
go.live <- as.Date(c("2021-02-01", "2022-05-24", "2021-06-28", "2020-09-01", "2022-01-24"))

go_live <- data.frame(la, go.live)

print(go_live)

# Merging go live dates on to the main dataframe ----
cla_merge <- merge(cla_merge, go_live, by = c("la"), all.x = TRUE)


# Trial period began: March 2020
# Trial period ended: November 2022

# Creating the treatment/control variable----
# CLA
#cla_merge$treatment <- ifelse(cla_merge$`ref date1` >= cla_merge$`go.live`, 1, 0)

# Check the proportion of treatment and control 
cla_merge %>%
  select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins

#0      1        Sum 
#13214  12311    25525 


# How many children in the treatment entered care and how many in the control 

proportions <- tapply(cla_merge$primary_outcome, cla_merge$treatment, mean)
print(proportions)
#0            1 
#0.04926593   0.06920640

CLA_table <- table(Treatment = cla_merge$treatment, `Entered care` = cla_merge$primary_outcome)
# Calculate percentage of entering care by treatment group
percent_table <- prop.table(CLA_table, 1) * 100
print(percent_table)
# Extract the percentage for those who entered care (entering care = 1)
care_percentage <- percent_table[, 2]

# Table showing number and % of children who entered care by LA and by treatment status 
care_summary <- cla_merge %>%
  group_by(LA, treatment) %>%
  summarise(
    n_entered_care = sum(primary_outcome == 1, na.rm = TRUE),
    n_total = n(),
    pct_entered_care = 100 * n_entered_care / n_total
  ) %>%
  ungroup()

print(care_summary)

# Creating a table showing number and % for children who did NOT enter care by LA and treatmen status 
no_care_summary <- cla_merge %>%
  group_by(LA, treatment) %>%
  summarise(
    n_not_entered_care = sum(primary_outcome == 0, na.rm = TRUE),
    n_total = n(),
    pct_not_entered_care = 100 * n_not_entered_care / n_total
  ) %>%
  ungroup()

################################################################################

# Creating time periods.

# NOTE: change time period/wedge to go live date rather than arbitrary 6 months. 

#time: This represents the time periods after the baseline (as dummy variables).
#-	We consider the referral date to be the relevant date according to which the relevant time dummy is determined.
#-	A series of indicator variables adjusting for time trends by introducing dummy variables for each time after the baseline period t = 0.
#-	Does the baseline span from -6 months to 0? Or is it -6 months? Are there 6 time periods?
#  -	Plan: Create indicator variable (factor?) with time periods, split into: 

# First LA (Walsall) go live: 01/09/2020 (1 sept 2020)
# Baseline begins six months before this: 

#  1)	 -6 to 0 months; March 1 2020 - September 1 2020
#  2)   Walsall go live: 01/09/2020 (1 sept 2020)
#  3)   Lanc go live: 01/02/2021 (1 Feb 2021)
#  4)   Telf go live: 28/06/2021 (28 June 2021)
#  5)   Wands go live: 24/01/22 (24 jan 2022)
#  6)   Swindon go live: 24/05/2022 (24 May 2022)
#  7)   6 months after last LA goes live May 2022 - November 2022.


#Converting referral date to date rather than POX, to enable running the rest of
#the code.
cla_merge$`ref date1` <- as.Date(cla_merge$`ref date1`)

# Specify the start and end dates for each period
period_breaks <- as.Date(c("2020-03-01", "2020-09-01", "2021-02-01", 
                           "2021-06-28", "2022-01-24", "2022-05-24", 
                           "2022-11-30"))

# Create labels for each period
period_labels <- c(
  "Baseline",
  "Period 1",
  "Period 2",
  "Period 3",
  "Period 4",
  "Period 5"
)

# Assign the 'ref date1' values to the correct period based on the breaks
cla_merge$time_period <- cut(cla_merge$`ref date1`,
                             breaks = period_breaks,
                             labels = period_labels,
                             right = FALSE,  # Left-closed intervals [start_date, end_date)
                             include.lowest = TRUE)

# Checks and balances for merged dataset ----

# Proportion of children who entered care in period 1 across the LAs
proportion_in_care <- cla_merge %>%
  filter(time_period == 'Period 1') %>%                                
  group_by(la) %>%                                             
  summarise(
    total_children = n(),                                     
    went_into_care = sum(primary_outcome == 1),                
    proportion_in_care = (went_into_care / total_children) * 100 
  )

# Descriptives for unborn babies 
# Exploring descriptives: unborn babies 
ggplot(cla_merge, aes(x = la, fill = unborn)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ treatment) +
  labs(title = "Distribution of Unborn Status by LA and Treatment Group", x = "Local Authority (LA)", y = "Count") +
  theme_minimal()

# Exploring descriptives: unborn babies 
ggplot(cla_merge, aes(x = la, fill = unborn)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ primary_outcome) +
  labs(title = "Distribution of Unborn Status by LA and Primary outcome", x = "Local Authority (LA)", y = "Count") +
  theme_minimal()

# % visual 
prop_unborn_treat_tab <- cla_merge %>% group_by(la, treatment, unborn) %>% summarise(count = n()) %>% mutate(prop = round( count / sum(count), 2))

prop_unborn_tab <- cla_merge %>% group_by(la, unborn) %>% summarise(count = n()) %>% mutate(prop = round( count / sum(count), 2))

# Looking at missingness by LA
cla_merge %>% 
  gg_miss_var(show_pct = TRUE, facet = `la`)

# Exploring missingness
missing_percentage_by_LA <- cla_merge %>%
  group_by(la) %>%
  summarise(across(everything(), 
                   ~ mean(is.na(.)) * 100,  # Calculate the percentage of missing values
                   .names = "percent_missing_{col}")) %>%
  ungroup()

write.csv(missing_percentage_by_LA, file = "Output/missing_by_la.csv", row.names = FALSE)

#Disability 
table(cla_merge$disability1, cla_merge$unborn, cla_merge$LA, useNA="ifany")

# View the result
print(missing_percentage_by_LA)

# Cluster heterogeneity ----
#1 Check SMDs for different baseline demographics by LA
covariates = c(
  'la',
  'age_group',
  'gender1',
  'ethnicity1',
  'disability1',
  'uasc1',
  '`previous cpp1`',
  'unborn'
  #'cla_rate_per_10_000_children'
)

formula_la <- as.formula(
  paste(
    "la ~", 
    paste0('treatment + ', 
           paste(covariates[-1], collapse = " + "))))

balance_la <- cobalt::bal.tab(
  formula_la, 
  data = cla_merge, 
  estimand = "ATE", 
  continuous = 'std',
  s.d.denom = "pooled")

love.plot(balance_la,
          stats = "mean.diffs", 
          abs = TRUE,
          var.order = "unadjusted", 
          drop.missing = FALSE,
          stars = 'std',
          threshold = 0.05) +
  labs(title = paste0("Absolute Mean Differences",
                      " across Local Authorities"))


# Balance checks ----

# Standardized means differences 

#2 Method 2:

# Create a formula with covariates
formula_trt <- as.formula(
  paste("treatment ~",
        paste(covariates, collapse = " + ")))

# Use bal.tab() to calculate SMDs for covariates by treatment
balance_trt <- cobalt::bal.tab(
  formula_trt, 
  data = cla_merge, 
  estimand = "ATE", 
  s.d.denom = "pooled")

# Generate love plot for treatment group balance
love.plot(balance_trt,
          stat = "mean.diffs", 
          abs = TRUE,
          stars = 'std',
          var.order = "unadjusted", 
          threshold = 0.05) +
  labs(title = paste0("Absolute Mean Differences",
                      " by Treatment Group"))

save(cla_merge, file = "Output/cla_merge_pri_outcome.RData")


################################################################################

#Descriptives for analysis slide deck 
la_percentage <- cla_merge %>%
  group_by(la) %>%
  summarise(count = n()) %>%  # Count number of observations per LA
  mutate(percentage = (count / sum(count)) * 100)  # Calculate percentage

# View the result
print(la_percentage)

table(cla_merge$treatment)

sum(!is.na(cla_merge$`cla date`)) #1984

(sum(!is.na(cla_merge$`cla date`)) / nrow(cla_merge)) * 100 #7.772772

# Count the observations where primary outcome is 1
primary_outcome_count <- sum(cla_merge$primary_outcome == 1, na.rm = TRUE) 
print(primary_outcome_count) #1503

# Calculate the percentage
primary_outcome_percent <- (primary_outcome_count / nrow(cla_merge)) * 100 
print(primary_outcome_percent) #5.888345

# Baseline proportion
baseline_enter_care_by_la <- cla_merge %>%
  filter(time_period == "Baseline") %>%     # Filter for the baseline period
  group_by(la) %>%                          # Group by local authority (la)
  summarise(
    total_in_baseline = n(),                           # Total children in the baseline period for each LA
    entered_care = sum(primary_outcome == 1, na.rm = TRUE),  # Count of children who entered care (primary outcome == 1)
    entered_care_percent = (entered_care / total_in_baseline) * 100  # Proportion of children entering care
  ) %>%
  ungroup()  # Ungroup after summarization

# Proportion of outcome by time period 
primary_outcome_count_table <- cla_merge %>%
  group_by(la, time_period) %>%                                   # Group by LA and time period
  summarise(
    count = sum(primary_outcome == 1, na.rm = TRUE),             # Count where primary outcome == 1
    total = n()                                                   # Total number of observations in each group
  ) %>%
  mutate(proportion = count / total) %>%                         # Calculate the proportion of those who entered care
  select(la, time_period, count, proportion) %>%                 # Select only the required columns
  ungroup()  

View(primary_outcome_count_table)

# Table on missingness of ethnicity
missing_ethnicity_by_la <- cla_merge %>%
  group_by(la) %>%                                  
  summarise(
    total = n(),                                                 
    missing = sum(is.na(ethnicity1)),                              
    missing_percent = (missing / total) * 100,                    
  )

#Ethnicity missing 
mean(is.na(cla_merge$ethnicity1)) * 100 #6.714985
sum(is.na(cla_merge$ethnicity1))   # 1714

# Readiness for implementing 
# Tranche 1: Lancashire; Telford and Wreckin; Walsall; 
# Tranche 2: Wandsworth; Swindon 

# Creating variable for readiness 
cla_merge$readiness <- ifelse(
  cla_merge$la %in% c('Lancashire', 'Telford', 'Walsall'), 
  'High readiness', 'Low readiness')

# Randomisation checks ----

# Reflection
# Threat to randomisation:
# 'High readiness' = correlated with less chances of becoming CLA? 
# Very possible in this case

baseline_data <- subset(cla_merge, time_period == "Baseline")

contingency_table <- table(baseline_data$readiness, baseline_data$primary_outcome)

View(contingency_table)

chi_squared_result <- chisq.test(contingency_table)
print(chi_squared_result)

cramers_v <- assocstats(contingency_table)$cramer
print(cramers_v)  #0.04360591


################################################################################

# Merging DR1 CLA rate and CPP rate to DR2+DR3 merged dataframe 

load("Output/DR1_cpp_rate.RData")

cpp_rate_var <- cpp_rate %>% select(month, la, cpp_rate)

cla_rate_var <- all_dr1_bind %>% select(month, la, `cla rate`)

# filter for the correct dates 
cla_rate_var <- cla_rate_var[cla_rate_var$month >= as.Date("2020-03-01") & cla_rate_var$month <= as.Date("2022-11-30"), ]


# Merging CLA rate and CPP rate independently to main data frame 
cla_merge$month <- floor_date(cla_merge$`ref date1`, "month")

# CLA rate
merge_cla_rate <- merge(cla_merge, cla_rate_var, by = c("la", "month"), all.x = TRUE)
merge_cla_rate <- merge_cla_rate %>% select(-LA)

# CPP rate 
merge_cpp_rate <- merge(cla_merge, cpp_rate_var, by = c("la", "month"), all.x = TRUE)
merge_cpp_rate <- merge_cpp_rate %>% select(-LA)

# Saving the dataframe 
save(merge_cla_rate, file = "Output/merged_cla_rate.RData")
save(merge_cpp_rate, file = "Output/merged_cpp_rate.RData")


################################################################################

# Creating a variable for % White British 


# Add a new variable: percentage of White British by LA and Month, excluding missing Ethnicity
cla_merge <- cla_merge %>%
  mutate(
    white_british = ifelse(ethnicity1 == 'White British or Irish', 1, 0),
    white_british = ifelse(is.na(ethnicity1), NA, white_british)  # Retain NA for missing ethnicity1
  ) %>%
  group_by(la, month) %>%
  mutate(
    total_individuals_in_month = n(), 
    white_british_count_in_month = sum(white_british, na.rm = TRUE), # Exclude NA in summation
    white_british_percentage_in_month = (white_british_count_in_month / total_individuals_in_month) * 100
  ) %>%
  ungroup()

# Creating a variable for year from the month date
cla_merge$year <- format(cla_merge$month, "%Y")

# Add variable for social work staff turnover 
turnover <- read_excel("DR1 time varying variable/turnover_fs.xlsx")

#Change variable name from timer period to year 
colnames(turnover)[colnames(turnover) == "time_period"] <- "year"
colnames(turnover)[colnames(turnover) == "la_name"] <- "la"

# Add variable for number of children in la
number_children <- read_excel("DR1 time varying variable/no_children_2020-23.xlsx")

# merging 
# Turnover to main dataframe
class(cla_merge$year) #character
class(turnover$year) #Numeric

turnover$year <- as.character(turnover$year)

cla_merge <- left_join(cla_merge, turnover, by = c("la", "year"))

# Number of children to main dataframe 
class(number_children$year)
number_children$year <- as.character(number_children$year)

cla_merge <- left_join(cla_merge, number_children, by = c("la", "year"))

# Remove spaces from number of children variable 
colnames(cla_merge)[colnames(cla_merge) == "pop under 18"] <- "no_children"

save(cla_merge, file = "Output/cla_merge_pri_outcome.RData")

# Descriptives for missing data identification
missing_table <- cla_merge %>%
  summarise(across(everything(), ~ mean(is.na(.)) * 100)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "missing_percent") %>%
  filter(missing_percent > 0) %>%
  arrange(desc(missing_percent))

# View the result
print(missing_table)

miss_var_span <- cla_merge %>%
  select(`cla date`, ethnicity1, white_british, dob1, disability1, `age at ref1`, age_group, unborn) %>%
  group_by(across(everything(), ~ is.na(.))) %>%
  tally() %>%
  arrange(desc(n))

