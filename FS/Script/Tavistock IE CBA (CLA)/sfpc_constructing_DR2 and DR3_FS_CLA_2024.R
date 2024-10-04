################################################################################
#
#            Constructing DR2 dataframe for analysis (CLA) 2024 
#                 Cleaning DR3 dataframe for analysis 
#                               SFPC 
#                      Family Safeguarding Model 
#                        Emily Walker 2024
#
################################################################################

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

# Script to construct DR2 data pre merging with DR3 CLA.

#Create a dataset which groups by child ID, arrange by referral dates,  
#only keep the first referral. Then create second column where you add 18 months to the referral date.
#Assign a number to the refdates. 1-10. And then you can arrange by the number of ref.
#A separate column with the number of referral next to the referral date. Then pivot wider, on that basis.
#Step 3-6 should be 1-3. Create referral order column. Then do the pivot. Do it all in a mutate statement. 
#Piping – the pivot is dependent on creating the new column. Pipe the pivot within the table. Case 1 is multiple loops.
#Names from = columns, values from = referral date.

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

# Reading in DR2 ----
load("Output/DR2_bind.RData")

# Checking how many observations there are
length(all_dr2_bind$`child id`) # 64009

# Assigning number to referral dates
all_dr2_bind <- all_dr2_bind %>%
  group_by(`child la id`) %>%
  arrange(`ref date`) %>%
  mutate(referral_number = row_number())

# Changing ordering so that the number of referral is next to the referral date
all_dr2_bind <- all_dr2_bind[, c(1:4,18,5:17)]

# Check for duplicates------------------
duplicates <- duplicated(all_dr2_bind)
sum(duplicates) # 0 duplicates

# Check for duplicates for child ID
IDduplicates <- duplicated(all_dr2_bind$`child la id`)
sum(IDduplicates)  # 19920

# Pivoting the data wider=========
# Practice pivot -----
all_dr2_wide <- all_dr2_bind %>% 
  pivot_wider(
    id_cols = c(`child la id`, `LA`),
    names_from = `referral_number`, 
    values_from = c(`ref date`, `case id`, `previous cpp`,
    `gender`, `age at ref`, dob, `ethnicity`, `disability`, `uasc`, `ref trio`)
  )

colnames(all_dr2_wide)
length(all_dr2_wide$`child la id`)  #44089

# Exploring whethere there is the correct number of observations 
64009 - 19920 # 44089

# Appears to be correct number, as is the full sample minus the number of duplicates. 



# Creating a wide dataframe without the demogrphic information. 
# This assumes it stays constant. This is not always the case, 
# i.e. ethnicity can change from referral to referral. Suggestion is to take
# the most recent information (as opposed to the first referral as previously thought)

short_dr2_wide <- all_dr2_bind %>% 
  pivot_wider(
    id_cols = c(`child la id`),
    names_from = `referral_number`, 
    values_from = c(`ref date`, `case id`, `previous cpp`,
                    `gender`, `age at ref`, dob, `ethnicity`, `disability`, `uasc`, `ref trio`)
  )

colnames(short_dr2_wide)

###########################################################################

# Creating a subset dataframe which only includes the first referral 

# Remove underscore from column names 
names(all_dr2_wide) <- gsub("_", "", names(all_dr2_wide))

# Find columns that end with ' 1'
cols_to_keep <- grep("1$", names(all_dr2_wide), value = TRUE)

# Checking colnames 
colnames(all_dr2_wide)

# Add 'child id' to the list of columns to keep
cols_to_keep <- c("child la id", "LA", cols_to_keep)

# Subset the dataframe to keep only these columns
subset_dr2 <- all_dr2_wide[, cols_to_keep, drop = FALSE]

# checking colnames 
colnames(subset_dr2)

length(all_dr2_wide$`child la id`)   #44089
length(subset_dr2$`child la id`) #44089

# Check missing?
sum(is.na(all_dr2_wide$`child la id`)) #0
sum(is.na(subset_dr2$`child la id`)) # 0

# Creating a stripped back DR2 dataframe for the analysis of the primary outcome.----
# Create second column where you add 18 months to the referral date.

subset_dr2$`ref date 18months` <- subset_dr2$`ref date1` %m+% months(18)

# SUBSET: ----
#0-12 at the time of referral 
#who have been referred within the trial period
#and whose initial assessment identified parental substance misuse, domestic violence, or parental mental health as factors identified at the end of assessment. 
#Since these factors are only identified at assessment, our sample is restricted to children whose referral has progressed to an assessment 
#and where one of the factors identified at assessment includes one of the three factors defined above.

# Filtering DR2 so that it only covers the referral window in the trial period. 
# Subsetting the dataframe to only include dates of interest ----
# Trial period began: March 2020

# Note: Can't yet subset for the later dates, as may lose follow up info. 

# Exploring the dataframe 
length(subset_dr2$`ref date1`)
# 44089


# 
sum(subset_dr2$`ref date1`<="2020-03-01", na.rm = TRUE) # 8844
8843/44089*100  #20.05716

# DR2
subset_dr2 <- subset_dr2[subset_dr2$`ref date1` >= "2020-03-01", ]
length(subset_dr2$`ref date1`) #35246

sum(subset_dr2$`ref date1`>= "2022-11-30", na.rm = TRUE) # 470
470/35246*100 #1.333485

subset_dr2 <- subset_dr2[subset_dr2$`ref date1` <= "2022-11-30",]
length(subset_dr2$`ref date1`) #34826
# Exploring to see how many dropped 


# Missing = 2

# Filtering the dataset so that it only includes-----
# children who were <12 years old at time of first referral
# Exploring the dataframe 
length(subset_dr2$`age at ref1`)
# 34826

# Check how many children in the sample are over the age of 12
sum(subset_dr2$`age at ref1`> 12, na.rm = TRUE) # 9484
# How many are under 13
sum(subset_dr2$`age at ref1`<= 12, na.rm = TRUE) # 25454

# Check whether this makes up the whole sample
25454 + 9484 # = 34938
# check whether difference is accounted for by the missing 
sum(is.na(subset_dr2$`age at ref1`)) # 308
34938 + 308 # 35246

# What proportion of the sample is over the age of 12?
9484/35246*100  #26.90802

subset_dr2 <- subset_dr2[subset_dr2$`age at ref1` <= 12 | is.na(subset_dr2$`age at ref1`), ]

# Checking how many observations there are post subset
length(na.omit(subset_dr2$`age at ref1`)) # 25454

sum(is.na(subset_dr2$`child la id`)) # 2

# Creating grouped variable 
subset_dr2$age_group <- ifelse(subset_dr2$`age at ref1` < 0, "unborn", 
                               ifelse(subset_dr2$`age at ref1` <= 3, "3 and under", 
                                      ifelse(subset_dr2$`age at ref1` >= 4, "4-12", NA)))

# Factor variable 
subset_dr2$`age at ref1` <- factor(subset_dr2$`age at ref1`, 
                                   levels = c(-1, 0:12), 
                                   labels = c("unborn", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"))


# Converting gender = unborn into age unborn where this is missing
# Age grouped
table(subset_dr2$gender1, subset_dr2$age_group, useNA = "ifany")
subset_dr2$age_group[subset_dr2$gender1 == "Unknown/unborn" & is.na(subset_dr2$age_group)] <- "unborn"

# Age continuous 
table(subset_dr2$gender1, subset_dr2$`age at ref1`, useNA = "ifany")
subset_dr2$`age at ref1`[subset_dr2$gender1 == "Unknown/unborn" & is.na(subset_dr2$`age at ref1`)] <- "unborn"

# Creating a binary variable for unborn. 
subset_dr2$unborn <- ifelse(subset_dr2$age_group == "unborn", 1, 0)

# Recoding missing ethnicity to unborn where age group is unborn 
subset_dr2 <- subset_dr2 %>%
  mutate(ethnicity1 = if_else(age_group == "unborn", "unborn", ethnicity1))

# Filtering the dataset so that it only includes--------------
# Children whose referral has progressed to an assessment 
table(subset_dr2$`no further action1`, useNA = "ifany")
#     0      1    <NA> 
#   24624  1131     7 

# NOTE: Do not need to filter on this, as if 'trio of vulnerabilities' appear
# In factors identified at assessment, necessarily progressed to assessment. 

table(subset_dr2$`ref trio1`, useNA = "ifany")
#        0     1       <NA> 
#      14993  10767     2   

# Working out what % of sample were eligible 
10767 /25762*100  #41.79412

sum(is.na(all_dr2_wide$`ref trio1`)) # 0
sum(is.na(subset_dr2$`ref trio1`)) # 2

# Subsetting so that there are only children whose first referal was for DA, MH or SU
subset_dr2 <- subset_dr2[subset_dr2$`ref trio1` == 1 | is.na(subset_dr2$`ref trio1`), ] 
table(subset_dr2$`ref trio1`, useNA = "ifany")
################################################################################

# Saving the dataframe. 
save(subset_dr2, file = "Output/subset_DR2_pre.RData")

################################################################################
#
#                            Cleaning DR3 (CLA) 2024 
#                               SFPC 
#                      Family Safeguarding Model 
#                        Emily Walker 2024
#
################################################################################

# Reading in DR3 files ====
# 01 March 2020 - 30 May 2024
# Outcome: Whether a child became looked after

##################################################################

# Reading in Swindon DR3 CLA data 
# Children in the target cohort that became looked after between 01 March 2020 - 30 May 2024. 
# An observation is uniquely identified by Child ID and the period of care start date. If multiple periods of care, please list in separate rows. Please provide us with information on all periods of care that children have started in the time frame provided below.

swind_dr3_cla <- read_excel("Data/FS_DR3_2024/swindon_dr3_july24.xlsx",
                            sheet = 3,
                            skip = 4)


##################################################################

# Reading in Wandsworth DR3 CLA data
# Children in the target cohort that became looked after between 01 March 2020 - 30 May 2024. 
# An observation is uniquely identified by Child ID and the period of care start date. If multiple periods of care, please list in separate rows. Please provide us with information on all periods of care that children have started in the time frame provided below.

wands_dr3_cla <- read_excel("Data/FS_DR3_2024/wands_dr3_july24.xlsx",
                            sheet = 3,
                            skip = 4)

##################################################################

# Reading in Walsall DR3 CLA data
# Children in the target cohort that became looked after between 01 March 2020 - 30 May 2024. 
# An observation is uniquely identified by Child ID and the period of care start date. If multiple periods of care, please list in separate rows. Please provide us with information on all periods of care that children have started in the time frame provided below.

walsall_dr3_cla <- read_excel("Data/FS_DR3_2024/walsall_dr3_july24.xlsx",
                              sheet = 3,
                              skip = 4)


# Reading in Lancashire DR3 CLA Data 
lanc_dr3_cla <- read_excel("Data/FS_DR3_2024/lancashire_dr3_july24.xlsx",
                           sheet = 3, 
                           skip = 4)


#Reading in Telford and Wrekin DR3 CLA Data 
telford_dr3_cla <- read_excel("Data/FS_DR3_2024/telford_dr3_july24.xlsx",
                              sheet = 3, 
                              skip = 4)

##################################################################
# Checking the datasets---------- 

# Swindon################################################

# Looking at class of variables ----
#CLA start
class(swind_dr3_cla$`Start date of CLA (Period of care start date)`) # [1] "POSIXct" "POSIXt" 
# Child ID
class(swind_dr3_cla$`Child ID`) # [1] "numeric"

# Looking at dimentions of variables 
# CLA start 
range(swind_dr3_cla$`Start date of CLA (Period of care start date)`, 
      na.rm = TRUE)
# "2020-04-03 UTC" "2024-05-14 UTC"
#FLAG: first CLA start date in Swindon begins in April, rather than March 2020. Emily has emailed Swindon to check whether they looked at the right timeframe. 

# How many children went into care, as a proportion of the total 
# Count total number of observations 
Stotal_observations <- length(swind_dr3_cla$`Start date of CLA (Period of care start date)`)
# 6049
# Count number of non-missing observations in the DateInCare variable
Snon_missing_observations <- sum(!is.na(swind_dr3_cla$`Start date of CLA (Period of care start date)`))
# 392

# Proportion of cohort that went into care 
(Snon_missing_observations/Stotal_observations)*100
# 6.48041

# Wandsworth################################################
# Checking the class 
class(wands_dr3_cla$`Child ID`)
#  "numeric"
class(wands_dr3_cla$`Start date of CLA (Period of care start date)`)
# "POSIXct" "POSIXt" 

# Looking at dimensions of variables 
# CLA start 
range(wands_dr3_cla$`Start date of CLA (Period of care start date)`, 
      na.rm = TRUE)
#"2020-03-13 UTC" "2024-05-27 UTC"

# How many children went into care, as a proportion of the total 
# Count total number of observations 
Wtotal_observations <- length(wands_dr3_cla$`Start date of CLA (Period of care start date)`)
# 5646
# Count number of non-missing observations in the DateInCare variable
Wnon_missing_observations <- sum(!is.na(wands_dr3_cla$`Start date of CLA (Period of care start date)`))
# 339

# Proportion of cohort that went into care 
(Wnon_missing_observations/Wtotal_observations)*100
# 6.004251

# Running a visual to look at the distribution of CLA start dates
ggplot(wands_dr3_cla, aes(x = `Start date of CLA (Period of care start date)`)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Dates Children Went into Care",
       x = "Date",
       y = "Density") +
  theme_minimal()

# Walsall################################################

# Checking class---------
class(walsall_dr3_cla$`Child ID`)
# "numeric"
class(walsall_dr3_cla$`Start date of CLA (Period of care start date)`)
# "POSIXct" "POSIXt" 

# Checking range of the dates----------
range(walsall_dr3_cla$`Start date of CLA (Period of care start date)`,
      na.rm = TRUE)
# "2020-03-13 UTC" "2024-05-16 UTC"

# How many children went into care, as a proportion of the total--------
# Count total number of observations 
Wltotal_observations <- length(walsall_dr3_cla$`Start date of CLA (Period of care start date)`)
# 7273
# Count number of non-missing observations in the DateInCare variable
Wlnon_missing_observations <- sum(!is.na(walsall_dr3_cla$`Start date of CLA (Period of care start date)`))
# 523

# Proportion of cohort that went into care 
(Wlnon_missing_observations/Wltotal_observations)*100
# 7.19098

# Lancashire ###########################################################
# Checking the class 
class(lanc_dr3_cla$`Child ID`)   #"numeric"
class(lanc_dr3_cla$`Start date of CLA (Period of care start date)`) 
#  "POSIXct" "POSIXt"

# Checking range of the dates----------
range(lanc_dr3_cla$`Start date of CLA (Period of care start date)`, na.rm = TRUE)
#  "2020-03-06 UTC" "2024-05-24 UTC"

# How many children went into care, as a proportion of the total 
# Count total number of observations 
Ltotal_observations <- length(lanc_dr3_cla$`Start date of CLA (Period of care start date)`)
# 15197
# Count number of non-missing observations in the DateInCare variable
Lnon_missing_observations <- sum(!is.na(lanc_dr3_cla$`Start date of CLA (Period of care start date)`))
# 1596

# Proportion of cohort that went into care 
(Lnon_missing_observations/Ltotal_observations)*100
# 10.50207

# Lancashire ###########################################################
# Checking the class 
class(telford_dr3_cla$`Child ID`) #"character"
class(telford_dr3_cla$`Start date of CLA (Period of care start date)`) #POSIXct" "POSIXt


########################################################################
# Adding a LA marker variable to the individual datasets
# Swindon 
swind_dr3_cla$la <- "Swindon"
#Wandsworth
wands_dr3_cla$la <- "Wandsworth"
#Walsall
walsall_dr3_cla$la <- "Walsall"
#Lancashire 
lanc_dr3_cla$la <- "Lancashire"
# Telford 
telford_dr3_cla$la <- "Telford"

# Joining the LA returns into 1 DR3 CLA dataset
all_dr3_cla_bind <- bind_rows(swind_dr3_cla, wands_dr3_cla, walsall_dr3_cla, lanc_dr3_cla)

# Converting to character and then joining with Telford 
all_dr3_cla_bind$`Child ID` <- as.character(all_dr3_cla_bind$`Child ID`)
all_dr3_cla_bind <- bind_rows(all_dr3_cla_bind, telford_dr3_cla)

########################################################################

# Renaming the variables to make coding more straightforward
colnames(all_dr3_cla_bind)[1]  <- "child id"
colnames(all_dr3_cla_bind)[2]  <- "cla date"

#######################################################################

# Checking on bind 
length(all_dr3_cla_bind$`child id`) # 37992
length(swind_dr3_cla$`Child ID`) # 6049
length(wands_dr3_cla$`Child ID`) # 5646
length(walsall_dr3_cla$`Child ID`) # 7273
length(lanc_dr3_cla$`Child ID`) #15197
length(telford_dr3_cla$`Child ID`) #3827
5646 + 6049 + 7273 + 15197 + 3827 # = 37992

########################################################################


#Creating a concatenated variable for LA and child id
all_dr3_cla_bind$idlacombined <- paste(all_dr3_cla_bind$`child id`, all_dr3_cla_bind$la)

# Check for duplicates in concatenated data
sum(duplicated(all_dr3_cla_bind$idlacombined))
# There are 177 duplicates. 

sum(duplicated(all_dr3_cla_bind))
# 0 whole observation duplicates

# to check, does this mean there are no duplicate observations, but there are duplicate child ids because there is more than 1 referral

#######################################################################

# Change name for merging with DR2
colnames(all_dr3_cla_bind)[4] <- "child la id"

#######################################################################

# Saving the outcome dataframe
save(all_dr3_cla_bind, file = "Output/DR3_24_bind_cla.RData")


