################################################################################
#
#                            Cleaning DR3 (CLA) 2024 
#                               SFPC 
#                      Family Safeguarding Model 
#                        Emily Walker 2024
#
################################################################################

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

# Open correct project before running code: 'sfpc_familysafeguarding_cleaning'

# Clearing R -------------------------
# rm(list = ls())

# Install and load tidyverse package ---------------------------

# install packages if not already installed

library(tidyverse)
library(dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(data.table)
library(arsenal)
library(ggplot2)

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

##################################################################
# Checking the datasets---------- 

# Swindon################################################

# Looking at class of variables ----
#CLA start
class(swind_dr3_cla$`Start date of CLA (Period of care start date)`)
# [1] "POSIXct" "POSIXt" 
# Child ID
class(swind_dr3_cla$`Child ID`)
# [1] "numeric"

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

# Joining the LA returns into 1 DR3 CLA dataset
all_dr3_cla_bind <- bind_rows(swind_dr3_cla, wands_dr3_cla, walsall_dr3_cla, lanc_dr3_cla)

########################################################################

# Renaming the variables to make coding more straightforward
colnames(all_dr3_cla_bind)[1]  <- "child id"
colnames(all_dr3_cla_bind)[2]  <- "cla date"

#######################################################################

# Checking on bind 
length(all_dr3_cla_bind$`child id`) # 18968
length(swind_dr3_cla$`Child ID`) # 6049
length(wands_dr3_cla$`Child ID`) # 5646
length(walsall_dr3_cla$`Child ID`) # 7273
length(lanc_dr3_cla$`Child ID`) #15197
5646 + 6049 + 7273 + 15197 # = 34165

########################################################################


#Creating a concatenated variable for LA and child id
all_dr3_cla_bind$idlacombined <- paste(all_dr3_cla_bind$`child id`, all_dr3_cla_bind$la)

# Check for duplicates in concatenated data
sum(duplicated(all_dr3_cla_bind$idlacombined))
# There are 164 duplicates. This 

sum(duplicated(all_dr3_cla_bind))
# 0 whole observatio nduplicates

# to check, does this mean there are no duplicate observations, but there are duplicate child ids because there is more than 1 referral

#######################################################################

# Chanage name for merging with DR2
colnames(all_dr3_cla_bind)[4] <- "child la id"

#######################################################################

# Saving the outcome dataframe
save(all_dr3_cla_bind, file = "Output/DR3_24_bind_cla.RData")
