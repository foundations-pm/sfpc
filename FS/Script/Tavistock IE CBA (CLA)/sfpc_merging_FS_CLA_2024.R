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

# Reading in the data 
# Read in DR1 ----
load ("Output/DR1_bind.RData")

# Reading in DR2 ----
load("Output/DR2_bind.RData")

# Reading in subset DR2
# This dataframe includes information only for the first referral for each child
# It includes a variable for 18 months after the first referral 
load("Output/subset_DR2.RData")

# Reading in DR3 2024 (CLA)----
load("Output/DR3_24_bind_cla.RData")

################################################################################

# DR2 temporary subset until all LAs are returned.
# 29/07/24 Wandsworth, Walsall and Swindon returned 
# List of local authorities to include
include_authorities <- c("Wandsworth", "Walsall", "Swindon", "Lancashire")

# Subset the dataframe
subset_dr2 <- subset_dr2 %>%
  filter(LA %in% include_authorities)

# Merging DR2 and DR3 CLA ---------
cla_merge <- left_join(subset_dr2, all_dr3_cla_bind, by = "child la id")

# Exploring which DR3 ids appear to be not match with DR2 
# How many unique variables are there in DR2 which are not in DR3
missing_ids <- setdiff(subset_dr2$`child la id`, all_dr3_cla_bind$`child la id`)
length(missing_ids)#90
#Indicates there are 90 IDs in DR2 that do not have a match in DR3.

# How many unique variables are there in DR3 which are not in DR2.
missing_ids_2 <- setdiff(all_dr3_cla_bind$`child la id`, subset_dr2$`child la id`)
length(missing_ids_2)#24736

table(all_dr3_cla_bind$la)
table(subset_dr2$LA)

# Exploring what % of children from the sample (DR2) when into care. 
CLAtotal_observations <- length(cla_merge$`cla date`)
# 9396
# Count number of non-missing observations in the DateInCare variable
CLAnon_missing_observations <- sum(!is.na(cla_merge$`cla date`))
# 1014

# Proportion of cohort that went into care 
(CLAnon_missing_observations/CLAtotal_observations)*100
# 10.79183

# Exploring the merged dataframe 
# Age 
mean(cla_merge$`age at ref1`, na.rm = TRUE)   #4.809683
sd(cla_merge$`age at ref1`, na.rm = TRUE)    # 4.145024

# Gender 
gender_counts <- table(cla_merge$gender1)
# Female                            Male                         Neither 
# 4329                              4697                               5 
#Not Known             Not stated/recorded       Not stated/recorded (or unborn) 
#1                             101                             205 
#Unborn                         Unknown 
#22                              36 


# Calculate the proportion as a percentage
gender_percentage <- prop.table(gender_counts) * 100
# Female                            Male                         Neither 
# 46.07279693                     49.98935717                      0.05321413 
# Not Known             Not stated/recorded Not stated/recorded (or unborn) 
# 0.01064283                      1.07492550                      2.18177948 
# Unborn                         Unknown 
# 0.23414219                      0.38314176 


# Look at missingness 
vis_dat(cla_merge)
vis_miss(cla_merge)

# Visualize missingness in your dataframe
aggr(cla_merge, col = c('navyblue', 'yellow'),
     numbers = TRUE, sortVars = TRUE,
     labels = names(cla_merge), cex.axis = .7,
     gap = 3, ylab = c("Missing data", "Pattern"))


save(cla_merge, file = "Output/cla_merge_dr2_dr3.RData")

