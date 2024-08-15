################################################################################
#
#                     Merging DR3 and DR2 (CLA) 2024 
#                               SFPC 
#                      Family Safeguarding Model 
#                       Tavistock Analysis
#                        Emily Walker 2024
#
################################################################################

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
include_authorities <- c("Wandsworth", "Walsall", "Swindon")

# Subset the dataframe
subset_dr2 <- subset_dr2 %>%
  filter(LA %in% include_authorities)

# Merging DR2 and DR3 CLA ---------
cla_merge <- left_join(subset_dr2, all_dr3_cla_bind, by = "child la id")
     # Warning message:
     #In left_join(subset_dr2, all_dr3_cla_bind, by = "child la id") :
     #  Detected an unexpected many-to-many relationship between `x` and `y`.
     #Row 66 of `x` matches multiple rows in `y`.
     #ℹ Row 19 of `y` matches multiple rows in `x`.
     #ℹ If a many-to-many relationship is expected, set `relationship = "many-to-many"` to silence this warning.


# Exploring which DR3 ids appear to be not maych with DR2 
# How many unique variables are there in DR2 which are not in DR3
missing_ids <- setdiff(subset_dr2$`child la id`, all_dr3_cla_bind$`child la id`)
length(missing_ids)#429

# How many unique variables are there in 
missing_ids_2 <- setdiff(all_dr3_cla_bind$`child la id`,subset_dr2$`child la id`)
length(missing_ids_2)#3

save(cla_merge, file = "Output/cla_merge_dr2_dr3.RData")

