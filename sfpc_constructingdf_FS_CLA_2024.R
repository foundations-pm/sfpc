################################################################################
#
#            Constructing DR2 dataframe for analysis (CLA) 2024 
#                               SFPC 
#                      Family Safeguarding Model 
#                        Emily Walker 2024
#
################################################################################

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

# Assigning number to referral dates
all_dr2_bind <- all_dr2_bind %>%
  group_by(`child la id`) %>%
  arrange(`ref date`) %>%
  mutate(referral_number = row_number())

# Changing ordering so that the number of referral is next to the referal date
all_dr2_bind <- all_dr2_bind[, c(1:4,18,5:17)]

# Check for duplicates------------------
duplicates <- duplicated(all_dr2_bind)
sum(duplicates) # 0 duplicates

# Pivoting the data wider=========
# Practice pivot -----
all_dr2_wide <- all_dr2_bind %>% 
  pivot_wider(
    id_cols = c(`child la id`, `LA`),
    names_from = `referral_number`, 
    values_from = c(`ref date`, `case id`, `no further action`, `assess start date`, `outcome of sa`, `previous cpp`,
    `gender`, `age at ref`, `ethnicity`, `disability`, `uasc`, `ref trio`)
  )

colnames(all_dr2_wide)

# Creating a wide dataframe without the demogrphic information. 
# This assumes it stays constant. This is not always the case, 
# i.e. ethnicity can change from referral to referral. Suggestion is to take
# the most recent information (as opposed to the first referral as previously thought)

short_dr2_wide <- all_dr2_bind %>% 
  pivot_wider(
    id_cols = c(`child la id`),
    names_from = `referral_number`, 
    values_from = c(`ref date`, `case id`, `no further action`, `assess start date`, `outcome of sa`, `previous cpp`,
                    `age at ref`,`ref trio`)
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

# DR2
subset_dr2 <- subset_dr2[subset_dr2$`ref date1` >= "2020-03-01", ]
# Exploring to see how many dropped 
length(subset_dr2$`ref date1`)
# 35246

# Filtering the dataset so that it only includes-----
# children who were <12 years old at time of first referral
# Exploring the dataframe 
length(subset_dr2$`age at ref1`)
# 35246


################################################################################

# Saving the dataframe. 
save(subset_dr2, file = "Output/subset_DR2.RData")
