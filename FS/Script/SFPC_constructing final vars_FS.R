
# Using the merged dataframe to create variables needed for the analysis. 

# Created: Emily Walker
# Date: 20/12/23 

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

# Clearing R -------------------------
# rm(list = ls())

# install packages if not already installed
packages <- c("tidyverse", "dplyr", "readxl", "tibble", "lubridate", "data.table", "arsenal", "pacman", "naniar")
# Install all packages
install.packages(packages, dependencies = TRUE)
# Load all packages
lapply(packages, library, character.only = TRUE)

library(tidyverse)
library(dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(data.table)
library(arsenal)
library(pacman)
library(naniar)

# Read in the datasets
load ("Output/merged_cla.RData")

load ("Output/merged_cpp.RData")

load ("Output/merged_proc.RData")

load ("Output/merged_scl.RData")

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
cla <- merge(test_merge_cla, go_live, by = c("la"), all.x = TRUE)

cpp <- merge(test_merge_cpp, go_live, by = c("la"), all.x = TRUE)

proc <- merge(test_merge_proc, go_live, by = c("la"), all.x = TRUE)

scl <- merge(test_merge_scl, go_live, by = c("la"), all.x = TRUE)


# Trial period began: March 2020
# Trial period ended?: November 2022

# Creating the treatment/control variable----
# CLA
cla$treatment <- ifelse(cla$`ref date` > cla$`go.live`, 1, 0)

# CPP 
cpp$treatment <- ifelse(cpp$`ref date` > cpp$`go.live`, 1, 0)

# PROC
proc$treatment <- ifelse(proc$`ref date` > proc$`go.live`, 1, 0)

# SCL
scl$treatment <- ifelse(scl$`ref date` > scl$`go.live`, 1, 0)


# Check the proportion of treatment and control 
cla %>%
  select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins

#    0      1  
# 32010    26807 

# Dropping dates outside of period of interest ----

# For the purposes of this RCT, we will only consider children who have been in touch with
#children’s social care between six-months before the first local authority’s implementation
#date, and six-months after the last local authority's implementation date. We define the
#implementation date as the date the Family Safeguarding model is considered ‘Operationally
#Live’ in the local authority.

#The trial period, as indicated in the
#diagram, takes place from 6 months prior to when the first local authority implements the
#model (or goes Operationally Live), and continues until 6 months after the final local authority
#implements the model.

#Their original referral date falls within the trial period as defined above. 

#Children that form part of the samples described above will be designated as part of the
#treatment and control groups according to whether Family Safeguarding was implemented in
#their local authority at the time of their first referral start date in the trial period.

# Subsetting the dataframe to only include dates of interest ----
# Trial period began: March 2020
# Trial period ended: November 2022

# Note: Can't yet subset for the later dates, as may lose follow up info. 
#subsetscl <- test_merge_scl[test_merge_scl$`ref date` >= "2020-03-01" & test_merge_scl$`ref date` <= "2022-11-30", ]

# CLA Dataset
subsetcla <- test_merge_cla[test_merge_cla$`ref date` >= "2020-03-01", ]

# CPP Dataset 
subsetcpp <- test_merge_cpp[test_merge_cpp$`ref date` >= "2020-03-01", ]

# PROC Dataset
subsetproc <- test_merge_proc[test_merge_proc$`ref date` >= "2020-03-01", ]

# SCL Dataset 
subsetscl <- test_merge_scl[test_merge_scl$`ref date` >= "2020-03-01", ]


sumdupl <- sum(duplicated(test_merge_cla[, "child id"]) | 
                                              duplicated(test_merge_cla[, "child id"], fromLast = TRUE))

print(sumdupl)  # 28546 repeat information for children. 


# RESHAPE DATA -----------------------------------------------------------------
# Trying to collapse the row observations so that there is one row for each child ID. 

# TEST 1------------------------------------------------------------------------
# Not currently working 

test_reshaped <- test_merge_cla %>%
  group_by(`child id`) %>%
  arrange(`ref date`) %>%
  mutate(row_number = row_number()) %>%
  pivot_wider(names_from = row_number, values_from = c(`case id`, `ref date`, `no further action`,
                                                       `assess start date`, `outcome of sa`, `age at ref`,
                                                     `ref trio`, `Start date of CLA (Period of care start date)`)
              , names_glue = "{.value}_{row_number}")


# TEST 2------------------------------------------------------------------------
collapsed_df <- aggregate(. ~ `child id`, data = test_merge_cla, FUN = function(x) paste(unique(x), collapse = ", "))


# TEST 3 10/04/24---------------------------------------------------------------

test2_reshaped <- test_merge_cla %>%
  group_by(`child id`) %>%
  summarise_all(list(~ if (is.character(.)) toString(unique(.)) else first(unique(.)))))


# TEST 4 10/04/24---------------------------------------------------------------




# For Secondary Outcome 5 - Repeat referrals for domestic violence, mental health or
# substance misuse - we alter our definition of treatment and control. Here, the relevant
# referral date will be the first referral in the trial period, where the following assessment
# identified one of the three factors at the end of assessment.