
# SFPC impact evaluation (NWD)
# Master script

# Author: AA
# Date: 30/01/2024

# Open project 'High-SFPC-Impact - SFPC No Wrong Door Data Cleaning'

# Clear R

rm(list = ls())

# Description: The main script containing the code to prepare the No Wrong Door dataset for analysis. 
# This main script reads in data from three levels of data return: 
# DR1: LA level, aggregate data 
# DR2: individual level, demographic and referral information 
# DR3: individual level, outcome data

# The variable that uniquely identifies children is 'child id'. 
# One child may have multiple referrals across the trial period. 
# 

# libraries -----------

#Install packages if not installed yet

library(tidyverse)
#install.packages("tidyverse")
library(plyr)
#install.packages("plyr")
library(dplyr)
#install.packages("dplyr")
library(readxl)
#install.packages("readxl")
library(tibble)
#install.packages("tibble")
library(lubridate)
#install.packages("lubridate")
library(data.table)
#install.packages("data.table")
library(arsenal)
#install.packages("arsenal")
library(pacman)
#install.packages("pacman")
library(naniar)
#install.packages("naniar")

#-------------------------------------------------------------------------------
# Script 1
#                                  DR1
#
# DR1: Run script which reads in and cleans the DR1 files from the five LAs
# Details: Reads in 5 Excel files per 3 LAs and 4 Excel file for 2 LA, each containing data for a six month period. 
# Combines data creating one dataframe per LA. Then combines these so that there 
# is one dataframe containing all DR1 data across sites and time period. 

{source(file.path("Script/nwd_dr1_data_cleaning.R"))}

--------------------------------------------------------------------------------
  
# Script 2
#                                 DR2
#
# DR2: Run code to read in 3 dr2 data (CP, CLA and Ref) separately.
# Details: For CP data read in 2 Excel files for 5 LAs. For CP data read in 2 Excel files for 5 LAs. 
# For Ref data read in 2 Excel files for 5 LAs.
# Combines these dataframes into one DR 2 dataframe containing 
# all DR2 data (CLA, CP and Referral data).

# Notice - CP data contains an error message about an "invalid argument to unary operator".
# However, all data is read in as it should.

# Read in CP data

{source(file.path("Script/nwd_dr2_cp_data_cleaning.R"))} # Error in -{ : invalid argument to unary operator but no errors when read individually

# Read in CLA data

{source(file.path("Script/nwd_dr2_cla_data_cleaning.R"))}

# Read in Referral data

{source(file.path("Script/nwd_dr2_ref_data_cleaning.R"))}

# Read in merged DR 2 file containing CP, CLA and Referral data
  
{source(file.path("Script/nwd_dr2_merge.R"))}

--------------------------------------------------------------------------------
#                                 DR3
#
# DR3: Run script which reads in and cleans the DR3 files from 4 LAs. Leicester excluded from trials
# Details: Reads in four sheets of Excel in separately for each LA. Each sheet 
# contains data on an outcome of interest. The LAs are combined so that there 
# are four dataframes, one per outcome, of the combined LAs. 
  
# Notice - DR3 data contains an error message about an "invalid argument to unary operator".
# However, all data is read in as it should.

{source(file.path("Script/nwd_dr3_data_cleaning.R"))} # Error in -{ : invalid argument to unary operator but no errors when read individually

--------------------------------------------------------------------------------
#                               MERGING
#
# MERGE: Run script which reads in the DR1; DR2 and DR3 datasets, then merges
# them. First DR2 with the 4 DR3 datasets, then DR1 onto each of these datasets.
  
# Notice - Certain data checks are included in this script including duplication and missingness checks.
# Some data visualisation of the missingness checks are also included. These are saved in the Output folder
  
# Line 298 contains this error message: "Error in quickdf(.data[names(cols)]) : length(rows) == 1 is not TRUE"
# From line 179 to 214 there are warning messages that R has "Detected an unexpected many-to-many relationship between `x` and `y`".

{source(file.path("Script/nwd_mergingallDRs.R"))}

#-------------------------------------------------------------------------------
