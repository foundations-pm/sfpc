################################################################################
#
#                            MAIN SCRIPT 
#                               SFPC 
#                      Family Safeguarding Model 
#                      Tavistock Impact Estimates 
#                  Primary Outcome only (CLA status)
#                        Emily Walker AUGUST 24
#
################################################################################

# Description: the main script containing the code to prepare the 
# family safeguarding dataset for analysis for the Tavistock Cost Benefit Analysis.
# The DR3 outcome data includes only the primary outcome (whether or not a child 
# looked after up to 18 months after referral.)
# This main script reads in data from three levels of data return: 
     # DR1: LA level, aggregate data 
     # DR2: individual level, demographic and referral information 
     # DR3: individual level, outcome data 

# The variable that uniquely identifies children is 'child id'. 
# One child may have multiple referrals across the trial period. 


# Clearing R -------------------------
rm(list = ls())

# User paths -------------------------------------------------------------------
# Open project 'sfpc_familysafeguarding_cleaning'

install.packages <- TRUE
clean_DR1 <- TRUE
clean_DR2 <- TRUE
vars_DR2 <- TRUE
clean_DR3_24 <- TRUE
merging_24 <- TRUE
outcome_var <- TRUE

# Packages used ----------------------------------------------------------------

# install packages if not already installed
packages <- c("tidyverse",
              "dplyr", 
              "readxl",
              "tibble",
              "lubridate",
              "data.table",
              "arsenal",
              "pacman",
              "naniar")

# Install packes that are not yet installed 
sapply(packages, function(x) {
  if (!(x %in% installed.packages())) {
    install.packages(x, dependencies = TRUE)
  }
})

# Read in packages
lapply(packages, library, character.only = TRUE)

# Run code ---------------------------------------------------------------------
# Script 1
#                               DR1 CLEANING
#
# DR1: To run script which reads in and cleans the DR1 files from the five LAs
# Details: reads in 6 Excel files per LA, each containing data for a six month period. 
# Combines data creating one dataframe per LA. Then combines these so that there 
# is one dataframe containing all DR1 data across sites and time period. 

if (clean_DR1) {source(file.path("Script/sfpc_cleaning_FS_DR1.R"))}

--------------------------------------------------------------------------------
  # Script 2
  #                               DR2 CLEANING
  #
  # DR2: To run script which reads in and cleans the DR2 files from the five LAs
  # Details: reads in 6 Excel files per LA. Combines these so that there is one 
  # DR2 file per LA. Combines the LA dataframes into one dataframe containing 
  # all DR2 data across the time frame (referrals during the trial period).
  
  if (clean_DR2) {source(file.path("Script/sfpc_cleaning_FS_DR2.R"))}

--------------------------------------------------------------------------------
# Script 3
#                               DR2 CONSTRUCTING THE DATAFRAME AND VARS
  
  if (vars_DR2) {source(file.path("Script/Tavistock IE CBA (CLA)/sfpc_constructingdf_FS_CLA_2024.R"))}


--------------------------------------------------------------------------------
  # Script 4
  #                               DR3 CLEANING
  #
  # DR3: To run script which reads in and cleans the DR3 files from the five LAs.
  # Details: Reads in four sheets of Excel in separately for each LA. Each sheet 
  # contains data on an outcome of interest. The LAs are combined so that there 
  # are four dataframes, one per outcome, of the combined LAs. 
  
  if (clean_DR3_24) {source(file.path("Script/Tavistock IE CBA (CLA)/sfpc_cleaning_FS_DR3_24.R"))}

--------------------------------------------------------------------------------
  # Script 5
  #                               MERGING DR2 and DR3 
  
  if (merging_24) {source(file.path("Script/Tavistock IE CBA (CLA)/sfpc_merging_FS_CLA_2024.R"))}

--------------------------------------------------------------------------------
  
  # Script 6 
  #                        CONSTRUCTING THE OUTCOME VARIABLES 
  # 
  # A script which uses DR3 and DR2 to consruct the primary outcome variable. 
  # I.e. whether a child has become looked after within 18 months of referral. 
  
  if (outcome_var) {source(file.path("Script/Tavistock IE CBA (CLA)/sfpc_constuctingoutcome_FS_CLA_2024.R"))}

