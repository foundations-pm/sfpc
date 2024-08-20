# Script to check whether eligible children are missing from the sample.

########################################################################

library(tidyverse)
library(dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(data.table)
library(arsenal)
library(pacman)
library(naniar)

rm(list = ls())

# Reading in dataframe for child IDs for Swindon 
load("Output/DR2_bind.RData")

# Reading in additional child IDs sent by Swindon to double check eligibility. 
add_childID <- read_excel("Data/FS_DR3_2024/Swindon_Checks_List of BLAs.xlsx",
                          sheet = 2)

# Converting into same variable format
add_childID$`Id [Person]` <- as.character(add_childID$`Id [Person]`)


# Checking for matches
IDmatches <- inner_join(add_childID, all_dr2_bind, by = c("Id [Person]" = "child id"))

# Two matches, both referred outside of the trial period 
print(IDmatches)