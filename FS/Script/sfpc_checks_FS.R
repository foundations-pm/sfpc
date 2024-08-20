
# PRE MERGE CHECKS #
# Checks on the individual binded dataframes before merging #

# Things to look out for?

# Clearing R -------------------------
rm(list = ls())

# install packages if not already installed
library(tidyverse)
library(dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(data.table)
library(arsenal)

# Read in DR1 ----
load ("Output/DR1_bind.RData")

# Reading in DR2 ----
load("Output/DR2_bind.RData")

# Reading in the DR3s ----
load ("Output/DR3_bind_cla.RData")

load ("Output/DR3_bind_cpp.RData")

load ("Output/DR3_bind_proc.RData")

load ("Output/DR3_bind_scl.RData")

###############################################
# CARE PROCEEDINGS
# Checking if all the relevant DR2 IDs were included in the previous DR 3 return 
unique_ids_dr2 <- unique(all_dr2_bind$`child id`)
unique_ids_dr3 <- unique(bind_dr3_proc$`Child ID`)

# Find IDs that are in dr2 but not in dr3
ids_in_dr2_not_in_dr3 <- setdiff(unique_ids_dr2, unique_ids_dr3)

# Create a new data frame with observations from dr12 that are not in dr3
df_not_in_dr3 <- all_dr2_bind[all_dr2_bind$`child id` %in% ids_in_dr2_not_in_dr3, ]

---------------------------------------------------
# SCHOOL DATA 
# Checking if all the relevant DR2 IDs were included in the previous DR3 return 
unique_ids_dr2 <- unique(all_dr2_bind$`child id`)
unique_ids_dr3 <- unique(bind_dr3_scl$`Child ID`)

# Find IDs that are in df1 but not in df2
scl_ids_in_dr2_not_in_dr3 <- setdiff(unique_ids_dr2, unique_ids_dr3)

# Create a new data frame with observations from df1 that are not in df2
scl_df_not_in_dr3 <- all_dr2_bind[all_dr2_bind$`child id` %in% scl_ids_in_dr2_not_in_dr3, ]

-----------------------------------------------------




