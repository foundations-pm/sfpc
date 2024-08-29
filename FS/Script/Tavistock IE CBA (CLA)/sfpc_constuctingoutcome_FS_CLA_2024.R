################################################################################
#
#                Constructing primary outcome variable (CLA) 2024 
#                     From merged dataset DR2 and DR3 
#                               SFPC 
#                      Family Safeguarding Model 
#                       Tavistock Analysis
#                        Emily Walker 2024
#
################################################################################

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

load("Output/cla_merge_dr2_dr3.RData")

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

# Constructing the primary outcome variable 
# Whether or not the child has become looked after
# measure: Coded 1 if the child has become looked after at any
# point within 18 months of the referral. Coded 0 if the
# child has not become looked after within this period.

cla_merge$primary_outcome <- ifelse(
  is.na(cla_merge$`cla date`), 
  0,
  ifelse(
    cla_merge$`cla date` <= cla_merge$`ref date 18months`,
    1,
    0
  )
)

table(cla_merge$primary_outcome)
#  0          1 
# 8606       790 


# Treatment condition:
################################################################################
# COPY AND PASTED FROM PREVIOUS SCRIPT - NOT READY YET 
################################################################################

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
cla_merge <- merge(cla_merge, go_live, by = c("la"), all.x = TRUE)


# Trial period began: March 2020
# Trial period ended: November 2022

# Creating the treatment/control variable----
# CLA
cla_merge$treatment <- ifelse(cla_merge$`ref date1` > cla_merge$`go.live`, 1, 0)

# Check the proportion of treatment and control 
cla_merge %>%
  select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins

#   0    1    <NA>   Sum 
# 4755  4551   90    9396 


# How many children in the treatment entered care and how many in the control 

proportions <- tapply(cla_merge$primary_outcome, cla_merge$treatment, mean)
print(proportions)
#0          1 
#0.06435331 0.10635025 

CLA_table <- table(Treatment = cla_merge$treatment, `Entered care` = cla_merge$primary_outcome)
# Calculate percentage of entering care by treatment group
percent_table <- prop.table(CLA_table, 1) * 100
print(percent_table)
# Extract the percentage for those who entered care (entering care = 1)
care_percentage <- percent_table[, 2]


colnames(cla_merge)

################################################################################

# Creating time periods.

#time: This represents the time periods after the baseline (as dummy variables).
#-	We consider the referral date to be the relevant date according to which the relevant time dummy is determined.
#-	A series of indicator variables adjusting for time trends by introducing dummy variables for each time after the baseline period t = 0.
#-	Does the baseline span from -6 months to 0? Or is it -6 months? Are there 6 time periods?
#  -	Plan: Create indicator variable (factor?) with time periods, split into: 

# First LA (Walsall) go live: 01/09/2020 (1 sept 2020)
# Baseline begins six months before this: 

#  1)	 -6 to 0 months; March 1 2020 - September 1 2020
#  2)	0 to 6 months; September 1 2020 - March 1 2021
#  3)	6 to 12 months; March 1 2021- September 1 2021
#  4)	12 to 18 months; September 1 2021 - March 1 2022
#  5)	18 to 24 months; March 1 2022 - September 2022
#  6)	24 to 30 months; September 2022 - March 2023
#  7) 30 to 36 months; March 2023 - September 2023
#  8) 36 to 42 months; September 2023 - March 2024
#  9) 42 to 48 months; March 2024 - September 2024
#  10) 48 to 54 months; September 2024 to March 2025
#  11) 54 to 60 months; March 2025 to September 2025

#Converting referral date to date rather than POX, to enable running the rest of
#the code.
cla_merge$`ref date1` <- as.Date(cla_merge$`ref date1`)

start_date <- as.Date("2020-03-01")

periods <- seq.Date(start_date, by = "6 months", length.out = 12)

cla_merge$time_period <- cut(cla_merge$`ref date1`, 
                        breaks = periods, 
                        labels = paste("Period", 1:11),
                        right = FALSE,  # Left-closed intervals [start_date, end_date)
                        include.lowest = TRUE)

