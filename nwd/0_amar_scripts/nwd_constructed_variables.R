
# SFPC impact evaluation (NWD)
# Constructing final outcome variables

# Author: AA
# Date: 21/02/2024

rm(list = ls()) 

#install.packages("tidyverse")
library(tidyverse)
# install.packages("readxlsb")
library(readxl)
# install.packages("tibble")
library(tibble)
# install.packages("dplyr")
library(dplyr)
#install.packages("psych")
library(psych)
#install.packages("plyr") 
library(plyr)
library(writexl)
#install.packages("writexl")
library(openxlsx)
#install.packages('openxlsx')

# Read in merged datasets ####

#CIN

load ("Output/merged_cin.RData")

#CLA

load ("Output/merged_cla.RData")

#CAREEP

load ("Output/merged_careep.RData")

#NEET

load ("Output/merged_neet.RData")

# Remove Leicester from the data frames

test_merge_careep <- test_merge_careep[!(test_merge_careep$LA %in% "Leicester"),]
test_merge_cin <- test_merge_cin[!(test_merge_cin$LA %in% "Leicester"),]
test_merge_cla <- test_merge_cla[!(test_merge_cla$LA %in% "Leicester"),]
test_merge_neet <- test_merge_neet[!(test_merge_neet$LA %in% "Leicester"),]

# Omit Month column from dataframe

test_merge_careep$Month <- NULL
test_merge_cin$Month <- NULL
test_merge_cla$Month <- NULL
test_merge_neet$Month <- NULL

# Create treatment/control variable ----

# Dates----
# Trial period began: October 2019
# Last go-live date: September 2021
# Trial period ended: March 2022
# (subset to only include referral dates within this time period)

# Norfolk go live date: 15/06/2021 (15 May 2021)
# Redcar go live date: 15/09/2021 (15 September 2021)
# Rochdale go live date: 15/04/2020 (15 April 2020)
# Warrington go live date: 15/04/2021 (15 April 2021)

# Conditions ----

# Primary outcome

# Control: Young people whose first referral in the trial period was when 
# the local authority was running their business as usual model.

# Treatment: Young people whose first referral in the trial period took 
# place when the local authority was running the No Wrong Door model.

# Secondary outcomes

# Control: Young people whose first period of care that starts within the 
# trial period was when the local authority was running their business as usual model.

# Treatment: Young people whose first period of care that starts within 
# the trial period was when the local authority was running the No Wrong Door model.

# Randomise the order based on in which local authorities implement the programme, 
# in six month intervals, rather than based on which local authority implements the model.

# check for duplicate rows

sum(duplicated(test_merge_careep)) # 2930

test_merge_careep <- test_merge_careep[-which(duplicated(test_merge_careep)), ]

sum(duplicated(test_merge_cin)) # 4814

test_merge_cin <- test_merge_cin[-which(duplicated(test_merge_cin)), ]

sum(duplicated(test_merge_cla)) # 2712

test_merge_cla <- test_merge_cla[-which(duplicated(test_merge_cla)), ]

sum(duplicated(test_merge_neet)) # 2675

test_merge_neet <- test_merge_neet[-which(duplicated(test_merge_neet)), ]

# Create a dataframe with go-live dates ----

# Define variables

LA <- c("Norfolk", "Redcar", "Rochdale", "Warrington")

go.live <- as.Date(c("2021/06/15", "2021/09/15", "2020/04/15", "2021/04/15"))

go_live <- data.frame(LA, go.live)

print(go_live)

# Merge go-live dates on to main dataframe ----

careep <- merge(test_merge_careep, go_live, by = c("LA"), all.x = TRUE)

cin <- merge(test_merge_cin, go_live, by = c("LA"), all.x = TRUE)

cla <- merge(test_merge_cla, go_live, by = c("LA"), all.x = TRUE)

neet <- merge(test_merge_neet, go_live, by = c("LA"), all.x = TRUE)

# Create treatment/control variables for outcomes

# Care EP

careep$treatment <- ifelse(careep$`Referral Date` > careep$go.live, 1, 0)

# CLA

cla$treatment <- ifelse(cla$`Referral Date` > cla$go.live, 1, 0)

# CIN

cin$treatment <- ifelse(cin$`Referral Date` > cin$go.live, 1, 0)

# NEET

neet$treatment <- ifelse(neet$`Referral Date` > neet$go.live, 1, 0)

# Check the proportion of treatment and control for outcomes

careep %>% select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins

#   0     1   <NA>    Sum 
# 28986 20133 11024  60143

cla %>% select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins

#   0     1    <NA>   Sum 
# 25915 19395  6000  51310

cin %>% select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins

#   0     1    <NA>   Sum 
# 32750 31000  7539 71289 

neet %>% select(`treatment`)%>%
  table(useNA = "ifany") %>%
  addmargins

#   0     1    <NA>   Sum 
# 25933 19406  6062  51401

# Drop dates outside of period of interest ----

# From TP: "For the purposes of our evaluation, we will only consider children who have 
# been in touch with children’s social care between four months before the first 
# local authority’s implementation date, and four months after the last local 
# authority's implementation date." We define the implementation date as the date 
# the No Wrong Door model is considered ‘Operationally Live’ in the local authority.

# The trial period takes place from 4 months prior to the first local authority implements 
# the model (or goes Operationally Live), and continues until 4 months after the final local 
# authority implements the model.

# Careep dataset

careepssdate <- careep[careep$`Referral Date` >= "2019-12-15" & careep$`Referral Date` <= "2022-01-15",] # Change PO

# CIN dataset

cinssdate <- cin[cin$`Referral Date` >= "2019-12-15" & cin$`Referral Date` <= "2022-01-15",]

# CLA dataset

classdate <- cla[cla$`Referral Date` >= "2019-12-15" & cla$`Referral Date` <= "2022-01-15",] # Change PO

# NEET dataset

neetssdate <- neet[neet$`Referral Date` >= "2019-12-15" & neet$`Referral Date` <= "2022-01-15",]

# Check how many rows have the same Child ID information

sumduplcareep <- sum(duplicated(careepssdate[, "Child ID"]) | 
                 duplicated(careepssdate[, "Child ID"], fromLast = TRUE))

print(sumduplcareep) # 38,977 additional rows of information from a child

sumduplcla <- sum(duplicated(classdate[, "Child ID"]) | 
                       duplicated(classdate[, "Child ID"], fromLast = TRUE))

print(sumduplcla) # 30,563 additional rows of information from a child

sumduplcin <- sum(duplicated(cinssdate[, "Child ID"]) | 
                    duplicated(cinssdate[, "Child ID"], fromLast = TRUE))

print(sumduplcin) # 49,456 additional rows of information from a child

sumduplneet <- sum(duplicated(neetssdate[, "Child ID"]) | 
                    duplicated(neetssdate[, "Child ID"], fromLast = TRUE))

print(sumduplneet) # 30,656 additional rows of information from a child

# Subset CAREEP data based on whether child has become looked after 18 month after referral

careepla <- 

# Subset CAREEP data based on sample age 12-17 at referral within trial period

careepage12_17 <- careepssdate[careepssdate$`Age at Referral` >"12" & careepssdate$`Age at Referral` <"17.99", ]

careepage12_17 <- careepage12_17[complete.cases(careepage12_17$`Age at Referral`),]

# Subset CLA data based on sample age 12-17 at referral within trial period

claage12_17 <- classdate[classdate$`Age at Referral` >"12" & classdate$`Age at Referral` <"17.99", ]

claage12_17 <- claage12_17[complete.cases(claage12_17$`Age at Referral`),]

# Subset CIN data based on sample age 12-17 at referral within trial period

cinage12_17 <- cinssdate[cinssdate$`Age at Referral` >"12" & cinssdate$`Age at Referral` <"17.99", ]

cinage12_17 <- cinage12_17[complete.cases(cinage12_17$`Age at Referral`),]

# Subset NEET data based on sample age 16-20 at referral within trial period

neetage16_20 <- neetssdate[neetssdate$`Age at Referral` >"16" & neetssdate$`Age at Referral` <"20.99", ]

neetage16_20 <- neetage16_20[complete.cases(neetage16_20$`Age at Referral`),]

# Subset NEET data based on sample age 16-20 that left care during trial period

# neetage16_20lc <- neetage16_20[neetage16_20$`Period of care end date (date left care)`, neetage16_20$go.live] # Need to fix

# Recheck how many rows have the same Child ID information

sumduplcareep <- sum(duplicated(careepage12_17[, "Child ID"]) | 
                       duplicated(careepage12_17[, "Child ID"], fromLast = TRUE))

print(sumduplcareep) # 8,597 additional rows of information from a child

sumduplcla <- sum(duplicated(claage12_17[, "Child ID"]) | 
                       duplicated(claage12_17[, "Child ID"], fromLast = TRUE))

print(sumduplcla) # 7,621 additional rows of information from a child

sumduplcin <- sum(duplicated(cinage12_17[, "Child ID"]) | 
                    duplicated(cinage12_17[, "Child ID"], fromLast = TRUE))

print(sumduplcin) # 12,567 additional rows of information from a child

sumduplneet <- sum(duplicated(neetage16_20[, "Child ID"]) | 
                    duplicated(neetage16_20[, "Child ID"], fromLast = TRUE))

print(sumduplneet) # 3,325 additional rows of information from a child

