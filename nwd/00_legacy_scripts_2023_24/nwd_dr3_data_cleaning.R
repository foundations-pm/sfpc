
# SFPC impact evaluation (NWD)
# Data cleaning DR3

# Author: AA
# Date: 16/10/2023

rm(list = ls())

library(tidyverse)
library(dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(data.table)
library(ggplot2)
#install.packages("ggplot2")
library(arsenal)
#install.packages("arsenal")
library(pacman)
#install.packages("pacman")
library(naniar)
#install.packages("naniar")

# Read in DR3 files ####

# Norfolk ####

norfolk_dr3_apr23cla <- read_excel("Data/NWD_DR3/norfolk_dr3_apr23.xlsx", sheet = "DR3 - Data - CLA outcome")
norfolk_dr3_apr23careep <- read_excel("Data/NWD_DR3/norfolk_dr3_apr23.xlsx", sheet = "DR3 - Data - Care episode infor")
norfolk_dr3_apr23neet <- read_excel("Data/NWD_DR3/norfolk_dr3_apr23.xlsx", sheet = "DR3 - Data - NEET status ")
norfolk_dr3_apr23cin <- read_excel("Data/NWD_DR3/norfolk_dr3_apr23.xlsx", sheet = "Data - CIN plans ")

# Redcar ####

redcar_dr3_apr23cla <- read_excel("Data/NWD_DR3/redcar_dr3_apr23.xlsx", sheet = "DR3 - Data - CLA outcome")
redcar_dr3_apr23careep <- read_excel("Data/NWD_DR3/redcar_dr3_apr23.xlsx", sheet = "DR3 - Data - Care episode infor")
redcar_dr3_apr23neet <- read_excel("Data/NWD_DR3/redcar_dr3_apr23.xlsx", sheet = "DR3 - Data - NEET status")
redcar_dr3_apr23cin <- read_excel("Data/NWD_DR3/redcar_dr3_apr23.xlsx", sheet = "Data - CIN plans ")


# Rochdale ####

# CLA data is missing

rochdale_dr3_apr23careep <- read_excel("Data/NWD_DR3/rochdale_dr3_apr23.xlsx", sheet = "DR3 - Data - Care episode infor")
rochdale_dr3_apr23neet <- read_excel("Data/NWD_DR3/rochdale_dr3_apr23.xlsx", sheet = "DR3 - Data - NEET status")
rochdale_dr3_apr23cin <- read_excel("Data/NWD_DR3/rochdale_dr3_apr23.xlsx", sheet = "Data - CIN plans ")


# Warrington ####

warrington_dr3_apr23cla <- read_excel("Data/NWD_DR3/warrington_dr3_apr23.xlsx", sheet = "DR3 - Data - CLA outcome")
warrington_dr3_apr23careep <- read_excel("Data/NWD_DR3/warrington_dr3_apr23.xlsx", sheet = "DR3 - Data - Care episode infor")
warrington_dr3_apr23neet <- read_excel("Data/NWD_DR3/warrington_dr3_apr23.xlsx", sheet = "DR3 - Data - NEET status")
warrington_dr3_apr23cin <- read_excel("Data/NWD_DR3/warrington_dr3_apr23.xlsx", sheet = "Data - CIN plans ")


#Cleaning Data

# removes relevant rows and column

norfolk_dr3_apr23cla <- norfolk_dr3_apr23cla[-c(1,2),] # removes relevant rows
norfolk_dr3_apr23cla <- norfolk_dr3_apr23cla[,-c(4)] # removes relevant column
colnames(norfolk_dr3_apr23cla) <- norfolk_dr3_apr23cla[1,]
norfolk_dr3_apr23cla <- norfolk_dr3_apr23cla[-1,]

norfolk_dr3_apr23careep <- norfolk_dr3_apr23careep[-c(2),] # removes relevant rows
norfolk_dr3_apr23careep <- norfolk_dr3_apr23careep[,-c(11,12)] # removes relevant column
colnames(norfolk_dr3_apr23careep) <- norfolk_dr3_apr23careep[1,]
norfolk_dr3_apr23careep <- norfolk_dr3_apr23careep[-1,]

colnames(norfolk_dr3_apr23neet) <- norfolk_dr3_apr23neet[1,]
norfolk_dr3_apr23neet <- norfolk_dr3_apr23neet[-1,]

norfolk_dr3_apr23cin <- norfolk_dr3_apr23cin[-c(1:4),] # removes relevant rows
colnames(norfolk_dr3_apr23cin) <- norfolk_dr3_apr23cin[1,]
norfolk_dr3_apr23cin <- norfolk_dr3_apr23cin[-1,]

redcar_dr3_apr23cla <- redcar_dr3_apr23cla[-c(1,2),] # removes relevant rows
colnames(redcar_dr3_apr23cla) <- redcar_dr3_apr23cla[1,]
redcar_dr3_apr23cla <- redcar_dr3_apr23cla[-1,]

redcar_dr3_apr23careep <- redcar_dr3_apr23careep[-c(1),] # removes relevant rows

colnames(redcar_dr3_apr23neet) <- redcar_dr3_apr23neet[1,]
redcar_dr3_apr23neet <- redcar_dr3_apr23neet[-1,]

redcar_dr3_apr23cin <- redcar_dr3_apr23cin[-c(1:4),] # removes relevant rows
colnames(redcar_dr3_apr23cin) <- redcar_dr3_apr23cin[1,]
redcar_dr3_apr23cin <- redcar_dr3_apr23cin[-1,]

#Rochdale CLA data is missing

colnames(rochdale_dr3_apr23neet) <- rochdale_dr3_apr23neet[1,]
rochdale_dr3_apr23neet <- rochdale_dr3_apr23neet[-1,]

rochdale_dr3_apr23cin <- rochdale_dr3_apr23cin[-c(1:4),] # removes relevant rows
colnames(rochdale_dr3_apr23cin) <- rochdale_dr3_apr23cin[1,]
rochdale_dr3_apr23cin <- rochdale_dr3_apr23cin[-1,]

warrington_dr3_apr23cla <- warrington_dr3_apr23cla[-c(1,2),] # removes relevant rows
colnames(warrington_dr3_apr23cla) <- warrington_dr3_apr23cla[1,]
warrington_dr3_apr23cla <- warrington_dr3_apr23cla[-1,]

warrington_dr3_apr23careep <- warrington_dr3_apr23careep[,-c(3)] # removes relevant column

colnames(warrington_dr3_apr23neet) <- warrington_dr3_apr23neet[1,]
warrington_dr3_apr23neet <- warrington_dr3_apr23neet[-1,]

warrington_dr3_apr23cin <- warrington_dr3_apr23cin[-c(1:4),] # removes relevant rows
colnames(warrington_dr3_apr23cin) <- warrington_dr3_apr23cin[1,]
warrington_dr3_apr23cin <- warrington_dr3_apr23cin[-1,]

# check for duplicate rows

sum(duplicated(norfolk_dr3_apr23careep))
duplicated(rochdale_dr3_apr23careep)

# duplicated rows omitted

redcar_dr3_apr23cin <- redcar_dr3_apr23cin[-which(duplicated(redcar_dr3_apr23cin)), ]
rochdale_dr3_apr23careep <- rochdale_dr3_apr23careep[-which(duplicated(rochdale_dr3_apr23careep)), ]
rochdale_dr3_apr23cin <- rochdale_dr3_apr23cin[-which(duplicated(rochdale_dr3_apr23cin)), ]

# Missing data

norfolk_dr3_apr23careep <- norfolk_dr3_apr23careep %>% 
  mutate_all(na_if, "NULL")
stopifnot(sum(norfolk_dr3_apr23careep=="", na.rm=TRUE)==0)

norfolk_dr3_apr23neet <- norfolk_dr3_apr23neet %>% 
  mutate_all(na_if, "NULL")
stopifnot(sum(norfolk_dr3_apr23neet=="", na.rm=TRUE)==0)

# Some columns recoded from character to numeric and factor and change excel error to date

norfolk_dr3_apr23cla$`Date period of care commenced` <- as.numeric(norfolk_dr3_apr23cla$`Date period of care commenced`)
norfolk_dr3_apr23cla$`Date period of care commenced` <- as.Date(norfolk_dr3_apr23cla$`Date period of care commenced`, origin = "1899-12-30")
norfolk_dr3_apr23cin$`Child referral Date` <- as.numeric(norfolk_dr3_apr23cin$`Child referral Date`)
norfolk_dr3_apr23cin$`Child referral Date` <- as.Date(norfolk_dr3_apr23cin$`Child referral Date`, origin = "1899-12-30")
norfolk_dr3_apr23cin$`CIN Closure Date` <- as.numeric(norfolk_dr3_apr23cin$`CIN Closure Date`)
norfolk_dr3_apr23cin$`CIN Closure Date` <- as.Date(norfolk_dr3_apr23cin$`CIN Closure Date`, origin = "1899-12-30")
norfolk_dr3_apr23careep$`Date period of care commenced` <- as.numeric(norfolk_dr3_apr23careep$`Date period of care commenced`)
norfolk_dr3_apr23careep$`Date period of care commenced` <- as.Date(norfolk_dr3_apr23careep$`Date period of care commenced`, origin = "1899-12-30")
norfolk_dr3_apr23careep$`Date episode commenced` <- as.numeric(norfolk_dr3_apr23careep$`Date episode commenced`)
norfolk_dr3_apr23careep$`Date episode commenced` <- as.Date(norfolk_dr3_apr23careep$`Date episode commenced`, origin = "1899-12-30")
norfolk_dr3_apr23careep$`Date episode ceased` <- as.numeric(norfolk_dr3_apr23careep$`Date episode ceased`)
norfolk_dr3_apr23careep$`Date episode ceased` <- as.Date(norfolk_dr3_apr23careep$`Date episode ceased`, origin = "1899-12-30")
norfolk_dr3_apr23careep$`Date period of care ended` <- as.numeric(norfolk_dr3_apr23careep$`Date period of care ended`)
norfolk_dr3_apr23careep$`Date period of care ended` <- as.Date(norfolk_dr3_apr23careep$`Date period of care ended`, origin = "1899-12-30")
norfolk_dr3_apr23neet$`Period of care end date (date left care)` <- as.numeric(norfolk_dr3_apr23neet$`Period of care end date (date left care)`)
norfolk_dr3_apr23neet$`Period of care end date (date left care)` <- as.Date(norfolk_dr3_apr23neet$`Period of care end date (date left care)`, origin = "1899-12-30")

redcar_dr3_apr23neet$`Period of care end date (date left care)` <- as.numeric(redcar_dr3_apr23neet$`Period of care end date (date left care)`)
redcar_dr3_apr23neet$`Period of care end date (date left care)` <- as.Date(redcar_dr3_apr23neet$`Period of care end date (date left care)`, origin = "1899-12-30")
redcar_dr3_apr23cin$`Child referral Date` <- as.numeric(redcar_dr3_apr23cin$`Child referral Date`)
redcar_dr3_apr23cin$`Child referral Date` <- as.Date(redcar_dr3_apr23cin$`Child referral Date`, origin = "1899-12-30")
redcar_dr3_apr23cin$`CIN Closure Date` <- as.numeric(redcar_dr3_apr23cin$`CIN Closure Date`)
redcar_dr3_apr23cin$`CIN Closure Date` <- as.Date(redcar_dr3_apr23cin$`CIN Closure Date`, origin = "1899-12-30")
redcar_dr3_apr23cla$`Date period of care commenced` <- as.numeric(redcar_dr3_apr23cla$`Date period of care commenced`)
redcar_dr3_apr23cla$`Date period of care commenced` <- as.Date(redcar_dr3_apr23cla$`Date period of care commenced`, origin = "1899-12-30")

rochdale_dr3_apr23neet$`Period of care end date (date left care)` <- as.numeric(rochdale_dr3_apr23neet$`Period of care end date (date left care)`)
rochdale_dr3_apr23neet$`Period of care end date (date left care)` <- as.Date(rochdale_dr3_apr23neet$`Period of care end date (date left care)`, origin = "1899-12-30")
str(rochdale_dr3_apr23cin$`Child referral Date`)
rochdale_dr3_apr23cin$`Child referral Date` <- 
  as.Date(paste("01", rochdale_dr3_apr23cin$`Child referral Date`, 
                sep = "/"), format = "%d/%m/%Y")

warrington_dr3_apr23neet$`Period of care end date (date left care)` <- as.numeric(warrington_dr3_apr23neet$`Period of care end date (date left care)`)
warrington_dr3_apr23neet$`Period of care end date (date left care)` <- as.Date(warrington_dr3_apr23neet$`Period of care end date (date left care)`, origin = "1899-12-30")
warrington_dr3_apr23cin$`Child referral Date` <- as.numeric(warrington_dr3_apr23cin$`Child referral Date`)
warrington_dr3_apr23cin$`Child referral Date` <- as.Date(warrington_dr3_apr23cin$`Child referral Date`, origin = "1899-12-30")
warrington_dr3_apr23cin$`CIN Closure Date` <- as.numeric(warrington_dr3_apr23cin$`CIN Closure Date`)
warrington_dr3_apr23cin$`CIN Closure Date` <- as.Date(warrington_dr3_apr23cin$`CIN Closure Date`, origin = "1899-12-30")
warrington_dr3_apr23cla$`Date period of care commenced` <- as.numeric(warrington_dr3_apr23cla$`Date period of care commenced`)
warrington_dr3_apr23cla$`Date period of care commenced` <- as.Date(warrington_dr3_apr23cla$`Date period of care commenced`, origin = "1899-12-30")

# subset data to align with year of activity

# Create new variable 'days spent in care'.

norfolk_dr3_apr23careep$daysspentincare <- as.numeric(difftime(as.Date(norfolk_dr3_apr23careep$`Date period of care ended`),as.Date(norfolk_dr3_apr23careep$`Date period of care commenced`), units = "days"))
redcar_dr3_apr23careep$daysspentincare <- as.numeric(difftime(as.Date(redcar_dr3_apr23careep$`Date period of care ended`),as.Date(redcar_dr3_apr23careep$`Date period of care commenced`), units = "days"))
rochdale_dr3_apr23careep$daysspentincare <- as.numeric(difftime(as.Date(rochdale_dr3_apr23careep$`Date period of care ended`),as.Date(rochdale_dr3_apr23careep$`Date period of care commenced`), units = "days"))
warrington_dr3_apr23careep$daysspentincare <- as.numeric(difftime(as.Date(warrington_dr3_apr23careep$`Date period of care ended`),as.Date(warrington_dr3_apr23careep$`Date period of care commenced`), units = "days"))

# Change column names

colnames(norfolk_dr3_apr23careep)[11]  <- "Days spent in care"
colnames(redcar_dr3_apr23careep)[11]  <- "Days spent in care"
colnames(rochdale_dr3_apr23careep)[11]  <- "Days spent in care"
colnames(warrington_dr3_apr23careep)[11]  <- "Days spent in care"
colnames(warrington_dr3_apr23neet)[1]  <- "MainactivityProcessingyear"
colnames(warrington_dr3_apr23neet)[4]  <- "Mainactivity"
colnames(warrington_dr3_apr23cin)[2]  <- "Referral ID"

colnames(warrington_dr3_apr23neet)

#Pivot data

warrington_dr3_apr23neet <- warrington_dr3_apr23neet %>% pivot_wider(names_from = MainactivityProcessingyear, values_from = Mainactivity)

# Change column names

colnames(warrington_dr3_apr23neet)[6]  <- "Main activity -\r\nProcessing year 2020"
colnames(warrington_dr3_apr23neet)[5]  <- "Main activity - \r\nProcessing year 2021"
colnames(warrington_dr3_apr23neet)[4]  <- "Main activity -\r\nProcessing year 2022"
colnames(warrington_dr3_apr23neet)[3]  <- "Main activity -\r\nProcessing year 2023"

colnames(redcar_dr3_apr23neet)

# Change order of columns

col_order <- c("Child ID", "Period of care end date (date left care)", "Main activity -\r\nProcessing year 2020", "Main activity - \r\nProcessing year 2021", "Main activity -\r\nProcessing year 2022", "Main activity -\r\nProcessing year 2023")
warrington_dr3_apr23neet <- warrington_dr3_apr23neet[, col_order]

# Are we imputing NULL or turning them into NA?

# Adding a variable/column to show CIN dataframes come from a specific LA. This will need to be clear in the full merge across LAs.

LA = c("Norfolk")
norfolk_dr3_apr23cin$LA <- "Norfolk"
norfolk_dr3_apr23cin <- norfolk_dr3_apr23cin %>% relocate(LA)

LA = c("Redcar")
redcar_dr3_apr23cin$LA <- "Redcar"
redcar_dr3_apr23cin <- redcar_dr3_apr23cin %>% relocate(LA)

LA = c("Rochdale")
rochdale_dr3_apr23cin$LA <- "Rochdale"
rochdale_dr3_apr23cin <- rochdale_dr3_apr23cin %>% relocate(LA)

LA = c("Warrington")
warrington_dr3_apr23cin$LA <- "Warrington"
warrington_dr3_apr23cin <- warrington_dr3_apr23cin %>% relocate(LA)

# Merging LA CIN datasets

all_cin_dr3 <- rbind(norfolk_dr3_apr23cin, redcar_dr3_apr23cin, rochdale_dr3_apr23cin, warrington_dr3_apr23cin)

# save binded dr3 dataset

save(all_cin_dr3, file = "Output/alldr3cin.RData")

# Adding a variable/column to show Care Exp dataframes come from a specific LA. This will need to be clear in the full merge across LAs.

LA = c("Norfolk")
norfolk_dr3_apr23careep$LA <- "Norfolk"
norfolk_dr3_apr23careep <- norfolk_dr3_apr23careep %>% relocate(LA)

LA = c("Redcar")
redcar_dr3_apr23careep$LA <- "Redcar"
redcar_dr3_apr23careep <- redcar_dr3_apr23careep %>% relocate(LA)

LA = c("Rochdale")
rochdale_dr3_apr23careep$LA <- "Rochdale"
rochdale_dr3_apr23careep <- rochdale_dr3_apr23careep %>% relocate(LA)

LA = c("Warrington")
warrington_dr3_apr23careep$LA <- "Warrington"
warrington_dr3_apr23careep <- warrington_dr3_apr23careep %>% relocate(LA)

# Merging LA Care Exp datasets

all_careep_dr3 <- rbind(norfolk_dr3_apr23careep, redcar_dr3_apr23careep, rochdale_dr3_apr23careep, warrington_dr3_apr23careep)

# save binded dr3 dataset

save(all_careep_dr3, file = "Output/alldr3careep.RData")

# Adding a variable/column to show Care Exp dataframes come from a specific LA. This will need to be clear in the full merge across LAs.
# CLA data does not exist for Rochdale

LA = c("Norfolk")
norfolk_dr3_apr23cla$LA <- "Norfolk"
norfolk_dr3_apr23cla <- norfolk_dr3_apr23cla %>% relocate(LA)

LA = c("Redcar")
redcar_dr3_apr23cla$LA <- "Redcar"
redcar_dr3_apr23cla <- redcar_dr3_apr23cla %>% relocate(LA)

LA = c("Warrington")
warrington_dr3_apr23cla$LA <- "Warrington"
warrington_dr3_apr23cla <- warrington_dr3_apr23cla %>% relocate(LA)

# Merging LA Care Exp datasets

all_cla_dr3 <- rbind(norfolk_dr3_apr23cla, redcar_dr3_apr23cla, warrington_dr3_apr23cla)

# save binded dr3 dataset

save(all_cla_dr3, file = "Output/alldr3cla.RData")

# Adding a variable/column to show Care Exp dataframes come from a specific LA. This will need to be clear in the full merge across LAs.

LA = c("Norfolk")
norfolk_dr3_apr23neet$LA <- "Norfolk"
norfolk_dr3_apr23neet <- norfolk_dr3_apr23neet %>% relocate(LA)

LA = c("Redcar")
redcar_dr3_apr23neet$LA <- "Redcar"
redcar_dr3_apr23neet <- redcar_dr3_apr23neet %>% relocate(LA)

LA = c("Rochdale")
rochdale_dr3_apr23neet$LA <- "Rochdale"
rochdale_dr3_apr23neet <- rochdale_dr3_apr23neet %>% relocate(LA)

LA = c("Warrington")
warrington_dr3_apr23neet$LA <- "Warrington"
warrington_dr3_apr23neet <- warrington_dr3_apr23neet %>% relocate(LA)

# Merging LA Care Exp datasets

all_neet_dr3 <- rbind.fill(norfolk_dr3_apr23neet, redcar_dr3_apr23neet, rochdale_dr3_apr23neet, warrington_dr3_apr23neet)

# save binded dr3 dataset

save(all_neet_dr3, file = "Output/alldr3neet.RData")

#DR3 Missing data

# Missing CIN data

#Percent of rows with any value missing
pct_miss_case(all_cin_dr3)  #49.9%

# Visual to see missingness
gg_miss_var(all_cin_dr3, show_pct = TRUE)

# Visual to see missingness by LA
all_cin_dr3 %>% 
  gg_miss_var(show_pct = TRUE, facet = `LA`)
ggsave("Output/allcindr3missing.png", width = 8, height = 6, units = "in")

# Missing care exp data

#Percent of rows with any value missing
pct_miss_case(all_careep_dr3)  #43.2%

# Visual to see missingness
gg_miss_var(all_careep_dr3, show_pct = TRUE)

# Visual to see missingness by LA
all_careep_dr3 %>% 
  gg_miss_var(show_pct = TRUE, facet = `LA`)
ggsave("Output/allcareepdr3missing.png", width = 8, height = 6, units = "in")

# Missing CLA data

#Percent of rows with any value missing
pct_miss_case(all_cla_dr3)  #0%

# Visual to see missingness
gg_miss_var(all_cla_dr3, show_pct = TRUE)

# Visual to see missingness by LA
all_cla_dr3 %>% 
  gg_miss_var(show_pct = TRUE, facet = `LA`)
ggsave("Output/allcladr3missing.png", width = 8, height = 6, units = "in")

# Missing NEET data

#Percent of rows with any value missing
pct_miss_case(all_neet_dr3)  #95.3%

# Visual to see missingness
gg_miss_var(all_neet_dr3, show_pct = TRUE)

# Visual to see missingness by LA
all_neet_dr3 %>% 
  gg_miss_var(show_pct = TRUE, facet = `LA`)
ggsave("Output/allneetdr3missing.png", width = 8, height = 6, units = "in")



