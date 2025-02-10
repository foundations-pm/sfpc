# SFPC impact evaluation (NWD)
# Data cleaning DR1

# Author: AA, DR
# Date: 26/06/2023
#   modified by DR: 30/08/23 (just changing WD)

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

# set working directory
# setwd("Script/")
  #now that this is in a project, there is no longer a need to set the WD


# Read in dr1 files ####
# Leicester ####

leicester_dr1_apr21 <- read_excel("Data/NWD_DR1/leicester_dr1_apr21.xlsx", sheet = "DR1 - Data - aggregate level")
leicester_dr1_apr22 <- read_excel("Data/NWD_DR1/leicester_dr1_apr22.xlsx", sheet = "DR1 - Data - aggregate level")
leicester_dr1_nov21 <- read_excel("Data/NWD_DR1/leicester_dr1_nov21.xlsx", sheet = "DR1 - Data - aggregate level")
leicester_dr1_nov20 <- read_excel("Data/NWD_DR1/leicester_dr1_nov20.xlsx", sheet = "DR1 - Data - aggregate level")

# Norfolk ####

norfolk_dr1_apr21 <- read_excel("Data/NWD_DR1/norfolk_dr1_apr21.xlsx", sheet = "DR1 - Data - aggregate level")
norfolk_dr1_apr22 <- read_excel("Data/NWD_DR1/norfolk_dr1_apr22.xlsx", sheet = "DR1 - Data - aggregate level")
norfolk_dr1_nov20 <- read_excel("Data/NWD_DR1/norfolk_dr1_nov20.xlsx", sheet = "DR1 - Data - aggregate level")
norfolk_dr1_nov21 <- read_excel("Data/NWD_DR1/norfolk_dr1_nov21.xlsx", sheet = "DR1 - Data - aggregate level")
norfolk_dr1_nov22 <- read_excel("Data/NWD_DR1/norfolk_dr1_nov22.xlsx", sheet = "DR1 - Data - aggregate level")

# Redcar ####

redcar_dr1_apr21 <- read_excel("Data/NWD_DR1/redcar_dr1_apr21.xlsx", sheet = "DR1 - Data - aggregate level")
redcar_dr1_apr22 <- read_excel("Data/NWD_DR1/redcar_dr1_apr22.xlsx", sheet = "DR1 - Data - aggregate level")
redcar_dr1_nov21 <- read_excel("Data/NWD_DR1/redcar_dr1_nov21.xlsx", sheet = "DR1 - Data - aggregate level")
redcar_dr1_nov22 <- read_excel("Data/NWD_DR1/redcar_dr1_nov22.xlsx", sheet = "DR1 - Data - aggregate level")

# Rochdale ####

rochdale_dr1_apr21 <- read_excel("Data/NWD_DR1/rochdale_dr1_apr21.xlsx", sheet = "DR1 - Data - aggregate level")
rochdale_dr1_apr22 <- read_excel("Data/NWD_DR1/rochdale_dr1_apr22.xlsx", sheet = "DR1 - Data - aggregate level")
rochdale_dr1_nov20 <- read_excel("Data/NWD_DR1/rochdale_dr1_nov20.xlsx", sheet = "Sheet1")
rochdale_dr1_nov21 <- read_excel("Data/NWD_DR1/rochdale_dr1_nov21.xlsx", sheet = "DR1 - Data - aggregate level")
rochdale_dr1_nov22 <- read_excel("Data/NWD_DR1/rochdale_dr1_nov22.xlsx", sheet = "DR1 - Data - aggregate level")

# Warrington ####

warrington_dr1_apr21 <- read_excel("Data/NWD_DR1/warrington_dr1_apr21.xlsx", sheet = "DR1 - Data - aggregate level")
warrington_dr1_apr22 <- read_excel("Data/NWD_DR1/warrington_dr1_apr22.xlsx", sheet = "DR1 - Data - aggregate level")
warrington_dr1_nov20 <- read_excel("Data/NWD_DR1/warrington_dr1_nov20.xlsx", sheet = "DR1 - Data - aggregate level")
warrington_dr1_nov21 <- read_excel("Data/NWD_DR1/warrington_dr1_nov21.xlsx", sheet = "DR1 - Data - aggregate level")
warrington_dr1_nov22 <- read_excel("Data/NWD_DR1/warrington_dr1_nov22.xlsx", sheet = "DR1 - Data - aggregate level")

# Cleaning data ####

leicester_dr1_nov20 <- leicester_dr1_nov20[-c(3,4,6)]
norfolk_dr1_nov20 <- norfolk_dr1_nov20[-c(3,4,5,6,8)]
rochdale_dr1_nov20 <- rochdale_dr1_nov20[-c(1,2,5,6,7,8,9,10,11,12,14,20,21)]
warrington_dr1_apr21 <- warrington_dr1_apr21[-c(4)]
warrington_dr1_nov20 <- warrington_dr1_nov20[-c(3,4,6,7,12,14)]

# Due to an issue with names not being the same with the rochdale spreadsheets the rbind function would not run. Therefore,
# using the columns from other spreadsheets the column names were aligned to allow rbind function to work.

colnames(rochdale_dr1_nov22) <- colnames(leicester_dr1_apr21)    # Change column names
colnames(rochdale_dr1_nov20) <- colnames(leicester_dr1_apr21)    # Change column names
colnames(rochdale_dr1_nov21) <- colnames(leicester_dr1_apr21)    # Change column names
colnames(rochdale_dr1_apr22) <- colnames(leicester_dr1_apr21)    # Change column names
colnames(rochdale_dr1_apr21) <- colnames(leicester_dr1_apr22)    # Change column names

# Some cells included characters recoded to NA

leicester_dr1_apr21$`Number of assessments completed (by CSC)`[leicester_dr1_apr21$`Number of assessments completed (by CSC)` == "October 2019 - March 2020 already submitted in the previous data request - please only submit data for October 2020 - March 2021"] <- NA
leicester_dr1_apr21$`Number of CPPs that started this month in the LA`[leicester_dr1_apr21$`Number of CPPs that started this month in the LA` == "October 2019 - March 2020 already submitted in the previous data request - please only submit data for October 2020 - March 2021"] <- NA
norfolk_dr1_apr22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`[norfolk_dr1_apr22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` == "To follow"] <- NA
redcar_dr1_apr21$`Number of assessments completed (by CSC)`[redcar_dr1_apr21$`Number of assessments completed (by CSC)` == "October 2019 - March 2020 already submitted in the previous data request - please only submit data for October 2020 - March 2021"] <- NA
redcar_dr1_apr21$`Number of CPPs that started this month in the LA`[redcar_dr1_apr21$`Number of CPPs that started this month in the LA` == "October 2019 - March 2020 already submitted in the previous data request - please only submit data for October 2020 - March 2021"] <- NA
rochdale_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`[rochdale_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` == "N/A"] <- NA
warrington_dr1_apr21$`Number of assessments completed (by CSC)`[warrington_dr1_apr21$`Number of assessments completed (by CSC)` == "October 2019 - March 2020 already submitted in the previous data request - please only submit data for October 2020 - March 2021"] <- NA
warrington_dr1_apr21$`Number of CPPs that started this month in the LA`[warrington_dr1_apr21$`Number of CPPs that started this month in the LA` == "October 2019 - March 2020 already submitted in the previous data request - please only submit data for October 2020 - March 2021"] <- NA

# Some cells included characters and full percentages recoded to percentages with decimals like other data in the column

norfolk_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`[norfolk_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` == "January 2019 - 14.35%"] <- ".1435"
norfolk_dr1_nov21$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`[norfolk_dr1_nov21$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` == "19.3% in January 2021"] <- ".193"
norfolk_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`[norfolk_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` == "21.1% Spring 2022"] <- ".211"
rochdale_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`[rochdale_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` == "Oct 2020: 25.1%"] <- ".251"
rochdale_dr1_apr22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`[rochdale_dr1_apr22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` == "31.1.%"] <- ".311"

# Some columns recoded from character to numeric

class(norfolk_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)
norfolk_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` <- as.numeric(norfolk_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)

class(norfolk_dr1_nov21$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)
norfolk_dr1_nov21$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` <- as.numeric(norfolk_dr1_nov21$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)

class(norfolk_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)
norfolk_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` <- as.numeric(norfolk_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)

rochdale_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` <- as.numeric(rochdale_dr1_nov20$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)
rochdale_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` <- as.numeric(rochdale_dr1_nov22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)
rochdale_dr1_apr22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` <- as.numeric(rochdale_dr1_apr22$`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`)

# Month recoded to Date from POSIXct

# Check the class of the Month variable
month_class <- class(leicester_dr1_nov20$Month)
cat("Class of Month variable in", leicester_dr1_nov20$Month, ":", month_class, "\n")
month_class <- class(leicester_dr1_nov21$Month)
cat("Class of Month variable in", leicester_dr1_nov21$Month, ":", month_class, "\n")
month_class <- class(leicester_dr1_apr21$Month)
cat("Class of Month variable in", leicester_dr1_apr21$Month, ":", month_class, "\n")
month_class <- class(leicester_dr1_apr22$Month)
cat("Class of Month variable in", leicester_dr1_apr22$Month, ":", month_class, "\n")
# All files have month saved as a time and date: POSIXct POSIXt

# Define the format string
format_string <- "%B-%y"  # %B represents the full month name, %y represents the two-digit year
# Parse the date column using the specified format
leicester_dr1_nov20$Month <- as.Date(leicester_dr1_nov20$Month, format = format_string)  
leicester_dr1_nov20$Month <- as.Date(leicester_dr1_nov20$Month, format = "%mm-%YY")
leicester_dr1_nov21$Month <- as.Date(leicester_dr1_nov21$Month, format = format_string)  
leicester_dr1_nov21$Month <- as.Date(leicester_dr1_nov21$Month, format = "%mm-%YY")
leicester_dr1_apr21$Month <- as.Date(leicester_dr1_apr21$Month, format = format_string)  
leicester_dr1_apr21$Month <- as.Date(leicester_dr1_apr21$Month, format = "%mm-%YY")
leicester_dr1_apr22$Month <- as.Date(leicester_dr1_apr22$Month, format = format_string)  
leicester_dr1_apr22$Month <- as.Date(leicester_dr1_apr22$Month, format = "%mm-%YY")
class(leicester_dr1_nov20$Month)
class(leicester_dr1_nov21$Month)
class(leicester_dr1_apr21$Month)
class(leicester_dr1_apr22$Month)

# Change whole numbers to decimal proportions

leicester_dr1_apr22 <- leicester_dr1_apr22 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

leicester_dr1_nov21 <- leicester_dr1_nov21 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

redcar_dr1_apr22 <- redcar_dr1_apr22 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

rochdale_dr1_nov22 <- rochdale_dr1_nov22 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

warrington_dr1_apr21 <- warrington_dr1_apr21 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

warrington_dr1_apr22 <- warrington_dr1_apr22 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

warrington_dr1_nov20 <- warrington_dr1_nov20 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

warrington_dr1_nov21 <- warrington_dr1_nov21 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

warrington_dr1_nov22 <- warrington_dr1_nov22 %>% 
  mutate(`Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.`=
           `Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.` 
         / 100)

# Merging LA datasets

all_leic_dr1 <- rbind(leicester_dr1_nov20,leicester_dr1_nov21,leicester_dr1_apr21,leicester_dr1_apr22)
all_norf_dr1 <- rbind(norfolk_dr1_nov20,norfolk_dr1_nov21,norfolk_dr1_nov22, norfolk_dr1_apr21,norfolk_dr1_apr22)
all_redcar_dr1 <- rbind(redcar_dr1_nov21,redcar_dr1_nov22,redcar_dr1_apr21,redcar_dr1_apr22)
all_roch_dr1 <- rbind(rochdale_dr1_nov20,rochdale_dr1_nov21,rochdale_dr1_nov22,rochdale_dr1_apr21,rochdale_dr1_apr22)
all_warr_dr1 <- rbind(warrington_dr1_nov20,warrington_dr1_nov21,warrington_dr1_nov22,warrington_dr1_apr21,warrington_dr1_apr22)

# Shorten column names

colnames(all_leic_dr1)[3]  <- "Proportion children eligible and claiming for FSM"
colnames(all_leic_dr1)[5]  <- "CLA at end of the month"
colnames(all_leic_dr1)[6]  <- "New CLA this month"
colnames(all_leic_dr1)[7]  <- "CIN plans started this month"
colnames(all_leic_dr1)[8]  <- "Open CIN cases this month"
colnames(all_leic_dr1)[9]  <- "CPPs started started this month"
colnames(all_leic_dr1)[10]  <- "Open CPPs this month"
colnames(all_leic_dr1)[11]  <- "New referrals this month"

colnames(all_norf_dr1) <- colnames(all_leic_dr1)
colnames(all_redcar_dr1) <- colnames(all_leic_dr1) 
colnames(all_roch_dr1) <- colnames(all_leic_dr1) 
colnames(all_warr_dr1) <- colnames(all_leic_dr1) 


# Adding a variable/column to show this dataframe comes from Lancashire. This will need to be clear in the full merge across LAs.

LA = c("Leicester")
all_leic_dr1$LA <- "Leicester"
all_leic_dr1 <- all_leic_dr1 %>% relocate(LA)

LA = c("Norfolk")
all_norf_dr1$LA <- "Norfolk"
all_norf_dr1 <- all_norf_dr1 %>% relocate(LA)

LA = c("Redcar")
all_redcar_dr1$LA <- "Redcar"
all_redcar_dr1 <- all_redcar_dr1 %>% relocate(LA)

LA = c("Rochdale")
all_roch_dr1$LA <- "Rochdale"
all_roch_dr1 <- all_roch_dr1 %>% relocate(LA)

LA = c("Warrington")
all_warr_dr1$LA <- "Warrington"
all_warr_dr1 <- all_warr_dr1 %>% relocate(LA)

# impute FSM missing data

all_leic_dr1 <- all_leic_dr1 %>% 
  arrange(`Month`) %>%
  fill(`Proportion children eligible and claiming for FSM`, 
       .direction = "downup")

all_norf_dr1 <- all_norf_dr1 %>% 
  arrange(`Month`) %>%
  fill(`Proportion children eligible and claiming for FSM`, 
       .direction = "downup")

all_redcar_dr1 <- all_redcar_dr1 %>% 
  arrange(`Month`) %>%
  fill(`Proportion children eligible and claiming for FSM`, 
       .direction = "downup")

all_roch_dr1 <- all_roch_dr1 %>% 
  arrange(`Month`) %>%
  fill(`Proportion children eligible and claiming for FSM`, 
       .direction = "downup")

all_warr_dr1 <- all_warr_dr1 %>% 
  arrange(`Month`) %>%
  fill(`Proportion children eligible and claiming for FSM`, 
       .direction = "downup")

# merge all LA dr1 data

all_dr1 <- rbind(all_leic_dr1,all_norf_dr1,all_redcar_dr1,all_roch_dr1,all_warr_dr1)


