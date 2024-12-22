# SFPC impact evaluation (NWD)
# Data cleaning DR2 CLA

# Author: AA
# Date: 26/07/2023
# Edited DR 30/08/2023 to update file paths

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
#install.packages("lubridate")
library(lubridate)
#install.packages("plyr")
library(plyr)
library(writexl)
#install.packages("writexl")
library(openxlsx)
#install.packages('openxlsx')

# Read in DR2 files ####
# Leicester ####

leicester_dr2_nov20 <- read_excel("Data/NWD_DR2/leicester_dr2_nov20.xlsx", 
                                  sheet = "DR2 - Data - sample pop CLA",
                                  col_types = c("guess","guess","guess","guess","guess","text",
                                                "guess","guess","guess","guess","guess"))

leicester_dr2_apr22 <- read_excel("Data/NWD_DR2/leicester_dr2_apr22.xlsx", 
                                  sheet = "DR2 - Data - sample pop CLA",
                                  col_types = c("guess","guess","guess","guess","guess","text",
                                                "guess","guess","guess","guess","guess"))

# Norfolk ####

norfolk_dr2_nov20 <- read_excel("Data/NWD_DR2/norfolk_dr2_nov20.xlsx", 
                                  sheet = "DR2 - Data - sample pop CLA",
                                  col_types = c("guess","guess","guess","guess","guess","text",
                                                "guess","guess","guess","guess","guess"))

norfolk_dr2_apr22 <- read_excel("Data/NWD_DR2/norfolk_dr2_apr22.xlsx", 
                                  sheet = "DR2 - Data - sample pop CLA",
                                  col_types = c("guess","guess","guess","guess","guess","text",
                                                "guess","guess","guess","guess","guess"))

# Redcar ####

redcar_dr2_nov20 <- read_excel("Data/NWD_DR2/redcar_dr2_nov20.xlsx", 
                                sheet = "DR2 - Data - sample pop CLA",
                                col_types = c("guess","guess","guess","guess","guess","text",
                                              "guess","guess","guess","guess","guess"))

redcar_dr2_apr22 <- read_excel("Data/NWD_DR2/redcar_dr2_apr22.xlsx", 
                                sheet = "DR2 - Data - sample pop CLA",
                                col_types = c("guess","guess","guess","guess","guess","text",
                                              "guess","guess","guess","guess","guess"))

# Rochdale ####

rochdale_dr2_nov20 <- read_excel("Data/NWD_DR2/rochdale_dr2_nov20cla.xlsx", 
                                sheet = "Report 1",
                                col_types = c("guess","guess","guess","guess","guess","text",
                                              "guess","guess","guess"))

rochdale_dr2_apr22 <- read_excel("Data/NWD_DR2/rochdale_dr2_apr22.xlsx", 
                                sheet = "DR2 - Data - sample pop CLA",
                                col_types = c("guess","guess","guess","guess","guess","text",
                                              "guess","guess","guess","guess","guess"))

# Warrington ####

warrington_dr2_nov20 <- read_excel("Data/NWD_DR2/warrington_dr2_nov20.xlsx", 
                                sheet = "DR2 - Data - sample pop CLA",
                                col_types = c("guess","guess","guess","guess","guess","text",
                                              "guess","guess","guess"))

warrington_dr2_apr22 <- read_excel("Data/NWD_DR2/warrington_dr2_apr22.xlsx", 
                                   sheet = "DR2 - Data - sample pop CLA",
                                   col_types = c("guess","guess","guess","guess","guess","text",
                                                 "guess","guess","guess","guess","guess"))

# Cleaning Data

# removes relevant rows

leicester_dr2_nov20 <- leicester_dr2_nov20[-c(1,2),] # removes relevant rows
colnames(leicester_dr2_nov20) <- leicester_dr2_nov20[1,]
leicester_dr2_nov20 <- leicester_dr2_nov20[-1,]

leicester_dr2_apr22 <- leicester_dr2_apr22[-c(1,2),] # removes relevant rows
colnames(leicester_dr2_apr22) <- leicester_dr2_apr22[1,]
leicester_dr2_apr22 <- leicester_dr2_apr22[-1,]

norfolk_dr2_nov20 <- norfolk_dr2_nov20[-c(1,2),] # removes relevant rows
colnames(norfolk_dr2_nov20) <- norfolk_dr2_nov20[1,]
norfolk_dr2_nov20 <- norfolk_dr2_nov20[-1,]

norfolk_dr2_apr22 <- norfolk_dr2_apr22[-c(1,2),] # removes relevant rows
colnames(norfolk_dr2_apr22) <- norfolk_dr2_apr22[1,]
norfolk_dr2_apr22 <- norfolk_dr2_apr22[-1,]

redcar_dr2_nov20 <- redcar_dr2_nov20[-c(1,2),] # removes relevant rows
colnames(redcar_dr2_nov20) <- redcar_dr2_nov20[1,]
redcar_dr2_nov20 <- redcar_dr2_nov20[-1,]

redcar_dr2_apr22 <- redcar_dr2_apr22[-c(1,2),] # removes relevant rows
colnames(redcar_dr2_apr22) <- redcar_dr2_apr22[1,]
redcar_dr2_apr22 <- redcar_dr2_apr22[-1,]

rochdale_dr2_apr22 <- rochdale_dr2_apr22[-c(1,2),] # removes relevant rows
colnames(rochdale_dr2_apr22) <- rochdale_dr2_apr22[1,]
rochdale_dr2_apr22 <- rochdale_dr2_apr22[-1,]

warrington_dr2_nov20 <- warrington_dr2_nov20[-c(1,2),] # removes relevant rows
colnames(warrington_dr2_nov20) <- warrington_dr2_nov20[1,]
warrington_dr2_nov20 <- warrington_dr2_nov20[-1,]

warrington_dr2_apr22 <- warrington_dr2_apr22[-c(1,2),] # removes relevant rows
colnames(warrington_dr2_apr22) <- warrington_dr2_apr22[1,]
warrington_dr2_apr22 <- warrington_dr2_apr22[-1,]

# check for duplicate rows

sum(duplicated(rochdale_dr2_nov20))

# Change column names

colnames(rochdale_dr2_nov20)
colnames(rochdale_dr2_nov20)[2]  <- "Referral ID (or Case ID)"
colnames(rochdale_dr2_nov20)[8]  <- "Disabled status"

# Removing unnecessary text from values

leicester_dr2_nov20$Gender <- str_replace(leicester_dr2_nov20$Gender, "[a)   ]", "")
leicester_dr2_nov20$Gender <- str_replace(leicester_dr2_nov20$Gender, "[b)   ]", "")
leicester_dr2_nov20$Gender <- str_replace(leicester_dr2_nov20$Gender, "c", "")
norfolk_dr2_apr22$Gender <- str_replace(norfolk_dr2_apr22$Gender, "[a)   ]", "")
norfolk_dr2_apr22$Gender <- str_replace(norfolk_dr2_apr22$Gender, "[b)   ]", "")
norfolk_dr2_apr22$Gender <- str_replace(norfolk_dr2_apr22$Gender, "d", "")
redcar_dr2_apr22$Gender <- str_replace(redcar_dr2_apr22$Gender, "[a)   ]", "")
redcar_dr2_apr22$Gender <- str_replace(redcar_dr2_apr22$Gender, "[b)   ]", "")
redcar_dr2_apr22$Gender <- str_replace(redcar_dr2_apr22$Gender, "d", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[a)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[i)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[j)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[g)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[l)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[o)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[d)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[f)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[h)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[r)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[k)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[m)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[p)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[c)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[n)  ]", "")
leicester_dr2_nov20$Ethnicity <- str_replace(leicester_dr2_nov20$Ethnicity, "[s)  ]", "")

norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[a)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[d)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[f)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[r)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[i)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[m)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[o)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[h)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[e)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[g)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[c)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[p)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[j)  ]", "")
norfolk_dr2_apr22$Ethnicity <- str_replace(norfolk_dr2_apr22$Ethnicity, "[s)  ]", "")

redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[a)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[d)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[f)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[r)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[i)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[m)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[o)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[h)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[e)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[g)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[c)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[p)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[j)  ]", "")
redcar_dr2_apr22$Ethnicity <- str_replace(redcar_dr2_apr22$Ethnicity, "[s)  ]", "")

norfolk_dr2_apr22$`Unaccompanied Asylum Seeker` <- str_replace(norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`, "[a) ]", "")
norfolk_dr2_apr22$`Unaccompanied Asylum Seeker` <- str_replace(norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`, "[b) ]", "")

redcar_dr2_apr22$`Unaccompanied Asylum Seeker` <- str_replace(redcar_dr2_apr22$`Unaccompanied Asylum Seeker`, "[a) ]", "")
redcar_dr2_apr22$`Unaccompanied Asylum Seeker` <- str_replace(redcar_dr2_apr22$`Unaccompanied Asylum Seeker`, "[b) ]", "")
redcar_dr2_apr22$`Unaccompanied Asylum Seeker` <- str_replace(redcar_dr2_apr22$`Unaccompanied Asylum Seeker`, "[ ]", "")

# Change extra text for Gender column to other/undetermined

norfolk_dr2_apr22$Gender <- str_replace(norfolk_dr2_apr22$Gender, "Indeterminate", "Other/undetermined")
norfolk_dr2_nov20$Gender <- str_replace(norfolk_dr2_nov20$Gender, "Indeterminate", "Other/undetermined")
norfolk_dr2_apr22$Gender <- str_replace(norfolk_dr2_apr22$Gender, "Neither", "Other/undetermined")
redcar_dr2_apr22$Gender <- str_replace(redcar_dr2_apr22$Gender, "Neither", "Other/undetermined")

# Missing data

norfolk_dr2_apr22 <- norfolk_dr2_apr22 %>% 
  mutate_all(na_if, "NULL")
stopifnot(sum(norfolk_dr2_apr22=="", na.rm=TRUE)==0) 

norfolk_dr2_nov20 <- norfolk_dr2_nov20 %>% 
  mutate_all(na_if, "N/A")
stopifnot(sum(norfolk_dr2_nov20=="", na.rm=TRUE)==0) 

norfolk_dr2_nov20 <- norfolk_dr2_nov20 %>% 
  mutate_all(na_if, "No Trace")
stopifnot(sum(norfolk_dr2_nov20=="", na.rm=TRUE)==0) 

redcar_dr2_apr22 <- redcar_dr2_apr22 %>% 
  mutate_all(na_if, "NULL")
stopifnot(sum(redcar_dr2_apr22=="", na.rm=TRUE)==0) 

# Some columns recoded from character to numeric

class(leicester_dr2_nov20$`Number of previous child protection plans`)
leicester_dr2_nov20$`Number of previous child protection plans` <- as.numeric(leicester_dr2_nov20$`Number of previous child protection plans`)

# Change ethnicity column from abbrev to full names

#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="WBRI"] <- "White British"
leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="Any other mixed background"] <- "MOTH"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="AIND"] <- "Indian"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="MWBA"] <- "White and Black African"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="ABAN"] <- "Bangladeshi"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="WOTH"] <- "Any other White background"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="MWAS"] <- "White and Asian"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="OOTH"] <- "Any other ethnic group"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="BOTH"] <- "Any other Black background"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="NOBT"] <- "Information not yet obtained"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="APKN"] <- "Pakistani"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="BAFR"] <- "Black - African"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="AOTH"] <- "Any other Asian background"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#leicester_dr2_nov20$Ethnicity[leicester_dr2_nov20$Ethnicity=="BCRB"] <- "Black Caribbean"

leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="White British"] <- "WBRI"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Any other mixed background"] <- "MOTH"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Indian"] <- "AIND"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="White and Black African"] <- "MWBA"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Bangladeshi"] <- "ABAN"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="White and Black Caribbean"] <- "MWBC"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Any other White background"] <- "WOTH"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="White and Asian"] <- "MWAS"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Any other ethnic group"] <- "OOTH"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Any other Black background"] <- "BOTH"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Information not yet obtained"] <- "NOBT"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Pakistani"] <- "APKN"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="African"] <- "BAFR"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Any other Asian background"] <- "AOTH"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Traveller of Irish heritage"] <- "WIRT"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Caribbean"] <- "BCRB"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="White Irish"] <- "WIRI"
leicester_dr2_apr22$Ethnicity[leicester_dr2_apr22$Ethnicity=="Traveller of Irish Heritage"] <- "WIRT"


#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="WBRI"] <- "White British"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="MOTH"] <- "Any other mixed background"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="AIND"] <- "Indian"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="MWBA"] <- "White and Black African"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="ABAN"] <- "Bangladeshi"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="WOTH"] <- "Any other White background"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="MWAS"] <- "White and Asian"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="OOTH"] <- "Any other ethnic group"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="BOTH"] <- "Any other Black background"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="NOBT"] <- "Information not yet obtained"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="APKN"] <- "Pakistani"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="BAFR"] <- "Black - African"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="AOTH"] <- "Any other Asian background"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#norfolk_dr2_nov20$Ethnicity[norfolk_dr2_nov20$Ethnicity=="BCRB"] <- "Black Caribbean"

#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="WBRI"] <- "White British"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="MOTH"] <- "Any other mixed background"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="AIND"] <- "Indian"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="MWBA"] <- "White and Black African"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="ABAN"] <- "Bangladeshi"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="WOTH"] <- "Any other White background"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="MWAS"] <- "White and Asian"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="OOTH"] <- "Any other ethnic group"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="BOTH"] <- "Any other Black background"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="NOBT"] <- "Information not yet obtained"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="APKN"] <- "Pakistani"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="BAFR"] <- "Black - African"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="AOTH"] <- "Any other Asian background"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="BCRB"] <- "Black Caribbean"
#norfolk_dr2_apr22$Ethnicity[norfolk_dr2_apr22$Ethnicity=="WROM"] <- "Gypsy/Roma"

#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="WBRI"] <- "White British"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="MOTH"] <- "Any other mixed background"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="AIND"] <- "Indian"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="MWBA"] <- "White and Black African"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="ABAN"] <- "Bangladeshi"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="WOTH"] <- "Any other White background"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="MWAS"] <- "White and Asian"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="OOTH"] <- "Any other ethnic group"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="BOTH"] <- "Any other Black background"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="NOBT"] <- "Information not yet obtained"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="APKN"] <- "Pakistani"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="BAFR"] <- "Black - African"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="AOTH"] <- "Any other Asian background"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="BCRB"] <- "Black Caribbean"
#redcar_dr2_nov20$Ethnicity[redcar_dr2_nov20$Ethnicity=="WROM"] <- "Gypsy/Roma"

#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="WBRI"] <- "White British"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="MOTH"] <- "Any other mixed background"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="AIND"] <- "Indian"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="MWBA"] <- "White and Black African"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="ABAN"] <- "Bangladeshi"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="WOTH"] <- "Any other White background"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="MWAS"] <- "White and Asian"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="OOTH"] <- "Any other ethnic group"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="BOTH"] <- "Any other Black background"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="NOBT"] <- "Information not yet obtained"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="APKN"] <- "Pakistani"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="BAFR"] <- "Black - African"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="AOTH"] <- "Any other Asian background"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="BCRB"] <- "Black Caribbean"
#redcar_dr2_apr22$Ethnicity[redcar_dr2_apr22$Ethnicity=="WROM"] <- "Gypsy/Roma"

#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="WBRI"] <- "White British"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="MOTH"] <- "Any other mixed background"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="AIND"] <- "Indian"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="MWBA"] <- "White and Black African"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="ABAN"] <- "Bangladeshi"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="WOTH"] <- "Any other White background"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="MWAS"] <- "White and Asian"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="OOTH"] <- "Any other ethnic group"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="BOTH"] <- "Any other Black background"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="NOBT"] <- "Information not yet obtained"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="APKN"] <- "Pakistani"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="BAFR"] <- "Black - African"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="AOTH"] <- "Any other Asian background"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="BCRB"] <- "Black Caribbean"
#rochdale_dr2_nov20$Ethnicity[rochdale_dr2_nov20$Ethnicity=="WIRI"] <- "White Irish"

#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="WBRI"] <- "White British"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="MOTH"] <- "Any other mixed background"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="AIND"] <- "Indian"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="MWBA"] <- "White and Black African"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="ABAN"] <- "Bangladeshi"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="WOTH"] <- "Any other White background"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="MWAS"] <- "White and Asian"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="OOTH"] <- "Any other ethnic group"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="BOTH"] <- "Any other Black background"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="NOBT"] <- "Information not yet obtained"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="APKN"] <- "Pakistani"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="BAFR"] <- "Black - African"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="AOTH"] <- "Any other Asian background"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="BCRB"] <- "Black Caribbean"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="WROM"] <- "Gypsy/Roma"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="WIRI"] <- "White Irish"
#rochdale_dr2_apr22$Ethnicity[rochdale_dr2_apr22$Ethnicity=="REFU"] <- "Refused"

#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="WBRI"] <- "White British"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="MOTH"] <- "Any other mixed background"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="AIND"] <- "Indian"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="MWBA"] <- "White and Black African"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="ABAN"] <- "Bangladeshi"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="WOTH"] <- "Any other White background"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="MWAS"] <- "White and Asian"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="OOTH"] <- "Any other ethnic group"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="BOTH"] <- "Any other Black background"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="NOBT"] <- "Information not yet obtained"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="APKN"] <- "Pakistani"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="BAFR"] <- "Black - African"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="AOTH"] <- "Any other Asian background"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#warrington_dr2_nov20$Ethnicity[warrington_dr2_nov20$Ethnicity=="BCRB"] <- "Black Caribbean"

#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="WBRI"] <- "White British"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="MOTH"] <- "Any other mixed background"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="AIND"] <- "Indian"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="MWBA"] <- "White and Black African"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="ABAN"] <- "Bangladeshi"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="MWBC"] <- "White and Black Caribbean"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="WOTH"] <- "Any other White background"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="MWAS"] <- "White and Asian"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="OOTH"] <- "Any other ethnic group"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="BOTH"] <- "Any other Black background"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="NOBT"] <- "Information not yet obtained"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="APKN"] <- "Pakistani"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="BAFR"] <- "Black - African"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="AOTH"] <- "Any other Asian background"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="WIRT"] <- "Traveller of Irish heritage"
#warrington_dr2_apr22$Ethnicity[warrington_dr2_apr22$Ethnicity=="BCRB"] <- "Black Caribbean"

# Change column names for Rochdale to full name

colnames(rochdale_dr2_apr22)
colnames(rochdale_dr2_nov20)[9]  <- "Unaccompanied Asylum Seeker"
colnames(rochdale_dr2_nov20)[4]  <- "Number of previous child protection plans"
colnames(rochdale_dr2_nov20)[6]  <- "Year and month of birth of the child"
colnames(rochdale_dr2_nov20)[3]  <- "Date period of care commenced"

# Change Y/N to Yes - 1 and 0 = No in disability column

leicester_dr2_nov20$`Disabled status`[leicester_dr2_nov20$`Disabled status`=="Y"] <- "1"
leicester_dr2_nov20$`Disabled status`[leicester_dr2_nov20$`Disabled status`=="N"] <- "0"
leicester_dr2_apr22$`Disabled status`[leicester_dr2_apr22$`Disabled status`=="N"] <- "0"
leicester_dr2_apr22$`Disabled status`[leicester_dr2_apr22$`Disabled status`=="Y"] <- "1"

norfolk_dr2_nov20$`Disabled status`[norfolk_dr2_nov20$`Disabled status`=="Y"] <- "1"
norfolk_dr2_nov20$`Disabled status`[norfolk_dr2_nov20$`Disabled status`=="N"] <- "0"
norfolk_dr2_apr22$`Disabled status`[norfolk_dr2_apr22$`Disabled status`=="N"] <- "0"
norfolk_dr2_apr22$`Disabled status`[norfolk_dr2_apr22$`Disabled status`=="Y"] <- "1"

redcar_dr2_nov20$`Disabled status`[redcar_dr2_nov20$`Disabled status`=="Yes"] <- "1"
redcar_dr2_nov20$`Disabled status`[redcar_dr2_nov20$`Disabled status`=="No"] <- "0"
redcar_dr2_apr22$`Disabled status`[redcar_dr2_apr22$`Disabled status`=="N"] <- "0"
redcar_dr2_apr22$`Disabled status`[redcar_dr2_apr22$`Disabled status`=="Y"] <- "1"

rochdale_dr2_nov20$`Disabled status`[rochdale_dr2_nov20$`Disabled status`=="Y"] <- "1"
rochdale_dr2_nov20$`Disabled status`[rochdale_dr2_nov20$`Disabled status`=="N"] <- "0"
rochdale_dr2_apr22$`Disabled status`[rochdale_dr2_apr22$`Disabled status`=="N"] <- "0"
rochdale_dr2_apr22$`Disabled status`[rochdale_dr2_apr22$`Disabled status`=="Y"] <- "1"

warrington_dr2_nov20$`Disabled status`[warrington_dr2_nov20$`Disabled status`=="Yes"] <- "1"
warrington_dr2_nov20$`Disabled status`[warrington_dr2_nov20$`Disabled status`=="No"] <- "0"
warrington_dr2_apr22$`Disabled status`[warrington_dr2_apr22$`Disabled status`=="N"] <- "0"
warrington_dr2_apr22$`Disabled status`[warrington_dr2_apr22$`Disabled status`=="Y"] <- "1"

# Change Y/N to Yes - 1 and 0 = No in unaccompanied asylum seeker column

leicester_dr2_nov20$`Unaccompanied Asylum Seeker`[leicester_dr2_nov20$`Unaccompanied Asylum Seeker`=="Y"] <- "1"
leicester_dr2_nov20$`Unaccompanied Asylum Seeker`[leicester_dr2_nov20$`Unaccompanied Asylum Seeker`=="N"] <- "0"
leicester_dr2_apr22$`Unaccompanied Asylum Seeker`[leicester_dr2_apr22$`Unaccompanied Asylum Seeker`=="N"] <- "0"
leicester_dr2_apr22$`Unaccompanied Asylum Seeker`[leicester_dr2_apr22$`Unaccompanied Asylum Seeker`=="Y"] <- "1"

norfolk_dr2_nov20$`Unaccompanied Asylum Seeker`[norfolk_dr2_nov20$`Unaccompanied Asylum Seeker`=="Y"] <- "1"
norfolk_dr2_nov20$`Unaccompanied Asylum Seeker`[norfolk_dr2_nov20$`Unaccompanied Asylum Seeker`=="N"] <- "0"
norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`[norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`==" No"] <- "0"
norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`[norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`==" Yes"] <- "1"
norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`[norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`=="Y"] <- "1"

redcar_dr2_nov20$`Unaccompanied Asylum Seeker`[redcar_dr2_nov20$`Unaccompanied Asylum Seeker`=="Y"] <- "1"
redcar_dr2_nov20$`Unaccompanied Asylum Seeker`[redcar_dr2_nov20$`Unaccompanied Asylum Seeker`=="N"] <- "0"
redcar_dr2_apr22$`Unaccompanied Asylum Seeker`[redcar_dr2_apr22$`Unaccompanied Asylum Seeker`=="No"] <- "0"
redcar_dr2_apr22$`Unaccompanied Asylum Seeker`[redcar_dr2_apr22$`Unaccompanied Asylum Seeker`=="Yes"] <- "1"
redcar_dr2_apr22$`Unaccompanied Asylum Seeker`[redcar_dr2_apr22$`Unaccompanied Asylum Seeker`=="Y"] <- "1"

rochdale_dr2_nov20$`Unaccompanied Asylum Seeker`[rochdale_dr2_nov20$`Unaccompanied Asylum Seeker`=="N"] <- "0"
rochdale_dr2_nov20$`Unaccompanied Asylum Seeker`[rochdale_dr2_nov20$`Unaccompanied Asylum Seeker`=="Y"] <- "1"
rochdale_dr2_apr22$`Unaccompanied Asylum Seeker`[rochdale_dr2_apr22$`Unaccompanied Asylum Seeker`=="N"] <- "0"
rochdale_dr2_apr22$`Unaccompanied Asylum Seeker`[rochdale_dr2_apr22$`Unaccompanied Asylum Seeker`=="Y"] <- "1"

warrington_dr2_nov20$`Unaccompanied Asylum Seeker`[warrington_dr2_nov20$`Unaccompanied Asylum Seeker`=="Y"] <- "1"
warrington_dr2_nov20$`Unaccompanied Asylum Seeker`[warrington_dr2_nov20$`Unaccompanied Asylum Seeker`=="N"] <- "0"
warrington_dr2_apr22$`Unaccompanied Asylum Seeker`[warrington_dr2_apr22$`Unaccompanied Asylum Seeker`=="Y"] <- "1"
warrington_dr2_apr22$`Unaccompanied Asylum Seeker`[warrington_dr2_apr22$`Unaccompanied Asylum Seeker`=="N"] <- "0"

# Change Y/N to Yes - 1 and 0 = No in free school meal eligibility

leicester_dr2_nov20$`Free school meal eligibility - ever FSM`[leicester_dr2_nov20$`Free school meal eligibility - ever FSM`=="Y"] <- "1"
leicester_dr2_nov20$`Free school meal eligibility - ever FSM`[leicester_dr2_nov20$`Free school meal eligibility - ever FSM`=="N"] <- "0"
norfolk_dr2_nov20$`Free school meal eligibility - ever FSM`[norfolk_dr2_nov20$`Free school meal eligibility - ever FSM`=="Y"] <- "1"
norfolk_dr2_nov20$`Free school meal eligibility - ever FSM`[norfolk_dr2_nov20$`Free school meal eligibility - ever FSM`=="N"] <- "0"
norfolk_dr2_apr22$`Free school meal eligibility - ever FSM`[norfolk_dr2_apr22$`Free school meal eligibility - ever FSM`=="Yes"] <- "1"
norfolk_dr2_apr22$`Free school meal eligibility - ever FSM`[norfolk_dr2_apr22$`Free school meal eligibility - ever FSM`=="No"] <- "0"
redcar_dr2_nov20$`Free school meal eligibility - ever FSM`[redcar_dr2_nov20$`Free school meal eligibility - ever FSM`=="Y"] <- "1"
redcar_dr2_nov20$`Free school meal eligibility - ever FSM`[redcar_dr2_nov20$`Free school meal eligibility - ever FSM`=="N"] <- "0"
redcar_dr2_apr22$`Free school meal eligibility - ever FSM`[redcar_dr2_apr22$`Free school meal eligibility - ever FSM`=="Yes"] <- "1"
redcar_dr2_apr22$`Free school meal eligibility - ever FSM`[redcar_dr2_apr22$`Free school meal eligibility - ever FSM`=="No"] <- "0"
rochdale_dr2_apr22$`Free school meal eligibility - ever FSM`[rochdale_dr2_apr22$`Free school meal eligibility - ever FSM`=="True"] <- "1"
warrington_dr2_apr22$`Free school meal eligibility - ever FSM`[warrington_dr2_apr22$`Free school meal eligibility - ever FSM`=="Y"] <- "1"
warrington_dr2_apr22$`Free school meal eligibility - ever FSM`[warrington_dr2_apr22$`Free school meal eligibility - ever FSM`=="N"] <- "0"

# Change Y/N to Yes - 1 and 0 = No in pupil premium eligibility

leicester_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[leicester_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="Y"] <- "1"
leicester_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[leicester_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="N"] <- "0"
norfolk_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[norfolk_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="Y"] <- "1"
norfolk_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[norfolk_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="N"] <- "0"
norfolk_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[norfolk_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="Yes"] <- "1"
norfolk_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[norfolk_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="No"] <- "0"
redcar_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[redcar_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="Y"] <- "1"
redcar_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[redcar_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="N"] <- "0"
redcar_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[redcar_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="Yes"] <- "1"
redcar_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[redcar_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="No"] <- "0"
rochdale_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[rochdale_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="TRUE"] <- "1"
warrington_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[warrington_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="Y"] <- "1"
warrington_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`[warrington_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`=="N"] <- "0"


# Some columns recoded from character to numeric and factor and change excel error to date

#Leicester

leicester_dr2_nov20$`Referral ID (or Case ID)` <- as.numeric(leicester_dr2_nov20$`Referral ID (or Case ID)`)
leicester_dr2_apr22$`Referral ID (or Case ID)` <- as.numeric(leicester_dr2_apr22$`Referral ID (or Case ID)`)
leicester_dr2_nov20$`Date period of care commenced` <- as.numeric(leicester_dr2_nov20$`Date period of care commenced`)
leicester_dr2_nov20$`Date period of care commenced` <- as.Date(leicester_dr2_nov20$`Date period of care commenced`, origin = "1899-12-30")
leicester_dr2_apr22$`Date period of care commenced` <- as.numeric(leicester_dr2_apr22$`Date period of care commenced`)
leicester_dr2_apr22$`Date period of care commenced` <- as.Date(leicester_dr2_apr22$`Date period of care commenced`, origin = "1899-12-30")
leicester_dr2_nov20$`Number of previous child protection plans` <- as.numeric(leicester_dr2_nov20$`Number of previous child protection plans`)
leicester_dr2_apr22$`Number of previous child protection plans` <- as.numeric(leicester_dr2_apr22$`Number of previous child protection plans`)
leicester_dr2_nov20$Gender <- as.factor(leicester_dr2_nov20$Gender)
leicester_dr2_apr22$Gender <- as.factor(leicester_dr2_apr22$Gender)
str(leicester_dr2_nov20$`Year and month of birth of the child`)
leicester_dr2_nov20$`Year and month of birth of the child` <- 
  as.Date(paste("01", leicester_dr2_nov20$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")
str(leicester_dr2_apr22$`Year and month of birth of the child`)
leicester_dr2_apr22$`Year and month of birth of the child` <- 
  as.Date(paste("01", leicester_dr2_apr22$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")
leicester_dr2_nov20$`Disabled status` <- as.numeric(leicester_dr2_nov20$`Disabled status`)
leicester_dr2_apr22$`Disabled status` <- as.numeric(leicester_dr2_apr22$`Disabled status`)
leicester_dr2_nov20$`Unaccompanied Asylum Seeker` <- as.numeric(leicester_dr2_nov20$`Unaccompanied Asylum Seeker`)
leicester_dr2_apr22$`Unaccompanied Asylum Seeker` <- as.numeric(leicester_dr2_apr22$`Unaccompanied Asylum Seeker`)
leicester_dr2_nov20$`Free school meal eligibility - ever FSM` <- as.numeric(leicester_dr2_nov20$`Free school meal eligibility - ever FSM`)
leicester_dr2_apr22$`Free school meal eligibility - ever FSM` <- as.numeric(leicester_dr2_apr22$`Free school meal eligibility - ever FSM`)
leicester_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)` <- as.numeric(leicester_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`)
leicester_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)` <- as.numeric(leicester_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`)


#Norfolk

norfolk_dr2_nov20$`Referral ID (or Case ID)` <- as.numeric(norfolk_dr2_nov20$`Referral ID (or Case ID)`)
norfolk_dr2_apr22$`Referral ID (or Case ID)` <- as.numeric(norfolk_dr2_apr22$`Referral ID (or Case ID)`)
norfolk_dr2_nov20$`Date period of care commenced` <- as.numeric(norfolk_dr2_nov20$`Date period of care commenced`)
norfolk_dr2_nov20$`Date period of care commenced` <- as.Date(norfolk_dr2_nov20$`Date period of care commenced`, origin = "1899-12-30")
norfolk_dr2_apr22$`Date period of care commenced` <- as.numeric(norfolk_dr2_apr22$`Date period of care commenced`)
norfolk_dr2_apr22$`Date period of care commenced` <- as.Date(norfolk_dr2_apr22$`Date period of care commenced`, origin = "1899-12-30")
norfolk_dr2_nov20$`Number of previous child protection plans` <- as.numeric(norfolk_dr2_nov20$`Number of previous child protection plans`)
norfolk_dr2_apr22$`Number of previous child protection plans` <- as.numeric(norfolk_dr2_apr22$`Number of previous child protection plans`)
norfolk_dr2_nov20$Gender <- as.factor(norfolk_dr2_nov20$Gender)
norfolk_dr2_apr22$Gender <- as.factor(norfolk_dr2_apr22$Gender)
norfolk_dr2_nov20$`Year and month of birth of the child` <- as.numeric(norfolk_dr2_nov20$`Year and month of birth of the child`)
norfolk_dr2_nov20$`Year and month of birth of the child` <- as.Date(norfolk_dr2_nov20$`Year and month of birth of the child`, origin = "1899-12-30")
norfolk_dr2_apr22$`Year and month of birth of the child` <- as.numeric(norfolk_dr2_apr22$`Year and month of birth of the child`)
norfolk_dr2_apr22$`Year and month of birth of the child` <- as.Date(norfolk_dr2_apr22$`Year and month of birth of the child`, origin = "1899-12-30")
norfolk_dr2_nov20$`Disabled status` <- as.numeric(norfolk_dr2_nov20$`Disabled status`)
norfolk_dr2_apr22$`Disabled status` <- as.numeric(norfolk_dr2_apr22$`Disabled status`)
norfolk_dr2_nov20$`Unaccompanied Asylum Seeker` <- as.numeric(norfolk_dr2_nov20$`Unaccompanied Asylum Seeker`)
norfolk_dr2_apr22$`Unaccompanied Asylum Seeker` <- as.numeric(norfolk_dr2_apr22$`Unaccompanied Asylum Seeker`)
norfolk_dr2_nov20$`Free school meal eligibility - ever FSM` <- as.numeric(norfolk_dr2_nov20$`Free school meal eligibility - ever FSM`)
norfolk_dr2_apr22$`Free school meal eligibility - ever FSM` <- as.numeric(norfolk_dr2_apr22$`Free school meal eligibility - ever FSM`)
norfolk_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)` <- as.numeric(norfolk_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`)
norfolk_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)` <- as.numeric(norfolk_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`)

#Redcar

redcar_dr2_nov20$`Referral ID (or Case ID)` <- as.numeric(redcar_dr2_nov20$`Referral ID (or Case ID)`)
redcar_dr2_apr22$`Referral ID (or Case ID)` <- as.numeric(redcar_dr2_apr22$`Referral ID (or Case ID)`)
redcar_dr2_nov20$`Date period of care commenced` <- as.numeric(redcar_dr2_nov20$`Date period of care commenced`)
redcar_dr2_nov20$`Date period of care commenced` <- as.Date(redcar_dr2_nov20$`Date period of care commenced`, origin = "1899-12-30")
redcar_dr2_apr22$`Date period of care commenced` <- as.numeric(redcar_dr2_apr22$`Date period of care commenced`)
redcar_dr2_apr22$`Date period of care commenced` <- as.Date(redcar_dr2_apr22$`Date period of care commenced`, origin = "1899-12-30")
redcar_dr2_nov20$`Number of previous child protection plans` <- as.numeric(redcar_dr2_nov20$`Number of previous child protection plans`)
redcar_dr2_apr22$`Number of previous child protection plans` <- as.numeric(redcar_dr2_apr22$`Number of previous child protection plans`)
redcar_dr2_nov20$Gender <- as.factor(redcar_dr2_nov20$Gender)
redcar_dr2_apr22$Gender <- as.factor(redcar_dr2_apr22$Gender)
redcar_dr2_nov20$`Year and month of birth of the child` <- as.numeric(redcar_dr2_nov20$`Year and month of birth of the child`)
redcar_dr2_nov20$`Year and month of birth of the child` <- as.Date(redcar_dr2_nov20$`Year and month of birth of the child`, origin = "1899-12-30")
redcar_dr2_apr22$`Year and month of birth of the child` <- as.numeric(redcar_dr2_apr22$`Year and month of birth of the child`)
redcar_dr2_apr22$`Year and month of birth of the child` <- as.Date(redcar_dr2_apr22$`Year and month of birth of the child`, origin = "1899-12-30")
redcar_dr2_nov20$`Disabled status` <- as.numeric(redcar_dr2_nov20$`Disabled status`)
redcar_dr2_apr22$`Disabled status` <- as.numeric(redcar_dr2_apr22$`Disabled status`)
redcar_dr2_nov20$`Unaccompanied Asylum Seeker` <- as.numeric(redcar_dr2_nov20$`Unaccompanied Asylum Seeker`)
redcar_dr2_apr22$`Unaccompanied Asylum Seeker` <- as.numeric(redcar_dr2_apr22$`Unaccompanied Asylum Seeker`)
redcar_dr2_nov20$`Free school meal eligibility - ever FSM` <- as.numeric(redcar_dr2_nov20$`Free school meal eligibility - ever FSM`)
redcar_dr2_apr22$`Free school meal eligibility - ever FSM` <- as.numeric(redcar_dr2_apr22$`Free school meal eligibility - ever FSM`)
redcar_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)` <- as.numeric(redcar_dr2_nov20$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`)
redcar_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)` <- as.numeric(redcar_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`)

#Rochdale

rochdale_dr2_nov20$`Referral ID (or Case ID)` <- as.numeric(rochdale_dr2_nov20$`Referral ID (or Case ID)`)
rochdale_dr2_nov20$`Number of previous child protection plans` <- as.numeric(rochdale_dr2_nov20$`Number of previous child protection plans`)
rochdale_dr2_nov20$Gender <- as.factor(rochdale_dr2_nov20$Gender)
str(rochdale_dr2_nov20$`Year and month of birth of the child`)
rochdale_dr2_nov20$`Year and month of birth of the child` <- 
  as.Date(paste("01", rochdale_dr2_nov20$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")
rochdale_dr2_nov20$`Disabled status` <- as.numeric(rochdale_dr2_nov20$`Disabled status`)
rochdale_dr2_nov20$`Unaccompanied Asylum Seeker` <- as.numeric(rochdale_dr2_nov20$`Unaccompanied Asylum Seeker`)

rochdale_dr2_apr22$`Referral ID (or Case ID)` <- as.numeric(rochdale_dr2_apr22$`Referral ID (or Case ID)`)
rochdale_dr2_apr22$`Date period of care commenced` <- as.numeric(rochdale_dr2_apr22$`Date period of care commenced`)
rochdale_dr2_apr22$`Date period of care commenced` <- as.Date(rochdale_dr2_apr22$`Date period of care commenced`, origin = "1899-12-30")
rochdale_dr2_apr22$`Number of previous child protection plans` <- as.numeric(rochdale_dr2_apr22$`Number of previous child protection plans`)
rochdale_dr2_apr22$Gender <- as.factor(rochdale_dr2_apr22$Gender)
str(rochdale_dr2_apr22$`Year and month of birth of the child`)
rochdale_dr2_apr22$`Year and month of birth of the child` <- 
  as.Date(paste("01", rochdale_dr2_apr22$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")
rochdale_dr2_apr22$`Disabled status` <- as.numeric(rochdale_dr2_apr22$`Disabled status`)
rochdale_dr2_apr22$`Unaccompanied Asylum Seeker` <- as.numeric(rochdale_dr2_apr22$`Unaccompanied Asylum Seeker`)
rochdale_dr2_apr22$`Free school meal eligibility - ever FSM` <- as.numeric(rochdale_dr2_apr22$`Free school meal eligibility - ever FSM`)
rochdale_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)` <- as.numeric(rochdale_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`)

#Warrington

warrington_dr2_nov20$`Referral ID (or Case ID)` <- as.numeric(warrington_dr2_nov20$`Referral ID (or Case ID)`)
warrington_dr2_apr22$`Referral ID (or Case ID)` <- as.numeric(warrington_dr2_apr22$`Referral ID (or Case ID)`)
warrington_dr2_nov20$`Date period of care commenced` <- as.numeric(warrington_dr2_nov20$`Date period of care commenced`)
warrington_dr2_nov20$`Date period of care commenced` <- as.Date(warrington_dr2_nov20$`Date period of care commenced`, origin = "1899-12-30")
warrington_dr2_apr22$`Date period of care commenced` <- as.numeric(warrington_dr2_apr22$`Date period of care commenced`)
warrington_dr2_apr22$`Date period of care commenced` <- as.Date(warrington_dr2_apr22$`Date period of care commenced`, origin = "1899-12-30")
warrington_dr2_nov20$`Number of previous child protection plans` <- as.numeric(warrington_dr2_nov20$`Number of previous child protection plans`)
warrington_dr2_apr22$`Number of previous child protection plans` <- as.numeric(warrington_dr2_apr22$`Number of previous child protection plans`)
warrington_dr2_nov20$Gender <- as.factor(warrington_dr2_nov20$Gender)
warrington_dr2_apr22$Gender <- as.factor(warrington_dr2_apr22$Gender)
str(warrington_dr2_nov20$`Year and month of birth of the child`)
warrington_dr2_nov20$`Year and month of birth of the child` <- 
  as.Date(paste("01", warrington_dr2_nov20$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")
str(warrington_dr2_apr22$`Year and month of birth of the child`)
warrington_dr2_apr22$`Year and month of birth of the child` <- 
  as.Date(paste("01", warrington_dr2_apr22$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")
warrington_dr2_nov20$`Disabled status` <- as.numeric(warrington_dr2_nov20$`Disabled status`)
warrington_dr2_apr22$`Disabled status` <- as.numeric(warrington_dr2_apr22$`Disabled status`)
warrington_dr2_nov20$`Unaccompanied Asylum Seeker` <- as.numeric(warrington_dr2_nov20$`Unaccompanied Asylum Seeker`)
warrington_dr2_apr22$`Unaccompanied Asylum Seeker` <- as.numeric(warrington_dr2_apr22$`Unaccompanied Asylum Seeker`)
warrington_dr2_apr22$`Free school meal eligibility - ever FSM` <- as.numeric(warrington_dr2_apr22$`Free school meal eligibility - ever FSM`) # Issue with data - Warrington Nov 20 does not include FSM data.
warrington_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)` <- as.numeric(warrington_dr2_apr22$`Pupil Premium eligibility (for Reception, Year 1 and Year 2)`) # Issue with data - Warrington Nov 20 does not include PPE data.

# create new variable listing age of children when NWD started in their LA

# Merging LA datasets

all_leic_dr2cla <- rbind(leicester_dr2_nov20, leicester_dr2_apr22)
all_norf_dr2cla <- rbind(norfolk_dr2_nov20, norfolk_dr2_apr22)
all_redcar_dr2cla <- rbind(redcar_dr2_nov20, redcar_dr2_apr22)
all_roch_dr2cla <- rbind.fill(rochdale_dr2_nov20, rochdale_dr2_apr22)
all_warr_dr2cla <- rbind.fill(warrington_dr2_nov20, warrington_dr2_apr22)

# Adding a variable/column to show this dataframe comes from a specific LA. This will need to be clear in the full merge across LAs.

LA = c("Leicester")
all_leic_dr2cla$LA <- "Leicester"
all_leic_dr2cla <- all_leic_dr2cla %>% relocate(LA)

LA = c("Norfolk")
all_norf_dr2cla$LA <- "Norfolk"
all_norf_dr2cla <- all_norf_dr2cla %>% relocate(LA)

LA = c("Redcar")
all_redcar_dr2cla$LA <- "Redcar"
all_redcar_dr2cla <- all_redcar_dr2cla %>% relocate(LA)

LA = c("Rochdale")
all_roch_dr2cla$LA <- "Rochdale"
all_roch_dr2cla <- all_roch_dr2cla %>% relocate(LA)

LA = c("Warrington")
all_warr_dr2cla$LA <- "Warrington"
all_warr_dr2cla <- all_warr_dr2cla %>% relocate(LA)

# merge all LA dr2 CLA data

all_dr2cla <- rbind(all_leic_dr2cla,all_norf_dr2cla,all_redcar_dr2cla,all_roch_dr2cla,all_warr_dr2cla)

# save binded dr2 dataset

save(all_dr2cla, file = "Output/alldr2cla.RData")

