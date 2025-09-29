
# Cleaning the family safeguarding dataset for SFPC
# DR2 - child level data across LAs

setwd('C:/Users/EmilyWalker/Foundations/High-SFPC-Impact - Working folder/sfpc_familysafeguarding_cleaning')

# Combine all the DR2 for each LA. 

# Open correct project before running code: 'sfpc_familysafeguarding_cleaning'

# Clearing R -------------------------
# rm(list = ls())

# Install and load tidyverse package ---------------------------

# install packages if not already installed
library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(tibble)
library(lubridate)
library(data.table)
library(arsenal)

# CIN Census ----
# https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/1025195/Children_in_need_census_2022_to_2023_guide.pdf

# Factors identified at end of assessment:
# Parental substance misuse: 1B, 2B
# Parental mental health: 4B
# Domestic violence: 3A, 3B

# Reading LAs in individually ########
###################################################
# Reading in LANCASHIRE DR2 files ====

lancs_dr2_nov20 <- read_excel("Data/FS_DR2/lancashire_dr2_nov20.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)

lancs_dr2_nov21 <- read_excel("Data/FS_DR2/lancashire_dr2_nov21.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)

lancs_dr2_nov22 <- read_excel("Data/FS_DR2/lancashire_dr2_nov22.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)

lancs_dr2_apr23 <- read_excel("Data/FS_DR2/lancashire_dr2_apr23.xlsx",
                               sheet = "DR2 - Data - sample pop",
                               skip = 3)

  # Check the class of the Month variable


# NO FURTHER ACTION (recode before binding) =====
# Cannot bind Lancs as Referral no further action is a character in 1 file and numeric in 2
# Coded as: 1 (or true) if the referral was received but after initial consideration no further action was taken; ·0 (or false) if the referral was received and after initial consideration further action was taken. This data item should not be left blank.
# Checking the class of the variable 
class(lancs_dr2_nov20$`Referral No further action`)
#character
class(lancs_dr2_nov21$`Referral No further action`)
#numeric
class(lancs_dr2_nov22$`Referral No further action`)
#character
class(lancs_dr2_apr23$`Referral No further action`)
#numeric

# Exploring the variable to see whether converting to numeric would lose any data. 
refnoaction_lanc_dr2_20 <- lancs_dr2_nov20 %>% 
  select(`Referral No further action`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
##MULTIVALUE      0         1         Sum 
#26              946       99        9592 

refnoaction_lanc_dr2_21 <- lancs_dr2_nov21 %>% 
  select(`Referral No further action`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
#   0    1    <NA>    Sum 
# 6836   42     3    6881 

refnoaction_lanc_dr2_22 <- lancs_dr2_nov22 %>% 
  select(`Referral No further action`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
##MULTIVALUE       0           1         Sum 
#      6          6914        76        6996 

# Testing turning the Referal no further action variable into numeric in 2020 and 2022 dataset.
# 2020: NOTE: this means losing the 26 'multivalue'observations to missing
lancs_dr2_nov20$`Referral No further action` <- 
  as.numeric(lancs_dr2_nov20$`Referral No further action`)
#NAs introduced by coercion 
numeric_refnoaction_lanc_dr2_20 <- lancs_dr2_nov20 %>% 
  select(`Referral No further action`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()

# 2022: NOTE: this means losing the 6 'multivalue' observations to missing 
lancs_dr2_nov22$`Referral No further action` <- 
  as.numeric(lancs_dr2_nov22$`Referral No further action`)
#NAs introduced by coercion 
numeric_refnoaction_lanc_dr2_22 <- lancs_dr2_nov22 %>% 
  select(`Referral No further action`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()


# BINDING LANCS DR2 Files ====
# Row binding the files to see how much duplication/ same child ID in different files there is. 
bind_lancs_dr2 <- bind_rows(lancs_dr2_nov20, lancs_dr2_nov21, lancs_dr2_nov22, lancs_dr2_apr23)

# CLEANING binded Lancs file -----

# CLEANING Lancs DATES ======
# Assessment actual start and referal date has time stamp as well as date. Remove by converting from POXIT to Date.
# Check the class of the assessment and referal dates in the separate dataframes. 
dataframe_names <- c("lancs_dr2_nov20", "lancs_dr2_nov21", "lancs_dr2_nov22", "lancs_dr2_apr23")

# REFERAL DATE
# Loop over the dataframe names
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  

  refdate_class <- class(df$`Referral Date`)
  
  # Print the result
  cat("Class of referal date", df_name, ":", refdate_class, "\n")
}

# Class of referal date lancs_dr2_nov20 : POSIXct POSIXt 
# Class of referal date lancs_dr2_nov21 : POSIXct POSIXt 
# Class of referal date lancs_dr2_nov22 : POSIXct POSIXt
# Class of referal date lancs_dr2_apr23 : POSIXct POSIXt 

# ASSESSMENT DATE
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  assessdate_class <- class(df$`Assessment actual start date`)
  
  # Print the result
  cat("Class of assessment date", df_name, ":", assessdate_class, "\n")
}

# Class of assessment date lancs_dr2_nov20 : POSIXct POSIXt 
# Class of assessment date lancs_dr2_nov21 : POSIXct POSIXt 
# Class of assessment date lancs_dr2_nov22 : POSIXct POSIXt
# Class of assessment date lancs_dr2_apr23 : POSIXct POSIXt 

# Converting POSIX to Date to drop the time stamp
bind_lancs_dr2$`Assessment actual start date`<- 
  as.Date(bind_lancs_dr2$`Assessment actual start date`)

bind_lancs_dr2$`Referral Date`<- 
  as.Date(bind_lancs_dr2$`Referral Date`)


# Recode disability status ----
bind_lancs_dr2 %>%
  select(`Disabled status`) %>%
  table(useNA = "ifany") %>%
  addmargins()

# N      Y      <NA>   Sum 
# 22737  1264   16    24017

# recode as binary variable 
bind_lancs_dr2$`Disabled status` <-
  ifelse(bind_lancs_dr2$`Disabled status` == "N", 0,
         ifelse(bind_lancs_dr2$`Disabled status` == "Y", 1,
                bind_lancs_dr2$`Disabled status`))

# 0        1    <NA>   Sum 
# 22737  1264    16   24017 

# Recode UASC ----
bind_lancs_dr2 %>%
  select(`Unaccompanied Asylum Seeker`) %>%
  table(useNA = "ifany") %>%
  addmargins()

#MULTIVALUE           N           Y         Sum 
#         4       23929          84       24017
# Checking the original file, these are also MULTIVALUE. Mark these down as missing. 

bind_lancs_dr2$`Unaccompanied Asylum Seeker` <-
  ifelse(bind_lancs_dr2$`Unaccompanied Asylum Seeker` == "#MULTIVALUE", NA,
         ifelse(bind_lancs_dr2$`Unaccompanied Asylum Seeker` == "N", 0,
                ifelse(bind_lancs_dr2$`Unaccompanied Asylum Seeker` == "Y", 1,
                       bind_lancs_dr2$`Unaccompanied Asylum Seeker`)))

#   0         1     <NA>   Sum 
#  23929      84     4     24017

# Recode outcome at assessment as binary ----
bind_lancs_dr2 %>%
  select(`Outcome of Single Assessment`) %>%
  table(useNA = "ifany") %>%
  addmargins()

#    No     Yes   <NA>   Sum 
#   11782   7746  4489   24017

bind_lancs_dr2$`Outcome of Single Assessment` <-
  ifelse(bind_lancs_dr2$`Outcome of Single Assessment` == "No", 0,
         ifelse(bind_lancs_dr2$`Outcome of Single Assessment` == "Yes", 1,
                bind_lancs_dr2$`Outcome of Single Assessment`))

# Recode month and year into date ----
str(bind_lancs_dr2$`Year and month of birth of the child`)
bind_lancs_dr2$`Year and month of birth of the child` <- 
  as.Date(paste("01", bind_lancs_dr2$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")

# Create variable for age at time of referral ----
bind_lancs_dr2$ageatref <- floor(as.numeric(difftime(bind_lancs_dr2$`Referral Date`, 
                                                bind_lancs_dr2$`Year and month of birth of the child`, 
                                                units = "days")/365.25))

# # Moving age so that is is next to year of birth variable 
new_order <- c(1:15, 19, 16:18)
bind_lancs_dr2 <- bind_lancs_dr2[, new_order]

# Recode ethnicity ----

table(bind_lancs_dr2$Ethnicity)

bind_lancs_dr2$Ethnicity <- as.factor(bind_lancs_dr2$Ethnicity)

#Create a binary (0/1) indicator variables for each category or level of the ethnicity variable.
# Use one-hot encoding
#ethnicity_df <- data.frame(model.matrix(~ Ethnicity - 1, data = bind_lancs_dr2))

# Rename the columns for clarity
#colnames(ethnicity_df) <- gsub("Ethnicity", "", colnames(ethnicity_df))

# Add the one-hot encoded variables to the original data frame
#bind_lancs_dr2 <- cbind(bind_lancs_dr2, ethnicity_df)

# Recoding factors identified at assessment so that it is a similar format to the other LAs ----
colnames(bind_lancs_dr2)

#"Factors identified at the end of assessment 1B"
bind_lancs_dr2 %>%
  select(`Factors identified at the end of assessment 1B`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#   No     Yes   <NA>   Sum 
#   16003  3503  4511   24017 
# Exploring NA, these are the cases which have not yet had an actual assessment date. In the model control for this. 

bind_lancs_dr2$`Factors identified at the end of assessment 1B` <-
  ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 1B` == "Yes", "1B",
         ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 1B` == "No", NA,
                bind_lancs_dr2$`Factors identified at the end of assessment 1B`))

#"Factors identified at the end of assessment 2B" 
bind_lancs_dr2 %>%
  select(`Factors identified at the end of assessment 2B`) %>%
  table(useNA = "ifany") %>%
  addmargins()
# No      Yes    <NA>    Sum 
# 16094   3413   4510   24017

bind_lancs_dr2$`Factors identified at the end of assessment 2B` <-
  ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 2B` == "Yes", "2B",
         ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 2B` == "No", NA,
                bind_lancs_dr2$`Factors identified at the end of assessment 2B`))

#"Factors identified at the end of assessment 3B"
bind_lancs_dr2 %>%
  select(`Factors identified at the end of assessment 3B`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#No     Yes   <NA>   Sum 
#14091  5422  4504   24017

bind_lancs_dr2$`Factors identified at the end of assessment 3B` <-
  ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 3B` == "Yes", "3B",
         ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 3B` == "No", NA,
                bind_lancs_dr2$`Factors identified at the end of assessment 3B`))

#"Factors identified at the end of assessment 3A" 
bind_lancs_dr2 %>%
  select(`Factors identified at the end of assessment 3A`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#    No    Yes  <NA>    Sum 
#   17683  1829  4505   24017

bind_lancs_dr2$`Factors identified at the end of assessment 3A` <-
  ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 3A` == "Yes", "3A",
         ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 3A` == "No", NA,
                bind_lancs_dr2$`Factors identified at the end of assessment 3A`))

#"Factors identified at the end of assessment 3C"
bind_lancs_dr2 %>%
  select(`Factors identified at the end of assessment 3C`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#   No   Yes  <NA>   Sum 
#18979   531  4507 24017

bind_lancs_dr2$`Factors identified at the end of assessment 3C` <-
  ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 3C` == "Yes", "3C",
         ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 3C` == "No", NA,
                bind_lancs_dr2$`Factors identified at the end of assessment 3C`))

#"Factors identified at the end of assessment 4B"
bind_lancs_dr2 %>%
  select(`Factors identified at the end of assessment 4B`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#   No   Yes  <NA>   Sum 
#12409  7097  4511  24017 

bind_lancs_dr2$`Factors identified at the end of assessment 4B` <-
  ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 4B` == "Yes", "4B",
         ifelse(bind_lancs_dr2$`Factors identified at the end of assessment 4B` == "No", NA,
                bind_lancs_dr2$`Factors identified at the end of assessment 4B`))

# Creating binary for whether referral reason included MH, DA, SU (trio of vulnerabilities) ----
trio_obs <- c("3B", "3A", "3C", "4B", "2B", "1B")

# Create a new binary column
# NOTE: More complicated code was needed to account for the fact most observations were NA.
bind_lancs_dr2 <- bind_lancs_dr2 %>%
  rowwise() %>%
  mutate(reftrio = ifelse(any(str_trim(c_across(starts_with("Factors identified"))) %in% trio_obs, na.rm = TRUE),
                          1, 0)) %>% ungroup()

# Creating a unique identifier to merge on ----
bind_lancs_dr2$concat_id <- paste(bind_lancs_dr2$`Child ID`, bind_lancs_dr2$`Referral ID (or Case ID)`)
# Add variable for LA ----
bind_lancs_dr2$LA <- "Lancashire"
# Adding LA to caseID ----
bind_lancs_dr2$childla_id <- paste(bind_lancs_dr2$`Child ID`, bind_lancs_dr2$`LA`)
new_order <- c(23,1:22)
bind_lancs_dr2 <- bind_lancs_dr2[, new_order]

# very high missingness in lancashire explore whether 0 was marked as NA. 
bind_lancs_dr2 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()

# Recode NA for Lancs as 0
bind_lancs_dr2 <- bind_lancs_dr2 %>%
  mutate(`Number of previous child protection plans` = ifelse(is.na(`Number of previous child protection plans`), 
                                                              0, `Number of previous child protection plans`))

###Duplicates ----
# Check for duplicate rows where all columns are the same
duplicates <- duplicated(bind_lancs_dr2) | duplicated(bind_lancs_dr2, fromLast = TRUE)

# duplicates: 124 

# Show duplicate rows
lancduplicate_rows <- bind_lancs_dr2[duplicates, ]
print(lancduplicate_rows)

##################################################
# Reading in SWINDON DR2 files =======

swind_dr2_nov20 <- read_excel("Data/FS_DR2/swindon_dr2_nov20.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)

swind_dr2_nov21 <- read_excel("Data/FS_DR2/swindon_dr2_nov21.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)
                              
swind_dr2_nov22 <- read_excel("Data/FS_DR2/swindon_dr2_nov22.xlsx")


swind_dr2_apr23 <- read_excel("Data/FS_DR2/swindon_dr2_apr23.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)

colnames(swind_dr2_nov21)
#Additional columns in Nov 21. Code to drop them 
swind_dr2_nov21 <- swind_dr2_nov21[,-c(14:18)]


# Exploring the Swindon dataframes ====
swindon_names <- c("swind_dr2_nov20", "swind_dr2_nov21", "swind_dr2_nov22", "swind_dr2_apr23")

# Loop over the dataframe names
for (df_name in swindon_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  swind_sum <- summary(df)
  
  # Print the result
  cat("summary of swindon data frames", df_name, ":", swind_sum , "\n")
}

#View(swind_sum)

comparedf(swind_dr2_nov20,swind_dr2_nov21) 

# RECODING DATES containing successful and unsuccessful code----

# Exploring the class of dates
dataframe_names <- c("swind_dr2_nov20", "swind_dr2_nov21", "swind_dr2_nov22", "swind_dr2_apr23")

# REFERAL DATE
# Loop over the dataframe names
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  refdate_class <- class(df$`Referral Date`)
  
  # Print the result
  cat("Class of referal date in swindon", df_name, ":", refdate_class, "\n")
}

# Result:
# Class of referal date in swindon swind_dr2_nov20 : character 
# Class of referal date in swindon swind_dr2_nov21 : POSIXct POSIXt 
# Class of referal date in swindon swind_dr2_nov22 : POSIXct POSIXt 
# Class of referal date in swindon swind_dr2_apr23 : POSIXct POSIXt 

# ASSESSMENT DATE
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  assessdate_class <- class(df$`Assessment actual start date`)
  
  # Print the result
  cat("Class of assessment date", df_name, ":", assessdate_class, "\n")
}

# Result:
# Class of assessment date swind_dr2_nov20 : character 
# Class of assessment date swind_dr2_nov21 : POSIXct POSIXt 
# Class of assessment date swind_dr2_nov22 : POSIXct POSIXt 
# Class of assessment date swind_dr2_apr23 : POSIXct POSIXt 

# Swindon Nov 2020 appears to be character instead of date 
str(swind_dr2_nov20$`Referral Date`) # structure of the data
summary(swind_dr2_nov20$`Referral Date`) # descriptive stats
str(swind_dr2_nov20$`Assessment actual start date`) # structure of the data

# Dates read in as character/Excel serial numbers. See below: 
#chr [1:2559] "43810" "14/05/2020" "29/01/2020" "13/03/2020" "43596" ...

# swind_dr2_nov20$`new_Referral Date` <- as.Date(swind_dr2_nov20$`Referral Date`)
#Error in charToDate(x) : 
#  character string is not in a standard unambiguous format

# UNSUCCESSFUL code to recode the date variables (keep collapsed) ----
# Code to change the Excel serial numbers (character) into date
# Check how many missing there are before changing anything
is.na(swind_dr2_nov20$`Referral Date`) %>% sum() # Zero missing

# Excel stores dates as the number of days since January 1, 1900 (for Windows). You need to specify this origin when converting Excel serial numbers to date objects in R.
swind_dr2_nov20$`Referral Date` <- as.numeric(swind_dr2_nov20$`Referral Date`)  # Convert to numeric first

#Looking at whether any went missing 
str(swind_dr2_nov20$`Referral Date`)
is.na(swind_dr2_nov20$`Referral Date`) %>% sum() # there are now 1522 NAs

#For Windows Excel (1900 date system)
swind_dr2_nov20$`Referral Date` <- as.Date(swind_dr2_nov20$`Referral Date`, origin = "1899-12-30")

# See if missings have been replaced
str(swind_dr2_nov20$`Referral Date`)
is.na(swind_dr2_nov20$`Referral Date`) %>% sum() # there are still 1522 NAs

# Above code to change date does not work. Manually changed Excel file to date category as a copy. 
# Renaming old file to keep for comparison 
old_swind_dr2_nov20 <- swind_dr2_nov20
rm(swind_dr2_nov20)

# Reading in modified file  
new_swind_dr2_nov20 <- read_excel("Data/FS_DR2/dates_swindon_dr2_nov20.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)

# Recode to dates
# new_swind_dr2_nov20$`Referral Date` <- as.Date(new_swind_dr2_nov20$`Referral Date`, origin = "1899-12-30")
# Error in charToDate(x) : 
# character string is not in a standard unambiguous format

# Recoding the variable into an interger, and then into a date works on the dodgy Excel data, but then it deletes the observations that are already dates. 
# Create new variable for the integer dates
new_swind_dr2_nov20$integer_refdate <- new_swind_dr2_nov20$`Referral Date`
# Changing the order so that the date columns are next to each other
new_order <- c(1:3, 14, 4:13)
new_swind_dr2_nov20 <- new_swind_dr2_nov20[, new_order]

# Recode this new variable as an integer, introducing missing
new_swind_dr2_nov20$`integer_refdate` <- as.integer(new_swind_dr2_nov20$`Referral Date`)
# Checking missing
is.na(new_swind_dr2_nov20$integer_refdate) %>% sum() #1522

# Recode the integers as dates
new_swind_dr2_nov20$`integer_refdate` <- as.Date(new_swind_dr2_nov20$integer_refdate, origin = "1899-12-30")

# Now recode the column where dates are in the correct format 
# First delete all the integer entries which we have preservered in another column. 
new_swind_dr2_nov20$`Referral Date`[grepl("^4", new_swind_dr2_nov20$`Referral Date`)] <- NA

new_swind_dr2_nov20$`Referral Date` <- as.Date(new_swind_dr2_nov20$`Referral Date`, format = "%d/%m/Y")


# Checking to see new file missingness and class
class(new_swind_dr2_nov20$`Referral Date`)
str(new_swind_dr2_nov20$`Referral Date`)
is.na(new_swind_dr2_nov20$`Referral Date`) %>% sum()


# SUCCESSFULLY reading in another file, attempting to figure out the issues at the source----
swind_dr2_nov20 <- read_excel("Data/FS_DR2/copy_swindon_dr2_nov20.xlsx")

# Checking the class shows the data has been successfully read in as a date. 
class(swind_dr2_nov20$`Referral Date`)

# Transform to date rather than POSIT 
swind_dr2_nov20$`Referral Date` <- as.Date (swind_dr2_nov20$`Referral Date`)
swind_dr2_nov20$`Assessment actual start date` <- as.Date(swind_dr2_nov20$`Assessment actual start date`)

# Checking this has worked
class(swind_dr2_nov20$`Referral Date`)
class(swind_dr2_nov20$`Assessment actual start date`)

# bind_swind_dr2 <- bind_rows(swind_dr2_nov20, swind_dr2_nov21, swind_dr2_nov22, swind_dr2_apr23)
# Error: can't combine $Referral ID (or Case ID)` <double> and `..2$Referral ID (or Case ID)` <character>.

# RECODE 'Referal ID' variable ----
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  refid_class <- class(df$`Referral ID (or Case ID)`)
  
  # Print the result
  cat("Class of referal ID in swindon", df_name, ":", refid_class, "\n")
}

# Class of referal ID in swindon swind_dr2_nov20 : numeric 
# Class of referal ID in swindon swind_dr2_nov21 : character 
# Class of referal ID in swindon swind_dr2_nov22 : character 
# Class of referal ID in swindon swind_dr2_apr23 : character 

# Explore the ref ID that has issues
str(swind_dr2_nov20$`Referral ID (or Case ID)`)
is.na(swind_dr2_nov20$`Referral ID (or Case ID)`) %>% sum () # = 0 

# Converting it into a character 
swind_dr2_nov20$`Referral ID (or Case ID)` <- as.character(swind_dr2_nov20$`Referral ID (or Case ID)`)
class(swind_dr2_nov20$`Referral ID (or Case ID)`)
is.na(swind_dr2_nov20$`Referral ID (or Case ID)`) %>% sum () # = 0
str(swind_dr2_nov20$`Referral ID (or Case ID)`)

# Binding SWINDON
# bind_swind_dr2 <- bind_rows(swind_dr2_nov20, swind_dr2_nov21, swind_dr2_nov22, swind_dr2_apr23)
# Appears to be issue with referral no further action

# RECODE 'referral no further action' variable ----
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  refnoa_class <- class(df$`Referral No further action`)
  
  # Print the result
  cat("Class of referal no further action in swindon", df_name, ":", refnoa_class, "\n")
}

# Results 
# Class of referal no further action in swindon swind_dr2_nov20 : numeric 
# Class of referal no further action in swindon swind_dr2_nov21 : character 
# Class of referal no further action in swindon swind_dr2_nov22 : numeric 
# Class of referal no further action in swindon swind_dr2_apr23 : numeric 

# Exploring the Nov 21 character variable 
str(swind_dr2_nov21$`Referral No further action`)

swind_dr2_nov21 %>% 
  select(`Referral No further action`) %>% 
  table(useNA = "ifany") %>%
  addmargins()

# Result
# 0      1    Sum 
# 3793  293   4086 

# Appears to be ok to convert to numeric 
swind_dr2_nov21$`Referral No further action` <- as.numeric(swind_dr2_nov21$`Referral No further action`)

# Checking how the conversion went 
swind_dr2_nov21 %>% 
  select(`Referral No further action`) %>% 
  table(useNA = "ifany") %>%
  addmargins()

# bind_swind_dr2 <- bind_rows(swind_dr2_nov20, swind_dr2_nov21, swind_dr2_nov22, swind_dr2_apr23)
# Bind shows that the year and month of child's bith has different classes

# RECODE 'Year and month of child birth' variable ----
# Exploring year and month of birth across datasets
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  childbirth_class <- class(df$`Year and month of birth of the child`)
  
  # Print the result
  cat("Class of referal no further action in swindon", df_name, ":", childbirth_class, "\n")
}

# Class of referal no further action in swindon swind_dr2_nov20 : POSIXct POSIXt 
# Class of referal no further action in swindon swind_dr2_nov21 : character 
# Class of referal no further action in swindon swind_dr2_nov22 : character 
# Class of referal no further action in swindon swind_dr2_apr23 : character 

# Result shows that November 20 has a different class. 
str(swind_dr2_nov20$`Year and month of birth of the child`)

# Compare it to another one 
str(swind_dr2_nov21$`Year and month of birth of the child`)

# Convert the other dates to date format 
# NOV 21
str(swind_dr2_nov21$`Year and month of birth of the child`)
swind_dr2_nov21$`Year and month of birth of the child` <- 
  as.Date(paste("01", swind_dr2_nov21$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")

#Checking it after recode
str(swind_dr2_nov21$`Year and month of birth of the child`)

# NOV 22
str(swind_dr2_nov22$`Year and month of birth of the child`)
swind_dr2_nov22$`Year and month of birth of the child` <- 
  as.Date(paste("01", swind_dr2_nov22$`Year and month of birth of the child`, sep = "/"), format = "%d/%m/%Y")

# Check it after the recode
str(swind_dr2_nov22$`Year and month of birth of the child`)

# APR 23
str(swind_dr2_apr23$`Year and month of birth of the child`)
swind_dr2_apr23$`Year and month of birth of the child` <- 
  as.Date(paste("01", swind_dr2_apr23$`Year and month of birth of the child`, sep = "/"), format = "%d/%m/%Y")

# Check it after recode
str(swind_dr2_apr23$`Year and month of birth of the child`)

# SUCCESSFUL Bind SWINDON ====
bind_swind_dr2 <- bind_rows(swind_dr2_nov20, swind_dr2_nov21, swind_dr2_nov22, swind_dr2_apr23)

# Cleaning bound Swindon ----
# Recode UASC ----

# Exploring the variable
bind_swind_dr2 %>%
  select(`Unaccompanied Asylum Seeker`) %>%
  table(useNA= "ifany") %>%
  addmargins()
  
# N        Y    <NA>    Sum 
# 10772    67     53    10892 

#Recode N and Y into binary variable. Leave missing 
bind_swind_dr2$`Unaccompanied Asylum Seeker` <- 
  ifelse(bind_swind_dr2$`Unaccompanied Asylum Seeker` == "N", 0,
         ifelse(bind_swind_dr2$`Unaccompanied Asylum Seeker`== "Y", 1,
                bind_swind_dr2$`Unaccompanied Asylum Seeker`))

# Checking to see if it worked
bind_swind_dr2 %>%
  select(`Unaccompanied Asylum Seeker`) %>%
  table(useNA= "ifany") %>%
  addmargins()

#     0       1    <NA>   Sum 
#   10772    67     53      10892 

# Recode Disability status -----
bind_swind_dr2 %>%
  select(`Disabled status`) %>%
  table(useNA = "ifany") %>%
  addmargins()

#    N      Y    <NA>   Sum 
#  10700   139    53    10892 

# Recode N and Y so that is is binary.
bind_swind_dr2$`Disabled status` <-
  ifelse(bind_swind_dr2$`Disabled status` == "N", 0,
         ifelse(bind_swind_dr2$`Disabled status` == "Y", 1,
                bind_swind_dr2$`Disabled status`))

# Check to see if it worked
bind_swind_dr2 %>%
  select(`Disabled status`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#     0     1    <NA>    Sum 
#  10700   139    53     10892 

# Investigated the missingness in disability status and UASC. These are unborn children. 

# Recode factors identified at assessment----
# Spliting the data up so that it is in separate columns.

# Split the original column by comma and convert it to a list
split_columns <- strsplit(bind_swind_dr2$`Factors identified at the end of assessment`, ",")

# Calculate the maximum number of elements in any split
max_elements <- max(sapply(split_columns, length))

# Create new column names for the split columns
new_factors <- paste("split_", 1:max_elements, sep = "")

for (i in 1:max_elements) {
  bind_swind_dr2[new_factors[i]] <- sapply(split_columns, function(x) ifelse(length(x) >= i, x[i], NA))
}

# Creating a dummy variable for whether factors identified at end of assessment include DA; SA; MH (trio)


# Recode outcome at single assessment as binary ----
bind_swind_dr2 %>%
  select(`Outcome of Single Assessment`) %>%
  table(useNA = "ifany") %>%
  addmargins()

#    No   Yes  <NA>   Sum 
#   5583  3141  2168 10892 

bind_swind_dr2$`Outcome of Single Assessment` <-
  ifelse(bind_swind_dr2$`Outcome of Single Assessment`== "No", 0, 
         ifelse(bind_swind_dr2$`Outcome of Single Assessment` == "Yes", 1,
                bind_swind_dr2$`Outcome of Single Assessment`))

# Create variable for age at time of referral ----
bind_swind_dr2$ageatref <- floor(as.numeric(difftime(bind_swind_dr2$`Referral Date`, 
                                                bind_swind_dr2$`Year and month of birth of the child`, 
                                                units = "days")/365.25))

colnames(bind_swind_dr2)
new_order <- c(1:10, 27, 11:26)
bind_swind_dr2 <- bind_swind_dr2[, new_order]

# Creating binary for whether referral reason included MH, DA, SU (trio of vulnerabilities) ----
trio_obs <- c("3B", "3A", "3C", "4B", "2B", "1B")

# Create a new binary column
# NOTE: More complicated code was needed to account for the fact most observations were NA.
bind_swind_dr2 <- bind_swind_dr2 %>%
  rowwise() %>%
  mutate(reftrio = ifelse(any(str_trim(c_across(starts_with("split_"))) %in% trio_obs, na.rm = TRUE),
                          1, 0)) %>% ungroup()
# Creating a unique identifier to merge on ----
bind_swind_dr2$concat_id <- paste(bind_swind_dr2$`Child ID`, bind_swind_dr2$`Referral ID (or Case ID)`)
# Add variable for LA -----
bind_swind_dr2$LA <- "Swindon"
# Adding LA to case ID ----
bind_swind_dr2$childla_id <- paste(bind_swind_dr2$`Child ID`, bind_swind_dr2$`LA`)
new_order <- c(31, 1:30)
bind_swind_dr2 <- bind_swind_dr2[, new_order]

##################################################
# Reading in TELFORD =========

## (Now fixed) FLAG Telford November 2022 has errors in it. The Excel has been concated. 

telford_dr2_nov20 <- read_excel("Data/FS_DR2/telford_dr2_nov20.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)

telford_dr2_nov21 <- read_excel("Data/FS_DR2/telford_dr2_nov21.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)

telford_dr2_nov22 <- read_excel("Data/FS_DR2/telford_dr2_nov22.xlsx",
                                sheet = "DR2 - Data - sample pop",
                                skip = 3)
#New names:
# * `Ethnicity` -> `Ethnicity...11`
# * `Ethnicity` -> `Ethnicity...13`

telford_dr2_apr23 <- read_excel("Data/FS_DR2/telford_dr2_apr23.xlsx",
                              sheet = "DR2 - Data - sample pop",
                              skip = 3)


# Deleting the bullet points -----
# APRIL 23
telford_dr2_apr23$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                                                         "", telford_dr2_apr23$`Outcome of Single Assessment`, 
                                                         ignore.case = TRUE)

telford_dr2_apr23$Gender <- gsub("^\\s*[a-z]\\)\\s*",
                                                         "", telford_dr2_apr23$Gender, 
                                                         ignore.case = TRUE)

telford_dr2_apr23$Ethnicity <- gsub("^\\s*[a-z]\\)\\s*",
                                 "", telford_dr2_apr23$Ethnicity, 
                                 ignore.case = TRUE)

# NOVEMBER 21
telford_dr2_nov21$Ethnicity <- gsub("^\\s*[a-z]\\)\\s*",
                                    "", telford_dr2_nov21$Ethnicity, 
                                    ignore.case = TRUE)

telford_dr2_nov21$Gender <- gsub("^\\s*[a-z]\\)\\s*",
                                    "", telford_dr2_nov21$Gender, 
                                    ignore.case = TRUE)


# RECODE year and month of child's birth. ----
# Explore the variables
colnames(telford_dr2_nov22)
# This information is split over two columns: 
# Column 10 "Year and month of birth of the child" contains the month. Column 11 "Ethnicity 11"  contains the year. 
# Exploring the variables 
class(telford_dr2_nov22$`Year and month of birth of the child`) # numeric 
class(telford_dr2_nov22$Ethnicity...11) # numeric 

# Concatenating DOB variable
telford_dr2_nov22$concat_DOB <- paste(telford_dr2_nov22$`Year and month of birth of the child`, 
                                      telford_dr2_nov22$Ethnicity...11, sep = "/")


# Recoding childs month and year of birth by file as they are formatted differently 
#Nov 2020
str(telford_dr2_nov20$`Year and month of birth of the child`)
telford_dr2_nov20$`Year and month of birth of the child` <- 
  as.Date(paste("01", telford_dr2_nov20$`Year and month of birth of the child`, sep = "/"), format = "%d/%m/%Y")

#Nov 2021
str(telford_dr2_nov21$`Year and month of birth of the child`)
telford_dr2_nov21$`Year and month of birth of the child` <- 
  as.Date(paste("01", telford_dr2_nov21$`Year and month of birth of the child`, sep = "-"), format = "%d-%m-%Y")

#Nov 2022
str(telford_dr2_nov22$concat_DOB)
telford_dr2_nov22$concat_DOB <-
  as.Date(paste("01", telford_dr2_nov22$concat_DOB, sep = "/"), format = "%d/%m/%Y")

# April 2023
str(telford_dr2_apr23$`Year and month of birth of the child`)
telford_dr2_apr23$`Year and month of birth of the child` <-
  as.Date(paste("01", telford_dr2_apr23$`Year and month of birth of the child`, sep = "/"), format = "%d/%m/%Y")

# Reordering the variables so that the concated is where DOB should be ----
# COmparing this against another
colnames(telford_dr2_nov20)
colnames(telford_dr2_nov21)

# DOB should be column 10 
new_order <- c(1:9, 16, 10:15)
telford_dr2_nov22 <- telford_dr2_nov22[, new_order]

# Dropping the old DOB variables 
telford_dr2_nov22 <- telford_dr2_nov22[, -c(11, 12, 13)]  # Drops columns 2 and 4

# Renaming columns in Telford 2022 ----
colnames(telford_dr2_nov22)
colnames(telford_dr2_nov20)

colnames(telford_dr2_nov22) <- colnames(telford_dr2_nov20)

# Deleting bullet points in NOVEMBER 22 ----
telford_dr2_nov22$Ethnicity <- gsub("^\\s*[a-z]\\)\\s*",
                                    "", telford_dr2_nov22$Ethnicity,
                                    ignore.case = TRUE)

# Exploring UASC before binding ----
telford_dr2_nov20 %>%
  select(`Unaccompanied Asylum Seeker`) %>%
  table(useNA= "ifany") %>%
  addmargins()

#Unaccompanied Asylum Seeking Child                               <NA>                                Sum 
#                                  2                               1520                               1522 

# Believe NA means not asylum seeking here. Check other data returns to see if the numbers look in the right region. 

telford_dr2_nov21%>%
  select(`Unaccompanied Asylum Seeker`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#British Citizen            N        Refugee Status               Y             Sum 
#            2             1831                 1                 7            1841 

#Slightly more complicated picture here. Code an extra category? Or include refugee status child in UASC? British Citizen?

telford_dr2_nov22 %>%
  select(`Unaccompanied Asylum Seeker`) %>%
  table(useNA= "ifany") %>%
  addmargins()

#  N                No Immigration Status Recorded        Y                            Sum 
#  11               2090                                  8                           2109 

# Again, more complicated picture. Why some no and some none recorded?

telford_dr2_apr23 %>%
  select(`Unaccompanied Asylum Seeker`) %>%
  table(useNA = "ifany") %>%
  addmargins ()

#   N     Y   Sum 
#   131   2   133 

# Telford 2022 0 coded as NA. Re-coding:
telford_dr2_nov22 <- telford_dr2_nov22 %>%
  mutate(`Number of previous child protection plans` = ifelse(is.na(`Number of previous child protection plans`), 
                                                              0, `Number of previous child protection plans`))


# Binding Telford together ----
bind_telford_dr2 <- bind_rows(telford_dr2_nov20, telford_dr2_nov21, telford_dr2_nov22, telford_dr2_apr23)

# Cleaning binded Telford file 
# TO DO in TELFORD
# # Factors identified at assesmment must be split so that each code has it's own column? Dummy coded? 
# Some columns have listing included, needs deleting (i.e a) b) ).  
# nov 22 has 'no immigration status' in words rather than Y N 
# Nov 20: has all NA for asylum seeker. Outcome at single assessment in words.


# Cleaning bound Telford ----
# Unaccompanied Asylum Seeker -----
# Explore the composition of the variable 
bind_telford_dr2 %>% 
  select(`Unaccompanied Asylum Seeker`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()

# Result
# British Citizen: 2          
# N: 1973
# Y: 17
# No Immigration Status Recorded: 2090
# Refugee Status: 1
# Unaccompanied Asylum Seeking Child: 2
# NA: 1520 (meaning not an UASC)
# sum: 5605

# Recode disability status 
bind_telford_dr2 %>%
  select(`Disabled status`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#   N    Y    Sum 
# 5208  397   5605 

bind_telford_dr2$`Disabled status` <-
  ifelse(bind_telford_dr2$`Disabled status` == "N", 0,
         ifelse(bind_telford_dr2$`Disabled status` == "Y", 1,
                bind_telford_dr2$`Disabled status`))

# Recode factors identified at end of assessment ----
# Split so that it is in separate columns 

# Split the original column by comma and convert it to a list
split_columns <- strsplit(bind_telford_dr2$`Factors identified at the end of assessment`, "[/,]")

# Calculate the maximum number of elements in any split
max_elements <- max(sapply(split_columns, length))

# Create new column names for the split columns
new_factors <- paste("split_", 1:max_elements, sep = "")

for (i in 1:max_elements) {
  bind_telford_dr2[new_factors[i]] <- sapply(split_columns, function(x) ifelse(length(x) >= i, x[i], NA))
}


# Recode outcome at single assessment as binary ----
bind_telford_dr2 %>%
  select(`Outcome of Single Assessment`) %>%
  table(useNA = "ifany") %>%
  addmargins()

# Child In Need - Active Child's Plan (CPlanActive_FP): 263 
# Legal Action (CoreLegal): 17 
# no : 2182 
# No : 72 
# No Further Action (NFACore): 382 
# On-Going Safeguarding Activity (CoreOther) : 189 
# Referral to Other Agency (CoreRefer) : 430 
# Strategy Discussion (StratDisc) : 54 
# yes :  1444 
# Yes : 44 
# <NA> : 528 
# Sum : 5605 

# Most options coded as yes (1) there is an outcome. Unsure about 'No Further Action (NFACore): 382'
# ACTION: Ask practice 

bind_telford_dr2$`Outcome of Single Assessment` <-
  ifelse(bind_telford_dr2$`Outcome of Single Assessment` == "yes", 1,
         ifelse(bind_telford_dr2$`Outcome of Single Assessment` == "Yes", 1,
                ifelse(bind_telford_dr2$`Outcome of Single Assessment` == "no", 0,
                       ifelse(bind_telford_dr2$`Outcome of Single Assessment` == "No", 0,
                              bind_telford_dr2$`Outcome of Single Assessment`))))

bind_telford_dr2$`Outcome of Single Assessment` <- 
  ifelse(bind_telford_dr2$`Outcome of Single Assessment` == "Child In Need - Active Child's Plan (CPlanActive_FP)", 1,
         ifelse(bind_telford_dr2$`Outcome of Single Assessment` == "Legal Action (CoreLegal)", 1,
                ifelse(bind_telford_dr2$`Outcome of Single Assessment` == "On-Going Safeguarding Activity (CoreOther)", 1,
                       ifelse(bind_telford_dr2$`Outcome of Single Assessment`== "Referral to Other Agency (CoreRefer)", 1,
                              ifelse(bind_telford_dr2$`Outcome of Single Assessment` == "Strategy Discussion (StratDisc)", 1,
                                     bind_telford_dr2$`Outcome of Single Assessment`)))))

#     0       1       No Further Action (NFACore) 
#   2254    2441                382 
#   <NA>         Sum 
#    528        5605 

# Create variable for age at time of referral ----
bind_telford_dr2$ageatref <- floor(as.numeric(difftime(bind_telford_dr2$`Referral Date`, 
                                                bind_telford_dr2$`Year and month of birth of the child`, 
                                                units = "days")/365.25))

new_order <- c(1:10, 30, 11:29)
bind_telford_dr2 <- bind_telford_dr2[, new_order]

# Creating binary for whether referral reason included MH, DA, SU (trio of vulnerabilities) ----
trio_obs <- c("3B", "3A", "3C", "4B", "2B", "1B")

# Create a new binary column
# NOTE: More complicated code was needed to account for the fact most observations were NA.
bind_telford_dr2 <- bind_telford_dr2 %>%
  rowwise() %>%
  mutate(reftrio = ifelse(any(str_trim(c_across(starts_with("split_"))) %in% trio_obs, na.rm = TRUE),
                          1, 0)) %>% ungroup()

# Creating a unique identifier to merge on ----
bind_telford_dr2$concat_id <- paste(bind_telford_dr2$`Child ID`, bind_telford_dr2$`Referral ID (or Case ID)`)
# Add variable for LA ----
bind_telford_dr2$LA <- "Telford"
# Adding LA to case ID ----
bind_telford_dr2$childla_id <- paste(bind_telford_dr2$`Child ID`, bind_telford_dr2$`LA`)
new_order <- c(34, 1:33)
bind_telford_dr2 <- bind_telford_dr2[, new_order]

#########################################################
# Reading in WALSALL =======
walsall_dr2_nov20 <- read_excel("Data/FS_DR2/walsall_dr2_nov20.xlsx",
                                sheet = "DR2 - Data - sample pop",
                                skip = 3)

walsall_dr2_nov21 <- read_excel("Data/FS_DR2/walsall_dr2_nov21.xlsx",
                                sheet = "DR2 - Data - sample pop",
                                skip = 3)

walsall_dr2_nov22 <- read_excel("Data/FS_DR2/walsall_dr2_nov22.xlsx",
                                sheet = "DR2 - Data - sample pop",
                                skip = 3)

walsall_dr2_apr23 <- read_excel("Data/FS_DR2/walsall_dr2_apr23.xlsx",
                                sheet = "DR2 - Data - sample pop",
                                skip = 3)

# Deleting bullet points from column names----------
# APRIL 23
walsall_dr2_apr23$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                              "", walsall_dr2_apr23$`Outcome of Single Assessment`, 
                              ignore.case = TRUE)

walsall_dr2_apr23$Ethnicity <- gsub("^\\s*[a-z]\\)\\s*",
                                                         "", walsall_dr2_apr23$Ethnicity, 
                                                         ignore.case = TRUE)

walsall_dr2_apr23$`Disabled status` <- gsub("^\\s*[a-z]\\)\\s*",
                                    "", walsall_dr2_apr23$`Disabled status`, 
                                    ignore.case = TRUE)


# NOVEMBER 20
walsall_dr2_nov20$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                                                         "", walsall_dr2_nov20$`Outcome of Single Assessment`, 
                                                         ignore.case = TRUE)

walsall_dr2_nov20$Ethnicity <- gsub("^\\s*[a-z]\\)\\s*",
                                    "", walsall_dr2_nov20$Ethnicity, 
                                    ignore.case = TRUE)

walsall_dr2_nov20$`Disabled status` <- gsub("^\\s*[a-z]\\)\\s*",
                                            "", walsall_dr2_nov20$`Disabled status`, 
                                            ignore.case = TRUE)

walsall_dr2_nov20$Gender <- gsub("^\\s*[a-z]\\)\\s*",
                                            "", walsall_dr2_nov20$Gender, 
                                            ignore.case = TRUE)
# NOVEMBER 21
walsall_dr2_nov21$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                                                         "", walsall_dr2_nov21$`Outcome of Single Assessment`, 
                                                         ignore.case = TRUE)

walsall_dr2_nov21$Ethnicity <- gsub("^\\s*[a-z]\\)\\s*",
                                    "", walsall_dr2_nov21$Ethnicity, 
                                    ignore.case = TRUE)

walsall_dr2_nov21$Gender <- gsub("^\\s*[a-z]\\)\\s*",
                                 "", walsall_dr2_nov21$Gender, 
                                 ignore.case = TRUE)

# NOVEMBER 22
walsall_dr2_nov22$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                                                         "", walsall_dr2_nov22$`Outcome of Single Assessment`, 
                                                         ignore.case = TRUE)

walsall_dr2_nov22$Ethnicity <- gsub("^\\s*[a-z]\\)\\s*",
                                    "", walsall_dr2_nov22$Ethnicity, 
                                    ignore.case = TRUE)

walsall_dr2_nov22$Gender <- gsub("^\\s*[a-z]\\)\\s*",
                                 "", walsall_dr2_nov22$Gender, 
                                 ignore.case = TRUE)

# Cleaning assessment actual start date before able to bind ----
# Exploring the variable 
dataframe_names <- c("walsall_dr2_nov20", "walsall_dr2_nov21", "walsall_dr2_nov22", "walsall_dr2_apr23")
# Loop over the dataframe names
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  asses_class <- class(df$`Assessment actual start date`)
  
  # Print the result
  cat("Class of assessment actual start date in Walsall", df_name, ":", asses_class, "\n")
}

#Class of assessment actual start date in Walsall walsall_dr2_nov20 : POSIXct POSIXt 
#Class of assessment actual start date in Walsall walsall_dr2_nov21 : character 
#Class of assessment actual start date in Walsall walsall_dr2_nov22 : POSIXct POSIXt 
#Class of assessment actual start date in Walsall walsall_dr2_apr23 : POSIXct POSIXt 

# Assessment actual start date for November 21 is in strange Excel format. Recoding it to be date friendly. ----

# Look at missing 
is.na(walsall_dr2_nov21$`Assessment actual start date`) %>% sum () #0
# Recode 'NULL' as NA
walsall_dr2_nov21$`Assessment actual start date` <- ifelse(walsall_dr2_nov21$`Assessment actual start date` 
                                                           == "NULL", NA, walsall_dr2_nov21$`Assessment actual start date`)

is.na(walsall_dr2_nov21$`Assessment actual start date`) %>% sum () # 213

# Code to numeric 
walsall_dr2_nov21$`Assessment actual start date` <- as.numeric(walsall_dr2_nov21$`Assessment actual start date`)
is.na(walsall_dr2_nov21$`Assessment actual start date`) %>% sum () # 213

# Converting to date
walsall_dr2_nov21$`Assessment actual start date` <- as.Date(walsall_dr2_nov21$`Assessment actual start date`, origin = "1899-12-30")
#NOTE: Assessment actual start date same as referal date for all. Go to original file and check if this is the case. 

# Recode: Unaccompanied asylum seeker ----
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  uasc_class <- class(df$`Unaccompanied Asylum Seeker`)
  
  # Print the result
  cat("Class of unaccompanied asylum SC in Walsall", df_name, ":", uasc_class, "\n")
}

#Class of unaccompanied asylum SC in Walsall walsall_dr2_nov20 : character 
#Class of unaccompanied asylum SC in Walsall walsall_dr2_nov21 : character 
#Class of unaccompanied asylum SC in Walsall walsall_dr2_nov22 : numeric 
#Class of unaccompanied asylum SC in Walsall walsall_dr2_apr23 : numeric 

# Checking composition of UASC
walsall_dr2_nov20 %>% 
  select(`Unaccompanied Asylum Seeker`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
#    N  Sum 
#  3098 3098 
# NOTE: zero UASC in Walsall at this time?

walsall_dr2_nov21 %>% 
  select(`Unaccompanied Asylum Seeker`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
#   N      Y   Sum 
#  3825    5   3830

walsall_dr2_nov22 %>% 
  select(`Unaccompanied Asylum Seeker`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
#    0    1   Sum 
#  4014   12  4026 

walsall_dr2_apr23 %>% 
  select(`Unaccompanied Asylum Seeker`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
#  0    1  Sum 
# 346   3  349 

# Recode Wal 20 and Wal 21 so that is it a binary numeric value, 0 = not UASC 1 = UASC ----
# Walsall 2020
walsall_dr2_nov20$`Unaccompanied Asylum Seeker` <- 
  ifelse(walsall_dr2_nov20$`Unaccompanied Asylum Seeker`== "N", 0, 
         walsall_dr2_nov20$`Unaccompanied Asylum Seeker`)

# Walsall 2021
walsall_dr2_nov21$`Unaccompanied Asylum Seeker` <- 
  ifelse(walsall_dr2_nov21$`Unaccompanied Asylum Seeker` == "N", 0, 
         ifelse(walsall_dr2_nov21$`Unaccompanied Asylum Seeker` == "Y", 1, 
                walsall_dr2_nov21$`Unaccompanied Asylum Seeker`))

# Class still shows as character, converting to numeric. 
walsall_dr2_nov21$`Unaccompanied Asylum Seeker` <- 
  as.numeric(walsall_dr2_nov21$`Unaccompanied Asylum Seeker`)

# Recode Child month and year of birth in Nov 20 and Nov 21 as it is currently in the Excel format -----

# NOVEMBER 2020
str(walsall_dr2_nov20$`Year and month of birth of the child`)

#Check for missing
is.na(walsall_dr2_nov20$`Year and month of birth of the child`) %>% sum ()  #0

# recode to numeric 
walsall_dr2_nov20$`Year and month of birth of the child` <- 
  as.numeric(walsall_dr2_nov20$`Year and month of birth of the child`)
# Check this?? Warning message:
# NAs introduced by coercion
is.na(walsall_dr2_nov20$`Year and month of birth of the child`) %>% sum ()  #5
# Investigate why there is missiness: 'Not stated/recorded (or unborn)'. 

# Recode the dates into R date format
walsall_dr2_nov20$`Year and month of birth of the child` <- 
  as.Date(walsall_dr2_nov20$`Year and month of birth of the child`, origin = "1899-12-30")

# Recode year and month of birth of child for ----
# NOVEMBER 2021
str(walsall_dr2_nov21$`Year and month of birth of the child`)

is.na(walsall_dr2_nov21$`Year and month of birth of the child`) %>% sum ()    # 0

# Recode as numeric 
walsall_dr2_nov21$`Year and month of birth of the child` <- 
  as.numeric(walsall_dr2_nov21$`Year and month of birth of the child`)
# Warning message:
# NAs introduced by coercion 

# Check for missing 
is.na(walsall_dr2_nov21$`Year and month of birth of the child`) %>% sum () # 12
# Check the 12 missing, 2 unexplained. 10 = 	Not stated/recorded (or unborn)

# Recode as date from Excel format
walsall_dr2_nov21$`Year and month of birth of the child` <- 
  as.Date(walsall_dr2_nov21$`Year and month of birth of the child`, origin = "1899-12-30")

# Check the class of all the year and month variables for the Walsall files
dataframe_names <- c("walsall_dr2_nov20", "walsall_dr2_nov21", "walsall_dr2_nov22", "walsall_dr2_apr23")

for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  yearbirth_class <- class(df$`Year and month of birth of the child`)
  
  # Print the result
  cat("Class of year and month of birth in Walsall", df_name, ":", yearbirth_class, "\n")
}

#Class of year and month of birth in Walsall walsall_dr2_nov20 : Date 
#Class of year and month of birth in Walsall walsall_dr2_nov21 : Date 
#Class of year and month of birth in Walsall walsall_dr2_nov22 : character 
#Class of year and month of birth in Walsall walsall_dr2_apr23 : character 

# Change NOV 2022 into a date 
str(walsall_dr2_nov22$`Year and month of birth of the child`)
walsall_dr2_nov22$`Year and month of birth of the child` <- 
  as.Date(paste("01", walsall_dr2_nov22$`Year and month of birth of the child`, sep = "/"), 
          format = "%d/%m/%Y")
# Check it works 
str(walsall_dr2_nov22$`Year and month of birth of the child`)


# Change APRIL 2023
str(walsall_dr2_apr23$`Year and month of birth of the child`)
walsall_dr2_apr23$`Year and month of birth of the child` <- 
  as.Date(paste("01", walsall_dr2_apr23$`Year and month of birth of the child`, sep = "/"), 
          format = "%d/%m/%Y")
 # Check it worked
str(walsall_dr2_apr23$`Year and month of birth of the child`)

# Binding Walsall ----
bind_walsall_dr2 <- bind_rows(walsall_dr2_nov20, walsall_dr2_nov21, walsall_dr2_nov22, walsall_dr2_apr23)


# TO DO IN WALSALL 
# Factors identified at assesmment must be split so that each code has it's own column? Dummy coded? 

# CLEANING binded  ----
# Recode Disability status ----
bind_walsall_dr2 %>%
  select(`Disabled status`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#    N    No     Y    Sum 
# 10833   349   121   11303 

bind_walsall_dr2$`Disabled status` <-
  ifelse(bind_walsall_dr2$`Disabled status` == "N", 0,
         ifelse(bind_walsall_dr2$`Disabled status` == "No", 0,
                ifelse(bind_walsall_dr2$`Disabled status` == "Y", 1,
                       bind_walsall_dr2$`Disabled status`)))

# Checking this has worked
bind_walsall_dr2 %>%
  select(`Disabled status`) %>%
  table(useNA = "ifany") %>%
  addmargins()

#    0      1    Sum 
# 11182   121    11303 

# Recode factors identified at end of assessment ----
# Split the original column by comma and convert it to a list
#NOTE: for some reason, this has to be done with space, then ; then space again. 
# Nov 22 and Apr 23: " " (space) # Nov 20 and Nov 21: ";"
#Allow for there being a trailing ; after some of these observations
split_columns <- strsplit(bind_walsall_dr2$`Factors identified at the end of assessment`, " ")

# Calculate the maximum number of elements in any split
max_elements <- max(sapply(split_columns, length))

# Create new column names for the split columns
new_factors <- paste("split_", 1:max_elements, sep = "")

for (i in 1:max_elements) {
  bind_walsall_dr2[new_factors[i]] <- sapply(split_columns, function(x) ifelse(length(x) >= i, x[i], NA))
}


# NOT WORKING YET
# Recode year and month of child's birth so that it's in date format 
# the date for unborn is '1900-01-01'.
# Recoding the date for unborn from '1900-01-01' to the same date as 
# the referral date, so that it shows as 0. 
#bind_walsall_dr2$`Year and month of birth of the child` <- ifelse(
#  bind_walsall_dr2$Gender == 'Not stated/recorded (or unborn)', 
#  bind_walsall_dr2$`Referral Date`, 
#  bind_walsall_dr2$`Year and month of birth of the child`
#)

# Recode outcome at single assessment as binary----
str(bind_walsall_dr2$`Outcome of Single Assessment`)

bind_walsall_dr2 %>%
  select(`Outcome of Single Assessment`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#   No    NULL   Yes    <NA>   Sum 
#  7649    204   3044   406    11303

# Recode No as 0. Yes as 1. and NULL as NA.
bind_walsall_dr2$`Outcome of Single Assessment` <-
  ifelse(bind_walsall_dr2$`Outcome of Single Assessment` == "No", 0,
         ifelse(bind_walsall_dr2$`Outcome of Single Assessment` == "Yes", 1,
                 ifelse(bind_walsall_dr2$`Outcome of Single Assessment` == "NULL", NA, 
                        bind_walsall_dr2$`Outcome of Single Assessment`)))

bind_walsall_dr2 %>%
  select(`Outcome of Single Assessment`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#    0     1   <NA>   Sum 
#  7649  3044   610   11303 

# Create varable for age at time of referral  ----
bind_walsall_dr2$ageatref <- floor(as.numeric(difftime(bind_walsall_dr2$`Referral Date`, 
                                                bind_walsall_dr2$`Year and month of birth of the child`, 
                                                units = "days")/365.25))

range(bind_walsall_dr2$ageatref, na.rm = TRUE)   #-1 122

# Children with age set as 122 are 'not stated/unborn'. They all come from 1 file where
# The DOB was saved in irregular Excel format. Prsume they are unborn as other data
# Appears complete. ACTION: Recode age at referral to 0.

bind_walsall_dr2$ageatref <- ifelse(bind_walsall_dr2$ageatref == 122, 0, 
                                    bind_walsall_dr2$ageatref)



# New order ----
new_order <- c(1:10, 27, 11:26)
bind_walsall_dr2 <- bind_walsall_dr2[, new_order]

# Creating binary for whether referral reason included MH, DA, SU (trio of vulnerabilities) ----
trio_obs <- c("3B", "3A", "3C", "4B", "2B", "1B", "3B;", "3A;", "3C;", "4B;", "2B;", "1B;")

# Create a new binary column
# NOTE: More complicated code was needed to account for the fact most observations were NA.
bind_walsall_dr2 <- bind_walsall_dr2 %>%
  rowwise() %>%
  mutate(reftrio = ifelse(any(str_trim(c_across(starts_with("split_"))) %in% trio_obs, na.rm = TRUE),
                          1, 0)) %>% ungroup()
# Creating a unique identifier to merge on ----
bind_walsall_dr2$concat_id <- paste(bind_walsall_dr2$`Child ID`, bind_walsall_dr2$`Referral ID (or Case ID)`)
# Add variable for LA ----
bind_walsall_dr2$LA <- "Walsall"
# Adding LA to case ID ----
bind_walsall_dr2$childla_id <- paste(bind_walsall_dr2$`Child ID`, bind_walsall_dr2$`LA`)
new_order <- c(31, 1:30)
bind_walsall_dr2 <- bind_walsall_dr2[, new_order]

##########################################################
# Reading in WANDSWORTH ======
wands_dr2_nov20 <- read_excel("Data/FS_DR2/wandsworth_dr2_nov20.xlsx",
                                sheet = "DR2 - Data - sample pop",
                                skip = 3)

wands_dr2_nov21 <- read_excel("Data/FS_DR2/wandsworth_dr2_nov21.xlsx",
                                skip = 3)

wands_dr2_nov22 <- read_excel("Data/FS_DR2/wandsworth_dr2_nov22.xlsx",
                                sheet = "DR2 - Data - sample pop",
                                skip = 3)
#Warning messages:
#  1: Expecting date in C5659 / R5659C3: got 'NULL' 
#2: Expecting date in C5660 / R5660C3: got 'NULL' 
#3: Expecting date in C5661 / R5661C3: got 'NULL' 
#4: Expecting date in C5662 / R5662C3: got 'NULL' 
#5: Expecting date in C5663 / R5663C3: got 'NULL' 
#6: Expecting date in C5664 / R5664C3: got 'NULL'

wands_dr2_apr23 <- read_excel("Data/FS_DR2/wandsworth_dr2_apr23.xlsx",
                                sheet = "DR2 - Data - sample pop",
                                skip = 3)

# Deleting bullet points from columns---------
# APRIL 23
wands_dr2_apr23$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                                                         "", wands_dr2_apr23$`Outcome of Single Assessment`, 
                                                         ignore.case = TRUE)

wands_dr2_apr23$Gender <- gsub("^\\s*[a-z]\\)\\s*",
                                                       "", wands_dr2_apr23$Gender, 
                                                       ignore.case = TRUE)

# NOVEMBER 20 
wands_dr2_nov20$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                                                       "", wands_dr2_nov20$`Outcome of Single Assessment`,
                                                       ignore.case = TRUE)

wands_dr2_nov20$Ethnicity <- gsub("^\\s*[a-z]\\)\\s*",
                                                       "", wands_dr2_nov20$Ethnicity,
                                                       ignore.case = TRUE)

# NOVEMBER 21
wands_dr2_nov21$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                                                       "", wands_dr2_nov21$`Outcome of Single Assessment`, 
                                                       ignore.case = TRUE)

wands_dr2_nov21$Gender <- gsub("^\\s*[a-z]\\)\\s*",
                               "", wands_dr2_nov21$Gender, 
                               ignore.case = TRUE)

# NOVEMBER 22
wands_dr2_nov22$`Outcome of Single Assessment` <- gsub("^\\s*[a-z]\\)\\s*",
                                                       "", wands_dr2_nov22$`Outcome of Single Assessment`, 
                                                       ignore.case = TRUE)

wands_dr2_nov22$Gender <- gsub("^\\s*[a-z]\\)\\s*",
                               "", wands_dr2_nov22$Gender, 
                               ignore.case = TRUE)

# Recode Referal ID so that they are the same class ----
# Investiagate the class of the variable 
dataframe_names <- c("wands_dr2_nov20", "wands_dr2_nov21", "wands_dr2_nov22", "wands_dr2_apr23")

for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  refid_class <- class(df$`Referral ID (or Case ID)`)
  
  # Print the result
  cat("Class of referral ID in Wandsworth", df_name, ":", refid_class, "\n")
}

#Class of referral ID in Wandsworth wands_dr2_nov20 : character 
#Class of referral ID in Wandsworth wands_dr2_nov21 : numeric 
#Class of referral ID in Wandsworth wands_dr2_nov22 : numeric 
#Class of referral ID in Wandsworth wands_dr2_apr23 : numeric 

# Explore Nov 20 
str(wands_dr2_nov20$`Referral ID (or Case ID)`)
# See missing
is.na(wands_dr2_nov20$`Referral ID (or Case ID)`) %>% sum () #0

# Investigating whether to recode to character
str(wands_dr2_nov21$`Referral ID (or Case ID)`)

# Convert to numeric
wands_dr2_nov20$`Referral ID (or Case ID)` <- 
  as.numeric(wands_dr2_nov20$`Referral ID (or Case ID)`, na.rm = TRUE)
# Check to see if any observations were lost
is.na(wands_dr2_nov20$`Referral ID (or Case ID)`) %>% sum () #0
# Nothing lost 
str(wands_dr2_nov20$`Referral ID (or Case ID)`)

# Recode Assessment actual start date ----
for (df_name in dataframe_names) {
  df <- get(df_name)
  asses_class <- class(df$`Assessment actual start date`)
  cat("Class of assessment dates in Wandsworth", df_name, ":", asses_class, "\n")
}

#Class of assessment dates in Wandsworth wands_dr2_nov20 : character 
#Class of assessment dates in Wandsworth wands_dr2_nov21 : POSIXct POSIXt 
#Class of assessment dates in Wandsworth wands_dr2_nov22 : POSIXct POSIXt 
#Class of assessment dates in Wandsworth wands_dr2_apr23 : POSIXct POSIXt 

# Explore Nov 20 as it's character
str(wands_dr2_nov20$`Assessment actual start date`)
# Has the Excel format
is.na(wands_dr2_nov20$`Assessment actual start date`) %>% sum () # 0
# Missing is coded as N/A as a character, swtich this to missing. 
wands_dr2_nov20$`Assessment actual start date` <- 
  ifelse(wands_dr2_nov20$`Assessment actual start date`== "N/A", NA,
         wands_dr2_nov20$`Assessment actual start date`)

# Checking on missing 
is.na(wands_dr2_nov20$`Assessment actual start date`) %>% sum ()  # 219

# Switching to numeric
wands_dr2_nov20$`Assessment actual start date` <- as.numeric(wands_dr2_nov20$`Assessment actual start date`)
str(wands_dr2_nov20$`Assessment actual start date`)

# Switching to date in R format
wands_dr2_nov20$`Assessment actual start date` <- 
  as.Date(wands_dr2_nov20$`Assessment actual start date`, origin = "1899-12-30")

# Recode Referral Date ----
# APRIL 23 has Excel date
str(wands_dr2_apr23$`Referral Date`)
# Looking at missing 
is.na(wands_dr2_apr23$`Referral Date`) %>% sum () #0

# 1 'NULL' observation. Recode
wands_dr2_apr23$`Referral Date` <-
  ifelse(wands_dr2_apr23$`Referral Date`== "NULL", NA,
         wands_dr2_apr23$`Referral Date`)

# Check it worked
is.na(wands_dr2_apr23$`Referral Date`) %>% sum () #1

# Convert to numeric 
wands_dr2_apr23$`Referral Date`<- as.numeric(wands_dr2_apr23$`Referral Date`)
str(wands_dr2_apr23$`Referral Date`)

# Convert to date 
wands_dr2_apr23$`Referral Date`<- as.Date(wands_dr2_apr23$`Referral Date`, origin = "1899-12-30")

# Recoding year and month of birth ----------------
# Nov 20
class(wands_dr2_nov20$`Year and month of birth of the child`)
str(wands_dr2_nov20$`Year and month of birth of the child`)
wands_dr2_nov20$`Year and month of birth of the child` <- 
  as.Date(paste("01", wands_dr2_nov20$`Year and month of birth of the child`, 
                sep = "/"), format = "%d/%m/%Y")

# Nov 21
class(wands_dr2_nov21$`Year and month of birth of the child`)
str(wands_dr2_nov21$`Year and month of birth of the child`)
wands_dr2_nov21$`Year and month of birth of the child` <- 
  as.Date(paste("01-", wands_dr2_nov21$`Year and month of birth of the child`, 
                sep = ""), format = "%d-%m-%Y")

# Nov 22
class(wands_dr2_nov22$`Year and month of birth of the child`)
str(wands_dr2_nov22$`Year and month of birth of the child`)
wands_dr2_nov22$`Year and month of birth of the child` <- 
  as.Date(paste("01-", wands_dr2_nov22$`Year and month of birth of the child`, 
                sep = ""), format = "%d-%m-%Y")

# April 23
class(wands_dr2_apr23$`Year and month of birth of the child`)
str(wands_dr2_apr23$`Year and month of birth of the child`)
wands_dr2_apr23$`Year and month of birth of the child` <- 
as.Date(paste("01", wands_dr2_apr23$`Year and month of birth of the child`, 
              sep = "/"), format = "%d/%m/%Y")


is.na(wands_dr2_nov20$`Year and month of birth of the child`) %>% sum () #41
is.na(wands_dr2_nov21$`Year and month of birth of the child`) %>% sum () #18
is.na(wands_dr2_nov22$`Year and month of birth of the child`) %>% sum () #38
is.na(wands_dr2_apr23$`Year and month of birth of the child`) %>% sum () #1

# Are these missing all unborn? In which case recode so as not to miss. 

# BINDING Wandsworth ----
bind_wands_dr2 <- bind_rows(wands_dr2_nov20, wands_dr2_nov21, wands_dr2_nov22, wands_dr2_apr23)

# Recoding NAs where the reason is 'Not stated/recorded (or unborn)' as 0?


# TO DO WANDS
# Factors identified at end of assessment needs work: Nov 20 is in words rather than code. Other years must be split so that each code has it's own column? Dummy coded? 

# Cleaning binded Wandsworth----
# Recode UASC ----
# Explore variable 
bind_wands_dr2 %>% 
  select(`Unaccompanied Asylum Seeker`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
#     N     Y     Sum 
#  12078   114   12192 

# Recode to dummy/binary variable. 
bind_wands_dr2$`Unaccompanied Asylum Seeker` <-
  ifelse(bind_wands_dr2$`Unaccompanied Asylum Seeker` == "N", 0,
         ifelse(bind_wands_dr2$`Unaccompanied Asylum Seeker` == "Y", 1,
                bind_wands_dr2$`Unaccompanied Asylum Seeker`))

# See if this worked
bind_wands_dr2 %>% 
  select(`Unaccompanied Asylum Seeker`) %>% 
  table(useNA = "ifany") %>% 
  addmargins()
#    0     1    Sum 
#  12078   114  12192 

# Recode Disability ----
bind_wands_dr2 %>%
  select(`Disabled status`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#    N     Y    Sum 
# 11490   702   12192 

#Recode to binary variable 
bind_wands_dr2$`Disabled status` <-
  ifelse(bind_wands_dr2$`Disabled status` == "N", 0,
         ifelse(bind_wands_dr2$`Disabled status` == "Y", 1,
                bind_wands_dr2$`Disabled status`))

# Check to see if it worked
bind_wands_dr2 %>%
  select(`Disabled status`) %>%
  table(useNA = "ifany") %>%
  addmargins()
# 0        1    Sum 
# 11490    702  12192 

# Recode factors identified at end of assessment.
# Wandsworth November 2020 did this in words. Ask them to send in code? OR recode manually. 

# Recode factors identified at assessment----
# Spliting the data up so that it is in separate columns.

# Split the original column by comma and convert it to a list
split_columns <- strsplit(bind_wands_dr2$`Factors identified at the end of assessment`, ",")

# Calculate the maximum number of elements in any split
max_elements <- max(sapply(split_columns, length))

# Create new column names for the split columns
new_factors <- paste("split_", 1:max_elements, sep = "")

for (i in 1:max_elements) {
  bind_wands_dr2[new_factors[i]] <- sapply(split_columns, function(x) ifelse(length(x) >= i, x[i], NA))
}

# Nov 2020 for factors identified at end of assessment is in words rather than code.
wands_dr2_nov20 %>%
  select(`Factors identified at the end of assessment`) %>%
  table(useNA = "ifany") %>%
  addmargins()

# Finding the code for 'trio of vulnerabilities' (MH/DA/SU)
# ",Domestic violence - Parent/Carer" = 3B 
# ",Domestic violence - Child" = 3A
# ",Domestic violence - Other" = 3C
# ",Mental health concerns - Parent/Carer" = 4B
# ",Drug misuse - Parent/Carer" = 2B
# ",Alcohol misuse - Parent/Carer" = 1B


# Recoding the factors identified at assessment from words to number code: ----

# Create a vector of column names to apply the recoding
cols_to_recode <- paste0("split_", 1:18)

# PARENTAL MENTAL HEALTH ----
# Define the recode function
# Make sure to trim white space around the words
recode_function <- function(x) {
  ifelse(trimws(x) == "Mental health concerns - Parent/Carer", "4B", x)
}

# Apply the recode function to selected columns by column number
bind_wands_dr2 <- bind_wands_dr2 %>%
  mutate_at(vars(cols_to_recode), funs(recode_function(.)))

# PARENTAL DRUG MISUSE ----
recode_function <- function(x) {
  ifelse(trimws(x) == "Drug misuse - Parent/Carer", "2B", x)
}

# Apply the recode function to selected columns by column number
bind_wands_dr2 <- bind_wands_dr2 %>%
  mutate_at(vars(cols_to_recode), funs(recode_function(.)))

# PARENTAL ALCOHOL MISUSE ----
recode_function <- function(x) {
  ifelse(trimws(x) == "Alcohol misuse - Parent/Carer", "1B", x)
}

# Apply the recode function to selected columns by column number
bind_wands_dr2 <- bind_wands_dr2 %>%
  mutate_at(vars(cols_to_recode), funs(recode_function(.)))

# DV PARENT ----
recode_function <- function(x) {
  ifelse(trimws(x) == "Domestic violence - Parent/Carer", "3B", x)
}

# Apply the recode function to selected columns by column number
bind_wands_dr2 <- bind_wands_dr2 %>%
  mutate_at(vars(cols_to_recode), funs(recode_function(.)))

# DV CHILD ----
recode_function <- function(x) {
  ifelse(trimws(x) == "Domestic violence - Child", "3A", x)
}

# Apply the recode function to selected columns by column number
bind_wands_dr2 <- bind_wands_dr2 %>%
  mutate_at(vars(cols_to_recode), funs(recode_function(.)))


# DV OTHER ----
recode_function <- function(x) {
  ifelse(trimws(x) == "Domestic violence - Other", "3C", x)
}

# Apply the recode function to selected columns by column number
bind_wands_dr2 <- bind_wands_dr2 %>%
  mutate_at(vars(cols_to_recode), funs(recode_function(.)))


# Recode outcome at single assessment as binary----
str(bind_wands_dr2$`Outcome of Single Assessment`)

bind_wands_dr2 %>%
  select(`Outcome of Single Assessment`) %>%
  table(useNA = "ifany") %>%
  addmargins()
# Incomplete      N/A        no        No     Not recorded      yes       Yes 
#     18          219       572      5682          604          2467      861 
#  <NA>          Sum 
#   1769        12192 

# Code the no, No as : 0. Code the yes and Yes as : 1. Code incomplete, N/A <NA> and not recorded as NA. 

bind_wands_dr2$`Outcome of Single Assessment` <-
  ifelse(bind_wands_dr2$`Outcome of Single Assessment`== "Incomplete", NA,
         ifelse(bind_wands_dr2$`Outcome of Single Assessment` == "no", 0,
                ifelse(bind_wands_dr2$`Outcome of Single Assessment` == "yes", 1,
                       bind_wands_dr2$`Outcome of Single Assessment`)))

bind_wands_dr2$`Outcome of Single Assessment` <-
  ifelse(bind_wands_dr2$`Outcome of Single Assessment`== "N/A", NA,
         ifelse(bind_wands_dr2$`Outcome of Single Assessment` == "No", 0,
                ifelse(bind_wands_dr2$`Outcome of Single Assessment` == "Yes", 1,
                       bind_wands_dr2$`Outcome of Single Assessment`)))

bind_wands_dr2$`Outcome of Single Assessment` <-
  ifelse(bind_wands_dr2$`Outcome of Single Assessment`== "Not recorded", NA,
         bind_wands_dr2$`Outcome of Single Assessment`)

bind_wands_dr2 %>%
  select(`Outcome of Single Assessment`) %>%
  table(useNA = "ifany") %>%
  addmargins()
# 0     1     <NA>   Sum 
# 6254  3328  2610   12192 

# Converting year and month into age at time of referral ----
bind_wands_dr2$ageatref <- floor(as.numeric(difftime(bind_wands_dr2$`Referral Date`, 
                                                bind_wands_dr2$`Year and month of birth of the child`, 
                                                units = "days")/365.25))

new_order <- c(1:10, 32, 11:31)
bind_wands_dr2 <- bind_wands_dr2[, new_order]


# Creating binary for whether referral reason included MH, DA, SU (trio of vulnerabilities) ----
# 0 = not present. 1 = present.
# Define the observations to check for
trio_obs <- c("3B", "3A", "3C", "4B", "2B", "1B")

# Create a new binary column
# NOTE: More complicated code was needed to account for the fact most observations were NA.
bind_wands_dr2 <- bind_wands_dr2 %>%
  rowwise() %>%
  mutate(reftrio = ifelse(any(str_trim(c_across(starts_with("split_"))) %in% trio_obs, na.rm = TRUE),
                          1, 0)) %>% ungroup()

# Creating a unique identifier to merge on ----
bind_wands_dr2$concat_id <- paste(bind_wands_dr2$`Child ID`, bind_wands_dr2$`Referral ID (or Case ID)`)

# Add variable for LA ----
bind_wands_dr2$LA <- "Wandsworth"
# Adding LA to case ID ----
bind_wands_dr2$childla_id <- paste(bind_wands_dr2$`Child ID`, bind_wands_dr2$`LA`)
new_order <- c(36, 1:35)
bind_wands_dr2 <- bind_wands_dr2[, new_order]

###Duplicates 
# Check for duplicate rows where all columns are the same
duplicates <- duplicated(bind_wands_dr2) | duplicated(bind_wands_dr2, fromLast = TRUE)

# duplicates: 6,208 

# Show duplicate rows
wandduplicate_rows <- bind_wands_dr2[duplicates, ]
print(wandduplicate_rows)

# I checked, and none of the individual Wandsworth files include duplicates. This suggests that their are time overlaps between them.

is.na(bind_wands_dr2$`Year and month of birth of the child`) %>% sum ()  #8806


################################################################################
# Preparing the datasets: recoding so that they are in a format that can be merged ----
# Exploring the class of Child ID
dataframe_names <- c("bind_lancs_dr2", "bind_swind_dr2", "bind_telford_dr2", "bind_walsall_dr2", "bind_wands_dr2")

# Child ID ----
# Loop over the dataframe names
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  childid_class <- class(df$`Child ID`)
  
  # Print the result
  cat("Class of child id in Dr2", df_name, ":", childid_class, "\n")
}

#Class of child id in Dr2 bind_lancs_dr2 : numeric 
#Class of child id in Dr2 bind_swind_dr2 : numeric 
#Class of child id in Dr2 bind_telford_dr2 : character 
#Class of child id in Dr2 bind_walsall_dr2 : numeric 
#Class of child id in Dr2 bind_wands_dr2 : numeric 

#As some of telford's referral IDs contain letters, conver the rest to character. Re-run is.na after each conversion to ensure no NA added.
# Lanc
is.na (bind_lancs_dr2$`Child ID`) %>% sum () # 0
bind_lancs_dr2$`Child ID` <- as.character(bind_lancs_dr2$`Child ID`)
# swind
is.na(bind_swind_dr2$`Child ID`) %>% sum () #0
bind_swind_dr2$`Child ID` <- as.character(bind_swind_dr2$`Child ID`)
# walsall
is.na(bind_walsall_dr2$`Child ID`) %>% sum () #0
bind_walsall_dr2$`Child ID` <- as.character(bind_walsall_dr2$`Child ID`)
# wands
is.na(bind_wands_dr2$`Child ID`) %>% sum () #0
bind_wands_dr2$`Child ID` <- as.character(bind_wands_dr2$`Child ID`)


# As some of Swindon's referral IDs contain letters, convert the rest to charcter. 
# Lanc
is.na(bind_lancs_dr2$`Referral ID (or Case ID)`) %>% sum ()   #0
bind_lancs_dr2$`Referral ID (or Case ID)` <- as.character(bind_lancs_dr2$`Referral ID (or Case ID)`)
# Tel
is.na(bind_telford_dr2$`Referral ID (or Case ID)`) %>% sum ()   #0
bind_telford_dr2$`Referral ID (or Case ID)` <- as.character(bind_telford_dr2$`Referral ID (or Case ID)`)
# Walsall
is.na(bind_walsall_dr2$`Referral ID (or Case ID)`) %>% sum ()   #0
bind_walsall_dr2$`Referral ID (or Case ID)` <- as.character(bind_walsall_dr2$`Referral ID (or Case ID)`)
# Wandsworth
is.na(bind_wands_dr2$`Referral ID (or Case ID)`) %>% sum ()   #0
bind_wands_dr2$`Referral ID (or Case ID)` <- as.character(bind_wands_dr2$`Referral ID (or Case ID)`)

# Referral ID ----
# Loop over the dataframe names
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  refid_class <- class(df$`Referral ID (or Case ID)`)
  
  # Print the result
  cat("Class of ref id in Dr2", df_name, ":", refid_class, "\n")
}

#Class of ref id in Dr2 bind_lancs_dr2 : numeric 
#Class of ref id in Dr2 bind_swind_dr2 : character 
#Class of ref id in Dr2 bind_telford_dr2 : numeric 
#Class of ref id in Dr2 bind_walsall_dr2 : numeric 
#Class of ref id in Dr2 bind_wands_dr2 : numeric 


# As some of Swindon's referral IDs contain letters, convert the rest to charcter. 
# Lanc
is.na(bind_lancs_dr2$`Referral ID (or Case ID)`) %>% sum ()   #0
bind_lancs_dr2$`Referral ID (or Case ID)` <- as.character(bind_lancs_dr2$`Referral ID (or Case ID)`)
# Tel
is.na(bind_telford_dr2$`Referral ID (or Case ID)`) %>% sum ()   #0
bind_telford_dr2$`Referral ID (or Case ID)` <- as.character(bind_telford_dr2$`Referral ID (or Case ID)`)
# Walsall
is.na(bind_walsall_dr2$`Referral ID (or Case ID)`) %>% sum ()   #0
bind_walsall_dr2$`Referral ID (or Case ID)` <- as.character(bind_walsall_dr2$`Referral ID (or Case ID)`)
# Wandsworth
is.na(bind_wands_dr2$`Referral ID (or Case ID)`) %>% sum ()   #0
bind_wands_dr2$`Referral ID (or Case ID)` <- as.character(bind_wands_dr2$`Referral ID (or Case ID)`)

# UASC ----
# Loop over the dataframe names
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  uasc_class <- class(df$`Unaccompanied Asylum Seeker`)
  
  # Print the result
  cat("Class of uasc in Dr2", df_name, ":", uasc_class, "\n")
}

#Class of uasc in Dr2 bind_lancs_dr2 : character 
#Class of uasc in Dr2 bind_swind_dr2 : character 
#Class of uasc in Dr2 bind_telford_dr2 : character 
#Class of uasc in Dr2 bind_walsall_dr2 : numeric 
#Class of uasc in Dr2 bind_wands_dr2 : character

# AWAITING REPLY FROM TELFORD on coding of UASC

is.na (bind_walsall_dr2$`Unaccompanied Asylum Seeker`) %>% sum () # 0

bind_walsall_dr2 %>%
  select(`Unaccompanied Asylum Seeker`) %>%
  table(useNA = "ifany") %>%
  addmargins()

#    0      1    Sum 
#  11283   20   11303 

bind_walsall_dr2$`Unaccompanied Asylum Seeker` <- as.character(bind_walsall_dr2$`Unaccompanied Asylum Seeker`)

################################################################################

# TEMP COMBINED DATAFRAME ----
# Creating a temporary dataframe which excludes all the factors identified at end of assessment (other then the trio variable).
# This is temporary until the factors identified at end of assessment (FIA) variable is organised.

# Creating temp datasets with columns for FIA deleted
# Lancs
temp_lancs_dr2 <- bind_lancs_dr2[, -c(7:12)]
# Swindon
temp_swind_dr2 <- bind_swind_dr2[, -c(7,16:28)]
# Telford 
temp_telford_dr2 <- bind_telford_dr2[, -c(7,16:31)]
# Walsall 
temp_walsall_dr2 <- bind_walsall_dr2[, -c(7,16:28)]
# Wandsworth 
temp_wands_dr2 <- bind_wands_dr2[, -c(7,16:33)]

all_dr2_bind <- bind_rows(temp_lancs_dr2, temp_swind_dr2, temp_telford_dr2, temp_walsall_dr2, temp_wands_dr2)

##########################################################
# To do next = group the factors identified at the end of assessment into the protocol groupings. Then delete the breakdown columns. 
# Allowing you to bind the whole of DR2 into one (across LAs). Then merge on to DR3.

# These come from Appendix A: definitions and guidance for primary need codes (see module 3) 
# These are not the same as factors identified at end of assessment 

#The main need for which child started to receive services for this referral (if applicable), as defined in the CIN census 
#included as a categorical variable: 
#0 = Not stated, 
#1 = Abuse or neglect, 
#2 = Child's disability/illness, 
#3 = Parental Disability/illness, 
# 4 = Family in acute stress, 
#5 = Family dysfunction, 
#6 = Socially unacceptable, 
#7 = Low income, 
#8 = Absent parenting, 
#9 = Cases other than Children in Need.

# Shortening variable names to make further cleaning easier-----
colnames(all_dr2_bind)

# Keep colname 1 the same as 'Child ID' in the DR3 files. Ease for merging. 

colnames(all_dr2_bind)[1]  <- "child la id"

colnames(all_dr2_bind)[2]  <- "child id"

colnames(all_dr2_bind)[3]  <- "case id"

colnames(all_dr2_bind)[4]  <- "ref date"

colnames(all_dr2_bind)[5]  <- "no further action"

colnames(all_dr2_bind)[6]  <- "assess start date"

colnames(all_dr2_bind)[7]  <- "outcome of sa"

colnames(all_dr2_bind)[8]  <- "previous cpp"

colnames(all_dr2_bind)[9]  <- "gender"

colnames(all_dr2_bind)[10]  <- "dob"

colnames(all_dr2_bind)[11]  <- "age at ref"

colnames(all_dr2_bind)[12]  <- "ethnicity"

colnames(all_dr2_bind)[13]  <- "disability"

colnames(all_dr2_bind)[14] <- "uasc"

colnames(all_dr2_bind)[15] <- "ref trio"

colnames(all_dr2_bind)[16] <- "concat id"

colnames(all_dr2_bind)[17] <- "LA"

# Look at how Previous CPP and outcome of SA coded, as appears to be high missingness ----
# Previous CPP 
all_dr2_bind %>%
  select(`previous cpp`) %>%
  table(useNA = "ifany") %>%
  addmargins()

# 0        1     2     3     4     5    <NA>     Sum 
# 24714  7240  1236   287    28     2   23618   57125 

# EXPLORING PATTERENED MISSINGNESS
# Visual to see missingness by LA
all_dr2_bind %>% 
  gg_miss_var(show_pct = TRUE, facet = `LA`)
ggsave("Output/DR2_missing.png", width = 8, height = 6, units = "in")

# very high missingness in lancashire explore whether 0 was marked as NA. 
bind_lancs_dr2 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()

# high missingness in Swindon 
bind_swind_dr2 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()

#Number of previous child protection plans
#0       1     2     3  <NA>   Sum 
#7,866   558    71    11  2,386 10892
# It appears that some DR2 returns could have entered prev. CPP as NA rather than 0

swind_dr2_nov20 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#0     1    2   <NA>     Sum 
#138   29    6   2386   2559 

swind_dr2_nov21 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#0       1    2    3     Sum 
#3626  411    43    6    4086 

swind_dr2_nov22 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#   0    1    2    3  Sum 
# 3090  107   15    4 3216 

swind_dr2_apr23 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#0    1    2    3  Sum 
#1012   11    7    1 1031 

# high missingness in Telford 
bind_telford_dr2 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()
# Number of previous child protection plans
# 0       1    2    3    4   <NA>  Sum 
# 2762 1254   47   12    3   1527 5605 

telford_dr2_nov20  %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#0    1    2    3    4  Sum 
#1241  219   47   12    3 1522 

telford_dr2_nov21 %>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#0      1    Sum 
#1521  320   1841 

telford_dr2_nov22%>%
  select(`Number of previous child protection plans`) %>%
  table(useNA = "ifany") %>%
  addmargins()
#1 <NA>  Sum 
#669 1440 2109

# Recode NA as 0 

# Recode number of previous child protection plans into 
all_dr2_bind$`previous cpp` <- dplyr::case_when(
  all_dr2_bind$`previous cpp` == 0 ~ '0',
  all_dr2_bind$`previous cpp` == 1 ~ '1',
  all_dr2_bind$`previous cpp` == 2 ~ '2',
  all_dr2_bind$`previous cpp` > 2 ~ '3+'
)

all_dr2_bind$`previous cpp` <- factor(
  all_dr2_bind$`previous cpp`,
  levels = c('0', '1', '2', '3+')
)

colnames(all_dr2_bind)

# Recoding variables into factors and setting the factor levels 
all_dr2_bind <- all_dr2_bind %>%
  mutate(
    
    gender = relevel(
      factor(gender), ref = 'Male'),
    
    ethnicity = relevel(
      factor(ethnicity), ref = 'WBRI'),
    
    disability = relevel(
      factor(disability), ref = '0'),
    
    #To add UASC once Teflord respond.
    #unaccompanied_asylum_seeker = relevel(
    #  factor(unaccompanied_asylum_seeker), ref = 'Not UASC'),
    
    #`no further action` = relevel(
    #  factor(`no further action`), ref = 'Further action'),
    
    `previous cpp` = relevel(
      factor(`previous cpp`), ref = '0')
  )


colnames(bind_lancs_dr2)

#################################################


# SAVE All DR2 file to outputs ----
save(all_dr2_bind, file = "Output/DR2_bind.RData")

# SUBSET: ----
#0-12 at the time of referral 
#who have been referred within the trial period
#and whose initial assessment identified parental substance misuse, domestic violence, or parental mental health as factors identified at the end of assessment. 
#Since these factors are only identified at assessment, our sample is restricted to children whose referral has progressed to an assessment 
#and where one of the factors identified at assessment includes one of the three factors defined above.


##########################################################


