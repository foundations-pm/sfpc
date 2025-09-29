
# Cleaning the family safeguarding dataset for SFPC
###################################################

# Install and load tidyverse package ---------------------------

# install packages if not already installed

library(tidyverse)
library (dplyr)
library(readxl)
library(tibble)
library(lubridate)
library(data.table)

# Clearing R -------------------------
rm(list = ls())

# READING IN THE DR1 FILES ---------------------------------------

# Failed attempts at reading in all files ======================

# Failed attempt 1 
# files <- c("Data/FS_DR1/lancashire_dr1_apr21.xlsx", "Data/FS_DR1/lancashire_dr1_apr22.xlsx",
        #   "Data/FS_DR1/lancashire_dr1_nov20.xlsx", "Data/FS_DR1/lancashire_dr1_nov21.xlsx",
        #   "Data/FS_DR1/lancashire_dr1_nov22.xlsx")
# names <- c('lanc_dr1_apr21','lanc_dr1_apr22,lanc_dr1_nov20','lanc_dr1_nov21','lanc_dr1_nov22')

# result <- list()
# for (i in seq_along(files)) {
#  result[names[i]] <- read_excel(path = files[i])
# }

### Successful bit of code to read in all the files ============
# Successful code to read in the DR1 Excel files
dr1_files <- list.files(path = "Data/FS_DR1",
                        pattern = "*.xlsx", 
                        full.names = TRUE)

# Reading in the files
dr1_all <- sapply(dr1_files, 
                  read_excel, sheet = "DR1 - Data - aggregate level",
                  col_names = TRUE,
                  trim_ws = TRUE,
                  skip = 1)

#Viewing to see if the dataframe looks correct. 
View(dr1_all)
class(dr1_all)

# Attempting to merge the dataframes from the list ===============
dr1_df <- rbindlist(dr1_all)
# Error message:  Item 3 has 14 columns, inconsistent with item 1 which has 11 columns. To fill missing columns use fill=TRUE.
dr1_df <- rbindlist(dr1_all,
                    fill=TRUE)
# Class attribute on column 1 of item 13 does not match with column 1 of item 8.

dr1_all <- do.call(rbind, dr1_all)
# Error in match.names(clabs, names(xi)) : 
#  names do not match previous names

# Attempt to recrify by standardising col names
# Ensure column names are consistent across data frames in dr1_all
consistent_colnames <- unique(unlist(lapply(dr1_all, colnames)))

# Update column names of individual data frames in dr1_all
dr1_all <- lapply(dr1_all, function(df) {
  colnames(df) <- consistent_colnames
  return(df)
})

# Bind data frames together
dr1_all <- do.call(rbind, dr1_all)

#Attempting to bind using Dyplr bind_rows, as col names do not have to be the same
dr1_all <- do.call(bind_rows,dr1_all)
#Error:
#! Can't combine `..1$43739` <character> and `..13$43739` <datetime<UTC>>.
#Run `rlang::last_trace()` to see where the error occurred.

merge(dr1_all, all=true)
# Error in (function (..., row.names = NULL, check.rows = FALSE, check.names = TRUE,  : 
                    #  arguments imply differing number of rows: 19, 5, 49, 6, 12, 11, 13


# Reading in files with standardised columns-----------------------------
# Define the common columns
common_cols <- c("Month",                                                                                                                                                       
                 "Number of assessments completed (by CSC)",                                                                                                                    
                 "Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.",
                 "CLA rate per 10,000 children",                                                                                                                                
                 "Number of children looked after at the end of the  month in the LA",                                                                                          
                 "Number of children who newly became looked after this month in the LA",                                                                                       
                 "Number of CIN plans that started this month in the LA",                                                                                                       
                 "Number of open CIN cases this month in the LA",                                                                                                               
                 "Number of CPPs that started this month in the LA",                                                                                                            
                 "Number of open CPPs this month in the LA",                                                                                                                    
                 "Number of new referrals this month in the LA") 

# Create an empty list to store data frames
data_list <- list()

# Iterate through each file
for (file in list.files(path = "Data/FS_DR1", 
                        pattern = "*.xlsx", 
                        full.names = TRUE,
                        trim_ws = TRUE)) {
  # Read the file into a data frame
  df <- read_excel(file, sheet = "DR1 - Data - aggregate level",)
  
  # Check if the data frame has the common columns
  missing_cols <- setdiff(common_cols, colnames(df))
  
  # Add missing columns with default values (NA)
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      df[[col]] <- NA
    }
  }
  
  # Append the modified data frame to the list
  data_list <- c(data_list, list(df))
}

# Converting the MONTH variable into the correct class for all 
for (i in seq_along(data_list)) {
  data_list[[i]]$Month <- as.POSIXct(data_list[[i]]$Month, format = "%d-%m-%Y")
}

# Changing all the other variables into numeric 
for (i in seq_along(data_list)) {
  # Exclude the "Month" column from conversion
  non_month_columns <- setdiff(names(data_list[[i]]), "Month")
  
  # Convert non-month columns to numeric
  data_list[[i]][non_month_columns] <- lapply(data_list[[i]][non_month_columns], as.numeric)
}

# Combine data frames into a single data frame
combined_df <- do.call(bind_rows, data_list)

# THIS ATTEMPT FINISHED 

#Creating a variable to show LA, to find something to merge on
LA = c("Blank")
dr1_all$LA <- "Blank"

#Attemps to merge 
merged_DR1 <- bind_rows(dr1_all, .id = "Blank", all=T)
# Error in `vctrs::data_frame()`:
 # ! Can't recycle `Data/FS_DR1/lancashire_dr1_apr21.xlsx` (size 19) to match `Data/FS_DR1/lancashire_dr1_apr22.xlsx` (size 5).
# Run `rlang::last_trace()` to see where the error occurred.

# Merging the data frames into LAs ---------------------
list_lancashire_DR1 <- (path="Data/FS_DR1", pattern = "lancashire")
# For the rbind command, files need the same column names and column types. 'bind_rows' does not need uniformality. 

lancashire_DR1 <- bind_rows(list_lancashire_DR1)

View(lancashire_DR1)

##  Alternative code where things are read in individually - more time consuming --------------------

# Read in LANCASHIRE INDIVIDUALLY  ===================================
interim_lancs_dr1_nov20 <- read_excel("Data/FS_DR1/lancashire_dr1_nov20.xlsx", 
                                      sheet = "DR1 - Data - aggregate level", 
                                      range = "A2:N20")
# Nov 20 dataset has three additional columns 
colnames(interim_lancs_dr1_nov20)
# Check column names for another submission. Others do not have cols 3,4,6
lancs_dr1_ap21 <- read_excel("Data/FS_DR1/lancashire_dr1_apr21.xlsx", 
                             sheet = "DR1 - Data - aggregate level", 
                             range = "A1:K7")

colnames(lancs_dr1_ap21)
# Subset the df so that it matches the other DR1 entries
lancs_dr1_nov20 <- interim_lancs_dr1_nov20[-c(3,4,6)]
# Checking to see if these worked. It did. 
colnames(lancs_dr1_nov20)
# Remove interim df
rm(interim_lancs_dr1_nov20)

# Reading in the rest of the data for Lancashire
lancs_dr1_ap21 <- read_excel("Data/FS_DR1/lancashire_dr1_apr21.xlsx", sheet = "DR1 - Data - aggregate level", 
                             range = "A1:K7")
lancs_dr1_nov21 <- read_excel("Data/FS_DR1/lancashire_dr1_nov21.xlsx", sheet = "DR1 - Data - aggregate level")
lancs_dr1_apr22 <- read_excel("Data/FS_DR1/lancashire_dr1_apr22.xlsx", sheet = "DR1 - Data - aggregate level")
lancs_dr1_nov22 <- read_excel("Data/FS_DR1/lancashire_dr1_nov22.xlsx", sheet = "DR1 - Data - aggregate level")

colnames(lancs_dr1_ap21)
# Merging LANCASHIRE ----------------------------
# To join two data frames vertically, use: total <- rbind(data frameA, data frameB)
all_lancs_dr1 <- bind_rows(lancs_dr1_nov20, lancs_dr1_ap21, lancs_dr1_nov21, lancs_dr1_apr22, lancs_dr1_nov22)

# Adding a variable/column to show this dataframe comes from Lancashire. This will need to be clear in the full merge across LAs. 
LA = c("Lancashire")
all_lancs_dr1$LA <- "Lancashire"

# Removing all the singular data frames
# Get the names of the data frames starting with 'lancs'
lancs_data_frames <- ls(pattern = "^lancs")

# Remove the data frames
rm(list = lancs_data_frames)

# Reading in SWINDON INDIVIDUALLY ---------------------------
interim_swind_dr1_nov20 <- read_excel("Data/FS_DR1/swindon_dr1_nov20.xlsx", 
                              sheet = "DR1 - Data - aggregate level",
                              col_names = TRUE,
                              trim_ws = TRUE)
swind_dr1_ap21 <- read_excel("Data/FS_DR1/swindon_dr1_apr21.xlsx", 
                             sheet = "DR1 - Data - aggregate level",
                             col_names = TRUE,
                             trim_ws = TRUE)
swind_dr1_nov21 <- read_excel("Data/FS_DR1/swindon_dr1_nov21.xlsx", 
                              sheet = "DR1 - Data - aggregate level",
                              col_names = TRUE,
                              trim_ws = TRUE)
swind_dr1_apr22 <- read_excel("Data/FS_DR1/swindon_dr1_apr22.xlsx", 
                              sheet = "DR1 - Data - aggregate level",
                              trim_ws = TRUE)
swind_dr1_nov22 <- read_excel("Data/FS_DR1/swindon_dr1_nov22.xlsx", 
                              sheet = "DR1 - Data - aggregate level",
                              col_names = TRUE,
                              trim_ws = TRUE)

# Looking at the composition of the files
colnames(interim_swind_dr1_nov20)
# Changing nov 20
swind_dr1_nov20 <- interim_swind_dr1_nov20[-c(3,4,5,6,7,9,14)]
colnames(swind_dr1_nov20)
#drop interim 
rm(interim_swind_dr1_nov20)

# Reconfiguring DATE AND TIME for Swindon =================
# Unable to bind the dataframes as not all the month columns are configured as date time. 
# Create a vector of dataframe names
dataframe_names <- c("swind_dr1_ap21", "swind_dr1_apr22", "swind_dr1_nov21", "swind_dr1_nov22", "swind_dr1_nov20")

# Loop over the dataframe names
for (df_name in dataframe_names) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Check the class of the Month variable
  month_class <- class(df$Month)
  
  # Print the result
  cat("Class of Month variable in", df_name, ":", month_class, "\n")
}
summary(swind_dr1_nov20)
  #month was uploaded as character 'Decemer 2019'
swind_dr1_nov20 <- swind_dr1_nov20 %>% 
  mutate(Month = na_if(Month, "Decemer 2019"))
  #replace month with na
swind_dr1_nov20 <- swind_dr1_nov20 %>% 
  mutate(Month = as.numeric(Month))
  #convert strings to numeric
swind_dr1_nov20 <- swind_dr1_nov20%>% 
  mutate(Month = as.Date(Month, origin = "1899-12-30")) #excel values, need to convert to date
  #convert numeric to dates

#fill in december date 

# CODE LOOKING AT TIME, cut from clean version --------


class(swind_dr1_nov20_new$Month)
swind_dr1_nov20
  
# Converting the variable individually 
swind_dr1_nov20$Month <- as.POSIXct(swind_dr1_nov20$Month, format = "%d-%m-%Y")

#Putting them all in the same formatt (date)

dataframe_date <- c("swind_dr1_nov20", "swind_dr1_ap21", "swind_dr1_apr22", "swind_dr1_nov21", "swind_dr1_nov22")

# Loop over the dataframe names=====================
for (df_name in dataframe_date) {
  # Get the dataframe object using its name
  df <- get(df_name)
  
  # Convert the Month variable to date using dmy()
  df$Month <- dmy(df$Month, format = "%b-%Y")
  
  # Assign the modified dataframe back to the global environment
  assign(df_name, df)
}

# Binding the swindon files together 
all_swind_dr1 <- bind_rows (swind_dr1_nov20, swind_dr1_ap21, swind_dr1_apr22, swind_dr1_nov21, swind_dr1_nov22)

#comparing why nov 2020 has more variables 
names(swind_dr1_nov20)
names (swind_dr1_nov21)

# merging to compare columns # didn't work
swind_dr1_merged <- merge(swind_dr1_nov20, 
                          swind_dr1_nov21, 
                          swind_dr1_nov22, 
                          swind_dr1_ap21, 
                          by = "Month", all = TRUE)

# Trying to find a way to read them in more efficiently. ----------------
swindon_files_dr1 <- list.files(path = "Data/FS_DR1", pattern = "swindon", 
                                full.names = TRUE)
list_swindon_DR1 <- lapply(swindon_files_dr1, read_excel, 
                           sheet = "DR1 - Data - aggregate level",
                           col_names = TRUE,
                           trim_ws = TRUE)
                        

all_swind_dr1 <- bind_rows(list_swindon_DR1)


# Repeating the process for the other LAs: 

# Move LA variable so that it's the first column. So far not working, come back to 
# all_lancs_dr1 %%
#  relocate(LA) %%
#  head()

# TO DO: Impute the free school meal data to fill in the NAs with the previous value. 

#
## TO DO: there is lots of missingness in column 3, free school meals. 
#The NAs should be replaced with the number at the top of the column as they only receive this data once every six months. 
#Impute.
# Look at the dates, how are they formatted. Make sure they are all the same.


# Define the common columns
common_cols <- c("Month",                                                                                                                                                       
                 "Number of assessments completed (by CSC)",                                                                                                                    
                 "Proportion of children / young people eligible and claiming for Free School Meals for all school-age children and young people in the LA, out of all pupils.",
                 "CLA rate per 10,000 children",                                                                                                                                
                 "Number of children looked after at the end of the  month in the LA",                                                                                          
                 "Number of children who newly became looked after this month in the LA",                                                                                       
                 "Number of CIN plans that started this month in the LA",                                                                                                       
                 "Number of open CIN cases this month in the LA",                                                                                                               
                 "Number of CPPs that started this month in the LA",                                                                                                            
                 "Number of open CPPs this month in the LA",                                                                                                                    
                 "Number of new referrals this month in the LA") 

# Create an empty list to store data frames
data_list <- list()

# Iterate through each file
for (file in list.files(path = "Data/FS_DR1", 
                        pattern = "*.xlsx", 
                        colnames = TRUE,
                        trim_ws = TRUE)) {
  # Read the file into a data frame
  df <- read_excel(file, sheet = "DR1 - Data - aggregate level",)
  
  # Check if the data frame has the common columns
  missing_cols <- setdiff(common_cols, colnames(df))
  
  # Add missing columns with default values (NA)
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      df[[col]] <- NA
    }
  }
  
  # Append the modified data frame to the list
  data_list <- c(data_list, list(df))
}

