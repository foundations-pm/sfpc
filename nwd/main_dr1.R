# Data Cleaning Script for No Wrong Doors RCT ----

# Paths  ----
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'Data/')
output_path = paste0(sharepoint_path, 'QA/')

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries ----
{ source(paste0(wd, "config.R")) }

# Functions ----
{ source(paste0(wd, "functions.R"))}

# Load data ----

# Set working dir to data path
setwd(data_path)

# Load data:

# Create list of file names
dr1_names <- list.files("NWD_DR1", # Name of folder with DR1 data
                        pattern="*.xlsx", full.names=TRUE)
# Remove incomplete files
dr1_names = dr1_names[-16] # Rochdale Nov 2020

# Read files into local env
dr1_list <- lapply(dr1_names, function(file) {  
  print(file) 
  read_excel(file, sheet = "DR1 - Data - aggregate level")})

# Assign names to each file
names(dr1_list) = dr1_names

# Quality checks ----
# Key steps:
# 1- Datasets dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

lapply(dr1_list, function(df){ 
  print(names(df))
  print(dim(df))})

colnames(dr1_list[['NWD_DR1/warrington_dr1_nov20.xlsx']])
colnames(dr1_list[['NWD_DR1/warrington_dr1_nov22.xlsx']])
colnames(dr1_list[['NWD_DR1/leicester_dr1_nov20.xlsx']])

# Data cleaning ----

# Workplan:

# 1- Keep only mandatory return & assing new names

# Nb completed assessment by CSC = assessment_completed_agg (int)
# CLA rate per 10,000 children = cla_agg_rate_per_10000 (int)
# Nb of children looked after at the end of the month in LA = cla_agg (int)
# Nb of newly looked after children at the end of month = cla_start_agg (int)
# Number of CIN plans started = cin_start_agg (int)
# Number of open CIN cases = cin_agg (int)
# Number of CPP plans started = cpp_start_agg (int)
# Number of CPP plans = cpp_agg (int)
# Number of new referrals = new_referrals_agg (int)

# 2- Create a column to identify each dataset by LA and month of return

# 3- Merge all records 

# 4- Data cleaning checks
# Total number missing 
# Location missing data 
# Value inconsistencies (class & range/categories)





