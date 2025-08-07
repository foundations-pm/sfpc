# Sensitivity analysis for No Wrong Doors RCT: primary outcome ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths

# where the primary outcome dataset is
data_path = paste0(sharepoint_path, 'QA/outputs/') 

# where to save final output list
output_path = paste0(
  sharepoint_path, 'QA/outputs/model_outputs/',
  'primary_analyses/sensitivity_analyses/')

# where to save individual model/output files 
working_folder = paste0(output_path, 'working_folder/')

# where to save individual sensitivity checks / files
#sensitiviy_checks_folder = paste0(output_path, 'sensitivity_analyses/')

# Dates
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folders to save output findings 
# in a neat an organised manner 
# Save individual files in a new directory
# Named after the month when the analyses were conducted

if(!dir.exists(file.path(paste0(working_folder, dir_date)))){
  
  dir.create(file.path(working_folder, dir_date)) # create new dir
  paste0("Creating new directory: '", dir_date,"'")# confirm new dir is created
  
} else { 
  
  cat(
    crayon::green(
      crayon::bold(
        paste0("Directory '", dir_date, "' already exists."))))
  
} # confirms dir already exists

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions
{ source(paste0(wd, "functions.R"))}

# Load data ----------------------------------------------------------------------------

# 1 load data 
data <- readRDS(file = paste0(
  data_path, 'primary_analysis_analytical_dataset_V2.Rds'))

# Note on output storage ---------------------------------------------------------------

# Individual model files to be saved in Month-Year (%B %Y format) folder 
# Output files to be created to store all findings from sensitivity analyses in folder sensitivity_analyses

setwd(output_path)


##S3: Change in treatment assignment -----------------------------------------

### Data --------------------------------------------------------
### Formula --------------------------------------------------------
### Fit model --------------------------------------------------------
### Save outputs --------------------------------------------------------

##S4: Widening of age gap  -----------------------------------------

### Data --------------------------------------------------------
### Formula --------------------------------------------------------
### Fit model --------------------------------------------------------
### Save outputs --------------------------------------------------------

##S5: Time/Treatment interaction effects -----------------------------------------

### Data --------------------------------------------------------
### Formula --------------------------------------------------------
### Fit model --------------------------------------------------------
### Save outputs --------------------------------------------------------

