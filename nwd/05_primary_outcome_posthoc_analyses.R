# Primary outcome analysis for No Wrong Doors RCT DR1 ----

# Set up  ----

#Paths
user_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(user_directory,'Foundations/High-SFPC-Impact - ')

# Data and output paths
data_path = paste0(sharepoint_path, 'QA/processing/linked_data/')

output_path = paste0(sharepoint_path,
                     'QA/outputs/model_outputs/sensitivity_analyses/')

# Dates
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

# Set up folders to save output findings 
# in a neat an organised manner 
# Save individual files in a new directory
# Named after the month when the analyses were conducted
main_dir = output_path

sub_dir = dir_date

if(!dir.exists(file.path(paste0(main_dir, sub_dir)))){
  
  dir.create(file.path(main_dir, sub_dir)) # create new dir
  paste0("Creating new directory: '", sub_dir,"'")# confirm new dir is created
  
} else { 
  
  cat(
    crayon::green(
      crayon::bold(
        paste0("Directory '", sub_dir, "' already exists."))))
  
} # confirms dir already exists

# Working directory
wd = paste0(user_directory, "Documents/sfpc/nwd/")

# Libraries 
{ source(paste0(wd, "config.R")) }

# Functions
{ source(paste0(wd, "functions.R"))}

# Load data --------------------------------------------------------------------

# 1 load data 
data <- readRDS(file = paste0(
  output_path, 'primary_analysis_analytical_dataset_V2.Rds'))

# Posthoc analyses (PH) ---------------------------------------------------------

## PH1: RTM effects ------------------------------------------------------------

### Data --------------------------------------------------------
### Formula --------------------------------------------------------
### Fit model --------------------------------------------------------
### Save outputs --------------------------------------------------------


## PH2: Seasonal effects -------------------------------------------------------

### PH2.1: Demand on CSC -------------------------------------------------------

#### Data --------------------------------------------------------
#### Formula --------------------------------------------------------
#### Fit model --------------------------------------------------------
#### Save outputs --------------------------------------------------------


### PH2.2: COVID-19 effects ----------------------------------------------------

#### Data --------------------------------------------------------
#### Formula --------------------------------------------------------
#### Fit model --------------------------------------------------------
#### Save outputs --------------------------------------------------------

