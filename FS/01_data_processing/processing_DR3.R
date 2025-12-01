#------------------------------------------------#
#
# FAMILY SAFEGUARDING: STEPPED WEDGE ANALYSES ----
#
#------------------------------------------------#


## DATA CLEANING: DATA RETURN 3 ----

## 1. Set-up  ----
r_directory = 'C:/Users/PerrineMachuel/'
sharepoint_path = paste0(r_directory,'Foundations/High-SFPC-Impact - Family Safeguarding')

# Data and output paths
data_path = paste0(sharepoint_path, '/Data')
output_path = paste0(sharepoint_path, '/Datasets/pre_processing/DR3')

# Working directory
wd = paste0(r_directory, "Documents/SFPC/FS/")

# Libraries 
{ source(paste0(r_directory, "Documents/SFPC/FS/config.R")) }

# Functions 
{ source(paste0(r_directory, "Documents/SFPC/FS/functions.R"))}

# Dates 
date = format(Sys.Date(),"%Y/%m/%d") # date format to save within dataframes

file_date = format(Sys.Date(),"_%Y%b%d") # date format to save files

dir_date = format(Sys.Date(),"%B %Y") # date format to create directories

## 2. Pre-processing ----

### 1. Load data ----

# Set working dir to data path
setwd(data_path)

# Load data:
# Create list of file paths
DR3_file_paths <- list.files("FS_DR3_2024", 
                             pattern="*.xlsx",
                             full.names=TRUE)

# List of all DR2 files
DR3_list = lapply(DR3_file_paths, function(.file_paths){
  
  read_xlsx_worksheets(.file_paths,
                       sheet_name = 'Data') })

# Assign names to list
# Clean list names
names(DR3_list) = DR3_file_paths

string_to_remove = c("FS_DR3_2024/", ".xlsx")

names(DR3_list) <- stringr::str_remove_all(
  names(DR3_list),  
  paste(string_to_remove, collapse = "|"))

# Flatten list - remove 1 nesting level
DR3_list <- lapply(DR3_list, `[[`, 1)

### 2. Inspect data ----
# Key steps:
# 1- Data set dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

for(name in names(DR3_list)){
  
  print(name)
  # Dimension of data
  print(dim(DR3_list[[name]])) 
  
  # Print head = 5 to assess where table starts
  print(head(DR3_list[[name]], 5)) }

### 3. Resize data ----

# All tables start at row 4
# Resize all tables starting from row 4
DR3_list <- lapply(DR3_list, function(data) {
  
  janitor::clean_names( # clean col names to standard names
    janitor::row_to_names( # make row 1 the columns of each dataset in list
      data[4:nrow(data),], 1)) # resize dataframe to start from row 4
  
})

# Checks
lapply(DR3_list, function(data) colnames(data))
lapply(DR3_list, function(data) dim(data))

### 4. Add LA ID ---- 

# Locate month return and LA
for(data in names(DR3_list)){ 
  
  local_authority = str_extract(names(DR3_list[data]), ".+?(?=_)")
  month_return = gsub(".*\\_", "", names(DR3_list[data]))
  
  print(local_authority)
  print(month_return)
  
  # Assign LA name and month return
  DR3_list[[data]] = DR3_list[[data]] %>% 
    dplyr::mutate(
      month_return = month_return,
      local_authority = local_authority) %>%
    relocate(local_authority, month_return) }

### 5. Unify data classes ----

# check class for all columns 
lapply(DR3_list, \(df) sapply(df, class))
lapply(DR3_list, head)

#### 1. As date ----
# Turn date columns into dates
DR3_list_transformed = lapply(DR3_list, function(data) {
  
  data %>% 
    dplyr::mutate(
      start_date_of_cla_period_of_care_start_date = 
        janitor::excel_numeric_to_date(
          as.numeric(start_date_of_cla_period_of_care_start_date))
      )
}) 

lapply(DR3_list_transformed, head)

#### 2. QA ----
# Compare missingness pre and post cleaning
# Count number of NAs per column 
# Make table 
dr3_pre_cleaning_missingness = purrr::map_dfr(
  DR3_list, function(data_return) {
    dplyr::tibble(
      column = names(data_return),
      n_missing = colSums(is.na(data_return))
    )
  }, .id = 'data_return')

dr3_post_cleaning_missingness = purrr::map_dfr(
  DR3_list_transformed, function(data_return) {
    dplyr::tibble(
      column = names(data_return),
      n_missing = colSums(is.na(data_return))
    )
  }, .id = 'data_return')

# save tables
tables = c('dr3_pre_cleaning_missingness', 'dr3_post_cleaning_missingness')

mget(tables) %>%                     # fetch objects safely by name
  iwalk(~ write_xlsx(
    .x,
    file.path(output_path, "QA", paste0(.y, file_date, ".xlsx"))
  ))

### 6. Bind rows ----
DR3_data = do.call(bind_rows, DR3_list_transformed)

# Change wands name to align with DR3
DR3_data = DR3_data %>%
  dplyr::mutate(local_authority = case_when(
    local_authority == 'wands' ~ 'wandsworth',
    TRUE ~ local_authority))

### 7. Save data ---- 

# append dr3 prefix to identify columns coming from dr3
colnames(DR3_data) <- paste0("dr3_", colnames(DR3_data))

saveRDS(DR3_data, file = paste0(
  output_path,"/DR3_pre_processed_data.Rds")) 
