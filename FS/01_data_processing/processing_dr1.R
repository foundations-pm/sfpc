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
output_path = paste0(sharepoint_path, '/Datasets/pre_processing/DR1')

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
DR1_file_paths <- list.files("FS_DR1", 
                             pattern="*.xlsx",
                             full.names=TRUE)

# List of all DR2 files
DR1_list = lapply(DR1_file_paths, function(.file_paths){
  
  read_xlsx_worksheets(.file_paths,
                       sheet_name = 'DR1 - Data - aggregate level') })

DR1_list = lapply(DR1_list, `[[`, 1)

# Assign names to list
# Clean list names
names(DR1_list) = DR1_file_paths

string_to_remove = c("FS_DR1/", ".xlsx")

names(DR1_list) <- stringr::str_remove_all(
  names(DR1_list),  
  paste(string_to_remove, collapse = "|"))

### 2. Inspect data ----
# Key steps:
# 1- Data set dimensions
# 2- Column names 
# 3- Missing data 
# 4- Unique values / unexpected values (e.g., int vs cat)

for(name in names(DR1_list)){
  
  print(name)
  # Dimension of data
  print(dim(DR1_list[[name]])) 
  
  # Print head = 5 to assess where table starts
  print(head(DR1_list[[name]], 5)) }

lapply(DR1_list, \(df) { print(colnames(df)) })

### 3. Resize data ----

# Only lancashire Nov 2020 needs resizing
# Row 1 to name
DR1_list[['lancashire_dr1_nov20']] <- janitor::row_to_names(
  DR1_list[['lancashire_dr1_nov20']], 1) 

# Assign clean names
DR1_list = lapply(DR1_list, \(data) {
  
  janitor::clean_names(data) # clean col names to standard names
  
})

# Checks
lapply(DR1_list, \(data) colnames(data))
lapply(DR1_list, \(data) dim(data))

### 4. Add LA ID ---- 

# Locate month return and LA
for(data in names(DR1_list)){ 
  
  local_authority = str_extract(names(DR1_list[data]), ".+?(?=_)")
  month_return = gsub(".*\\_", "", names(DR1_list[data]))
  
  print(local_authority)
  print(month_return)
  
  # Assign LA name and month return
  DR1_list[[data]] = DR1_list[[data]] %>% 
    dplyr::mutate(
      month_return = month_return,
      local_authority = local_authority) %>%
    relocate(local_authority, month_return) }

### 5. Unify data classes ----

# check class for all columns 
column_classes <- purrr::map_dfr(
  DR1_list,
  ~ tibble(
    column = names(.x),
    class  = map_chr(.x, ~ paste(class(.x), collapse = " / "))  
  ),
  .id = "data return"
)

#### 1. As date ----
# Turn date columns into dates
DR1_list_transformed = lapply(DR1_list, \(data) {
  
  data %>% 
    dplyr::mutate(
      month = case_when(
        grepl('-', month) ~ as.Date(month, "%Y-%m-%d"), 
        TRUE ~ janitor::excel_numeric_to_date(as.numeric(month))
      )
    )
}) 

#### 2. As numeric ----
# Turn all necessary columns into numeric vectors 
# Checks were performed to see if there was any other transformation to perform
# E.g., characters to remove 
DR1_list_transformed = lapply(DR1_list_transformed, \(data) {
  
  numeric_columns = data %>% dplyr::select( 
    -local_authority, -month_return, -month) %>%
    names()
  
  data %>% 
    dplyr::mutate(
      across(.cols = all_of(numeric_columns),
             .fns = ~ as.numeric(.x)))
}) 

#### 2. QA ----
# Compare missingness pre and post cleaning
# Count number of NAs per column 
# Make table 
dr1_pre_cleaning_missingness = purrr::map_dfr(
  DR1_list, function(data_return) {
    dplyr::tibble(
      column = names(data_return),
      n_missing = colSums(is.na(data_return))
    )
  }, .id = 'data_return')

dr1_post_cleaning_missingness = purrr::map_dfr(
  DR1_list_transformed, function(data_return) {
    dplyr::tibble(
      column = names(data_return),
      n_missing = colSums(is.na(data_return))
    )
  }, .id = 'data_return')

# save tables
tables = c('dr1_pre_cleaning_missingness', 'dr1_post_cleaning_missingness')

mget(tables) %>% # fetch objects safely by name
  purrr::iwalk(~ write_xlsx(
    .x,
    file.path(output_path, "QA", paste0(.y, file_date, ".xlsx"))
  ))

### 6. Bind rows ----
DR1_data = do.call(bind_rows, DR1_list_transformed)

na_count_table <- purrr::map_dfr(
  colnames(DR1_data),
  ~ tibble(
    column = .x,
    number_nas  = sum(is.na(DR1_data[[.x]]))
  )
)  

### 7. Save data ---- 
saveRDS(DR1_data, file = paste0(
  output_path,"/DR1_pre_processed_data.Rds")) 
