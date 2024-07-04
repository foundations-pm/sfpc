# Data cleaning for SFPC NWD RCT DR1 datasets ----

# Function sheet ----

#' Check raw data 
#'
#' @param data a 'tbl_df', 'tbl', 'data.frame' data frame
#' 
#' @return prints standard checks for any data frame
#' 
check_raw_data = function(data){
  
  print(paste0(str_flatten("Dimensions of the data: ", dim(data))))
  
  print(colnames(data))
  
  print(paste0("Total number of missing values: ", sum(is.na(data))))
  
}

#' Title
#'
#' @param excel_workbook_path 
#'
#' @return
#' @export
#'
#' @examples
read_xlsx_worksheets = function(excel_workbook_path){
  
  # Find data worksheet only
  sheets <- readxl::excel_sheets(excel_workbook_path)
  data_sheets <- sheets[grepl("Data", sheets, ignore.case = FALSE)]
  names(data_sheets) = data_sheets
  
  # Read data worksheets into a list
  lapply(data_sheets, function(.sheet) {
    
    readxl::read_excel(excel_workbook_path,
                       sheet = .sheet) })
  
}