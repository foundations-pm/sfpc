# Data cleaning for SFPC NWD RCT DR1 datasets ----

# Function sheet ----

#' check_raw_data
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

#' read_xlsx_worksheets
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

#' check_table_location
#'
#' @param data 
#' @param row_start 
#'
#' @return
#' @export
#'
#' @examples
check_table_location = function(data, row_start){
  
  data <- data %>%
    dplyr::mutate(across(everything(), as.character)) # temporary
  
  data <- data[row_start, ] 
  
  return(data) } 


#' recode values 
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
#' 
recode_values = function(data){
  
  data = mutate(
    data,
    
    referral_no_further_action = case_when(
      referral_no_further_action %in% c('0', 'FALSE', 'N', 'No') ~ 'Further action',
      referral_no_further_action %in% c('1', 'TRUE', 'Y', 'Yes') ~ 'No further action',
      TRUE ~ NA),
    
    gender = ifelse(
      gender %in% c(
        'Indeterminate','Transgender',
        'Not stated/recorded (or unborn)','UnkNwn'),
      'Other', gender),
    
    ethnicity_agg = case_when(
      
      ethnicity %in% c(
        'WBRI', 'WIRI',
        'a) WBRI', 'b) WIRI') ~ 
        'White British or Irish',
      
      ethnicity %in% c(
        'WIRT', 'WOTH', 'WROM', 
        'c) WIRT','d) WOTH', 'e) WROM') ~ 
        '	White: Gypsy, Irish Traveller, Roma or Other White',
      
      ethnicity %in% c(
        'MWBC','MWBA','MWAS','MOTH',
        'f) MWBC','g) MWBA','h) MWAS','i) MOTH') ~ 
        'Mixed or Multiple ethnic groups',
      
      ethnicity %in% c(
        'AIND','APKN','ABAN','AOTH', 'CHNE',
        'j) AIND', 'k) APKN', 'l) ABAN', 'm) AOTH', 'q) CHNE') ~ 
        '	Asian, Asian British or Asian Welsh',
      
      ethnicity %in% c(
        'BCRB','BAFR','BOTH',
        'n) BCRB', 'o) BAFR', 'p) BOTH') ~ 
        'Black, Black British, Black Welsh, Caribbean or African',
      
      ethnicity %in% c(
        'OOTH', 'r) OOTH') ~ 
        'Other ethnic group',
      
      ethnicity %in% c(
        'Not Recorded', 'REFU', 
        's) REFU', 'NOBT', 's) NOBT') | is.na(ethnicity) ~ NA,
    ),
    
    disabled_status = case_when(
      disabled_status %in% c('0', 'FALSE', 'N', 'No') ~ 'Not disabled',
      disabled_status %in% c('1', 'TRUE', 'Y', 'Yes') ~ 'Disabled',
      TRUE ~ NA),
    
    unaccompanied_asylum_seeker = case_when(
      unaccompanied_asylum_seeker %in% c('0', 'FALSE', 'N', 'No') ~ 'Not UASC',
      unaccompanied_asylum_seeker %in% c('1', 'TRUE', 'Y', 'Yes') ~ 'UASC',
      TRUE ~ NA),
    
    free_school_meal_eligibility_ever_fsm = case_when(
      free_school_meal_eligibility_ever_fsm %in% c(
        '0', 'FALSE', 'N', 'n', 'No', 'No Trace') ~ 'Not ever FSM',
      free_school_meal_eligibility_ever_fsm %in% c('1', 'True', 'Y', 'y', 'Yes') ~ 'Ever FSM',
      TRUE ~ NA),
    
    pupil_premium_eligibility_for_reception_year_1_and_year_2 = case_when(
      pupil_premium_eligibility_for_reception_year_1_and_year_2 %in% c(
        '0', 'FALSE', 'N', 'n', 'No', 'No Trace') ~ 'Not ever PPE',
      pupil_premium_eligibility_for_reception_year_1_and_year_2 %in% c(
        '1', 'True', 'Y', 'y', 'Yes') ~ 'Ever PPE',
      TRUE ~ NA))
  
  return(data)
  
}


#' describe
#'
#' @param data 
#' @param class 
#'
#' @return
#' @export
#'
#' @examples
describe = function(data, class = 'numeric', group = NA){
  
  if(class == "numeric"){
    
    print('Pulling descriptive statistics for continuous variables in data')
    
    covariates = data %>% 
      ungroup() %>%
      dplyr::select(where(~ is.numeric(.x))) %>% 
      colnames()
    
    print(paste0("Covariate list: ", 
                 stringr::str_flatten(
                   covariates, collapse = ", ")))
    
    descriptive_table = purrr::map_dfr(
      
      covariates, function(var){
        
        print(paste0('Variable: ', var))
        
        print(paste0('Is the data grouped? ',
                     is.grouped_df(data)))
        
        summary_tb = data %>% 
          dplyr::summarise(
            number_missing = sum(
              is.na(
                .data[[var]])),
            percent_missing = round(
              sum(
                is.na(.data[[var]]))/length(.data[[var]]),
              3),
            min = min(.data[[var]], na.rm = TRUE),
            max = max(.data[[var]], na.rm = TRUE),
            mean = mean(.data[[var]], na.rm = TRUE),
            sd = sd(.data[[var]], na.rm = TRUE),
            percentile_25 = quantile(.data[[var]], na.rm = TRUE)[[2]],
            median = quantile(.data[[var]], na.rm = TRUE)[[3]],
            percentile_75 = quantile(.data[[var]], na.rm = TRUE)[[4]],
            iqr = IQR(.data[[var]], na.rm = TRUE))
        
        summary_tb = dplyr::mutate(
          summary_tb,
          covariate = var,
          cv = sd/mean) %>%
          relocate(cv, .after = sd)
        
        print(summary_tb)
        
      })
    
    print(descriptive_table, n= 30)
  } 
  
  if(class == "categorical"){
    
    print('Pulling descriptive statistics for categorical variables in data')
    
    print(paste0('Is the data grouped? ',
                 is.grouped_df(data)))
    
    if(is.grouped_df(data)){
      
      covariates = data %>% 
        ungroup() %>%
        dplyr::select(where(~ is.character(.x)), 
                      where(~ is.factor(.x))) %>% 
        dplyr::select(-any_of(group)) %>%
        colnames()
      
    } else {
      
      covariates = data %>% 
        ungroup() %>%
        dplyr::select(where(~ is.character(.x)),
                      where(~ is.factor(.x))) %>% 
        colnames()  }
    
    print(paste0("Covariate list: ", 
                 stringr::str_flatten(
                   covariates, collapse = ", ")))
    
    descriptive_table = purrr::map_dfr(
      
      covariates, function(var){
        
        print(paste0('Variable: ', var))
        
        if(is.grouped_df(data)){
          
          summary_tb = data %>% 
            dplyr::group_by(across(any_of(c(group, var)))) %>%
            dplyr::summarise(count = n()) %>%
            dplyr::mutate(freq = count/sum(count)) %>%
            dplyr::rename(levels = any_of(var))
          
        } else {
          
          summary_tb = data %>% 
            dplyr::group_by(.data[[var]]) %>%
            dplyr::summarise(count = n()) %>%
            dplyr::mutate(freq = count/sum(count)) %>%
            dplyr::rename(levels = any_of(var))
       }
        
        dplyr::mutate(summary_tb,
                      covariate = var) })
  }
  
  if(class == "date"){
    
    print('Pulling descriptive statistics for date variables in data')
    
    covariates = data %>% 
      ungroup() %>%
      select(where(~ lubridate::is.Date(.x))) %>% 
      colnames()
    
    print(paste0("Covariate list: ", 
                 stringr::str_flatten(
                   covariates, collapse = ", ")))
    
    descriptive_table = purrr::map_dfr(
      
      covariates, function(var){
        
        print(paste0('Variable: ', var))
        
        summary_tb = data %>% 
          dplyr::summarise(
            number_missing = sum(
              is.na(.data[[var]])),
            percent_missing = round(
              sum(
                is.na(.data[[var]]))/length(.data[[var]]), 3),
            min = min(.data[[var]], na.rm = TRUE),
            max = max(.data[[var]], na.rm = TRUE))
        
        dplyr::mutate(
          summary_tb,
          covariate = var)  }) }
  
  descriptive_table = descriptive_table %>%
    mutate(across(.cols = where(is.numeric),
                  .fns = ~ round(.x, 2)))
  
  if(is.na(group[1])){
    
    descriptive_table = descriptive_table %>%
      relocate(covariate)
    
  } else {
    
    print("Grouped descriptive statistics")
    
    descriptive_table = descriptive_table %>%
      relocate(covariate, .after = any_of(group))
    
  }
  
  return(descriptive_table)
  
}


#' Get missing crosstabs
#'
#' @param covariate 
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
get_missing_crosstabs = function(covariate, data){
  
  groups = list(c(covariate),
                c('local_authority', covariate),
                c('local_authority', 'wedge', covariate))
  
  missing_cross_tab = lapply(groups, function(group){
    
    data %>%
      group_by(across(any_of(group))) %>%
      summarise(count = n()) %>%
      dplyr::mutate(freq = count/sum(count)) 
  })
  
  return(missing_cross_tab)
}

#' Check MNAR
#'
#' @param data 
#' @param missing_covariate 
#' @param auxiliary 
#'
#' @return
#' @export
#'
#' @examples
check_mnar = function(data,
                      missing_covariate,
                      auxiliary){
  
  data = data
  
  contigency_table = table(data[[missing_covariate]],
                           data[[auxiliary]])
  
  chi2_test = chisq.test(contigency_table)
  
  cramers_v = CramerV(contigency_table)
  
  table = data.frame(missing_covariate = missing_covariate,
                     auxiliary = auxiliary,
                     cramers_v = cramers_v,
                     X_squared = chi2_test[[1]],
                     chi2_p_value = chi2_test[[3]])
  
  row.names(table) <- NULL
  
  return(table)
  
}
