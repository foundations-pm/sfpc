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
  
  if(!is.na(
    str_extract(str_flatten(colnames(data)),
                'referral_no_further_action'))){
    
    print('Recoding referral no action')
    
    data = dplyr::mutate(
      data,
      
      referral_no_further_action = case_when(
        referral_no_further_action %in% c('0', 'FALSE', 'N', 'No') ~ 'Further action',
        referral_no_further_action %in% c('1', 'TRUE', 'Y', 'Yes') ~ 'No further action',
        TRUE ~ NA))
    
  }
  
  data = dplyr::mutate(
    data,
    
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
        'White: Gypsy, Irish Traveller, Roma or Other White',
      
      ethnicity %in% c(
        'MWBC','MWBA','MWAS','MOTH',
        'f) MWBC','g) MWBA','h) MWAS','i) MOTH') ~ 
        'Mixed or Multiple ethnic groups',
      
      ethnicity %in% c(
        'AIND','APKN','ABAN','AOTH', 'CHNE',
        'j) AIND', 'k) APKN', 'l) ABAN', 'm) AOTH', 'q) CHNE') ~ 
        'Asian, Asian British or Asian Welsh',
      
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
      dplyr::ungroup() %>%
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
          dplyr::relocate(cv, .after = sd)
        
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
        dplyr::ungroup() %>%
        dplyr::select(where(~ is.character(.x)), 
                      where(~ is.factor(.x))) %>% 
        dplyr::select(-any_of(group)) %>%
        colnames()
      
    } else {
      
      covariates = data %>% 
        dplyr::ungroup() %>%
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
        
        dplyr::mutate(summary_tb, covariate = var)
        
      })
  }
  
  if(class == "date"){
    
    print('Pulling descriptive statistics for date variables in data')
    
    covariates = data %>% 
      dplyr::ungroup() %>%
      dplyr::select(where(~ lubridate::is.Date(.x))) %>% 
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
    dplyr::mutate(across(.cols = where(is.numeric),
                         .fns = ~ round(.x, 2)))
  
  if(is.na(group[1])){
    
    descriptive_table = descriptive_table %>%
      dplyr::relocate(covariate)
    
  } else {
    
    print("Grouped descriptive statistics")
    
    descriptive_table = descriptive_table %>%
      dplyr::relocate(covariate, .after = any_of(group))
    
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
      dplyr::group_by(across(any_of(group))) %>%
      dplyr::summarise(count = n()) %>%
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
  
  # Chi-squared test
  contigency_table = table(data[[missing_covariate]],
                           data[[auxiliary]])
  
  chi2_test = chisq.test(contigency_table)
  
  # Check freq 
  expected_freq = chi2_test$expected
  freq_checks = c(expected_freq < 5)
  freq_checks = isTRUE(any(freq_checks))
  
  print(paste0('Covariate: ', auxiliary))
  print(paste0('Expected frequency with missing ethnicity: '))
  print(expected_freq)
  
  # Fishers' test
  f_test = fisher.test(contigency_table, simulate.p.value = TRUE)
  
  # Cramer's V 
  cramers_v = CramerV(contigency_table)
  
  table = data.frame(missing_covariate = missing_covariate,
                     auxiliary = auxiliary,
                     cramers_v = cramers_v,
                     X_squared = chi2_test[[1]],
                     exp_freq_under_5 = freq_checks,
                     chi2_p_value = chi2_test[[3]])
  
  row.names(table) <- NULL
  
  table = dplyr::mutate(
    table,
    fishers_test_p_value = case_when(
      exp_freq_under_5 == FALSE ~ NA,
      exp_freq_under_5 == TRUE ~ f_test[[1]]),
    strength_of_association = case_when(
      cramers_v < 0.1 ~ 'weak',
      cramers_v >= 0.1 & cramers_v < 0.3 ~ 'moderate',
      cramers_v >= 0.3 & cramers_v < 0.5 ~ 'strong',
      cramers_v >= 0.5 ~ 'very strong'),
    stat_significance = case_when(
      exp_freq_under_5 == FALSE & chi2_p_value < 0.05 ~ 'Significant',
      exp_freq_under_5 == FALSE & chi2_p_value > 0.05 ~ 'Not significant',
      exp_freq_under_5 == TRUE & fishers_test_p_value < 0.05 ~ 'Significant',
      exp_freq_under_5 == TRUE & fishers_test_p_value > 0.05 ~ 'Not significant'
      )) 
  
  return(table)
  
}

#' Get monthly rates
#'
#' @param data 
#' @param rate 
#' @param title 
#'
#' @return
#' @export
#'
#' @examples
get_monthly_rates = function(
    data, rate, title){
  
  restriction_periods <- data.frame(
    period = 'restrictions',
    xmin = as.Date(c("2020-09-14", "2020-12-03")),
    xmax = as.Date(c("2020-10-30", "2021-01-05")),
    ymin = -Inf,
    ymax = Inf
  )
  
  lockdown_periods <- data.frame(
    period = 'lockdowns',
    xmin = as.Date(c("2020-03-23", "2020-11-01", "2021-01-06")),
    xmax = as.Date(c("2020-06-01", "2020-12-02", "2021-03-08")),
    ymin = -Inf,
    ymax = Inf
  )
  
  ggplot(data, 
         aes(x = month, 
             y = .data[[rate]], 
             color = local_authority)) +
    
    # Shaded area for Lockdown periods
    geom_rect(
      data = lockdown_periods, 
      aes(xmin = xmin, xmax = xmax,
          ymin = ymin, ymax = ymax, 
          fill = period), 
      alpha = 0.5, inherit.aes = FALSE) +  # Darker grey with alpha 0.5
    # Shaded area for Restrictions periods
    geom_rect(
      data = restriction_periods, 
      aes(xmin = xmin, xmax = xmax, 
          ymin = ymin, ymax = ymax, 
          fill = period), 
      alpha = 0.2, inherit.aes = FALSE) +  # Lighter grey with alpha 0.2
    # Plot points and lines
    geom_point() +
    geom_line() +
    # Customize x-axis ticks and labels
    scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +  
    # Custom color palette for local authorities
    scale_color_manual(values = custom_colors) +
    # Define the fill color and transparency for the shaded areas
    scale_fill_manual(
      values = c("lockdowns" = "grey", "restrictions" = "grey"),  # Both use grey, but alpha is different
      name = "Covid restrictions",  # Legend title
      guide = guide_legend(override.aes = list(alpha = c(0.5, 0.2)))) +  # Alpha levels in the legend
    
    # Add labels and theme
    labs(title = title, 
         x = "Month", 
         y = "Rate per 10,000 children", 
         color = 'Local Authority') +  
    theme_minimal()
}


#' Get raw estimates
#'
#' @param summary_model_fit 
#' @param analysis_type 
#' @param formula 
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
get_raw_estimates = function(
    summary_model_fit,
    analysis_type,
    formula,
    date){
  
  df = data.frame(
    analysis_type = analysis_type,
    formula = formula,
    Coefficients = summary_model_fit$coefficients[, "Estimate"],       # Log Odds
    `Standard Error` = summary_model_fit$coefficients[, "Std. Error"],
    `p-value` = summary_model_fit$coefficients[, "Pr(>|z|)"],
    date = date)
  
  df = df %>% 
    tibble::rownames_to_column('term') %>%
    dplyr::relocate(term, .after = formula)
  
}

#' Get tidy estimates
#'
#' @param model_fit 
#' @param analysis_type 
#' @param formula 
#' @param date 
#'
#' @return
#' @export
#'
#' @examples
get_tidy_estimates = function(model_fit, 
                              analysis_type,
                              formula,
                              date){
  
  tidy_m = broom.mixed::tidy(
    model_fit, conf.int=TRUE, 
    exponentiate=TRUE,
    effects=c("fixed"))
  
  tidy_m = tidy_m %>%
    dplyr::mutate(
      date = date,
      across(where(is.numeric), round,4),
      analysis_type = analysis_type,
      formula = formula
    ) %>%
    dplyr::relocate(analysis_type, formula) 
  
  
}


#' Get performance table
#'
#' @param model_fit 
#' @param analysis_type 
#'
#' @return
#' @export
#'
#' @examples
get_performance_table = function(
    model_fit,
    formula,
    analysis_type){
  
  performance_df = performance::model_performance(model_fit)
  
  performance_df = performance_df %>%
    dplyr::mutate(analysis_type = analysis_type, 
                  formula = formula, 
                  date = date) %>%
    dplyr::relocate(analysis_type, formula)
  
}

#' Get optimisers warning messages
#' @param glmer_model_fit 
#'
#' @return
#' @export
#'
#' @examples
get_optimisers_warning_messages = function(
    glmer_model_fit,
    formula,
    analysis_type){
  
  aa <- allFit(glmer_model_fit)
  ss_list <- summary(aa) 
  
  # Convert into table 
  ss_df = data.frame(
    Optimizer = names(ss_list$msgs),
    Message = sapply(
      ss_list$msgs, 
      function(msg) {
        if (is.null(msg)) {
          "[OK]"  # Replace NULL with "[OK]" or use NA if preferred
        } else {
          msg  # Keep the warning message
        }
      }),
    stringsAsFactors = FALSE)
  
  ss_df <- ss_df %>%
    pivot_wider(names_from = Optimizer, 
                values_from = Message)
  
  ss_df = ss_df %>%
    dplyr::mutate(analysis_type = analysis_type,
                  formula = formula,
                  date = date) %>%
    dplyr::relocate(analysis_type, formula)
  
}

#' Get VIF table
#'
#' @param model_fit 
#'
#' @return
#' @export
#'
#' @examples
get_vif_table = function(model_fit,
                         formula,
                         analysis_type){
  
  vif_table = performance::check_collinearity(model_fit) 
  
  vif_table = vif_table %>%
    dplyr::mutate(analysis_type = analysis_type, 
                  formula = formula, 
                  date = date) %>%
    dplyr::relocate(analysis_type, formula)
  
}

#' Append results
#'
#' @param output_file a string
#' @param table_to_append an object of class 'df' or 'data.frame'
#' @param save_to a string, the name of the file to save
#'
#' @return an object of class 'df'
#' 
append_results = function(
    output_file, 
    table_1_to_append,
    table_2_to_append = NA,
    is_multisheet_workbook = FALSE,
    save_to){
  
  cat(
    crayon::red(
      crayon::bold(
        "MAKE SURE YOUR DIRECTORY IS CORRECTLY SPECIFIED")))
  
  if(is_multisheet_workbook == FALSE){
    
    if(is_empty(output_file) == FALSE){
      
      cat(
        crayon::green(
          crayon::bold(
            "Loading output file.")))
      
      output_tb = readxl::read_excel(
        paste0(output_file))
      
      table_1_to_append = table_1_to_append %>%
        dplyr::mutate(across(.cols = everything(),
                             .fns = ~ as.character(.x)))
      
      output_tb = output_tb %>%
        dplyr::mutate(across(.cols = everything(),
                             .fns = ~ as.character(.x)))
      
      output_tb = dplyr::bind_rows(
        output_tb, table_1_to_append)
      
      writexl::write_xlsx(
        output_tb,
        paste0(save_to))
      
    } else {
      
      cat(
        crayon::red(
          crayon::bold(
            "There are no output files in the specified directory:\n",
            "Saving file as is.")))
      
      writexl::write_xlsx(
        table_1_to_append,
        paste0(save_to)) # save as 'raw', 'tidy' or 'performance'
    } 
    
  }
  
  if(is_multisheet_workbook == TRUE){
    
    print('This is a multi-sheet excel output')
    
    if(purrr::is_empty(output_file)){ 
      
      print(cat(crayon::bold(crayon::red(
        paste0('There is no existing diagnostics output file.\n')))))
      
      print('Creating diagnostics output file')
      
      wb = openxlsx::createWorkbook()
      
      # Add the first sheet with table_1
      openxlsx::addWorksheet(wb, "General diagnostics")
      writeData(wb, "General diagnostics", table_1_to_append)
      
      # Add the second sheet with table_2
      openxlsx::addWorksheet(wb, "Multicollinearity")
      writeData(wb, "Multicollinearity", table_2_to_append)
      
      openxlsx::saveWorkbook(wb, save_to)
      
    } else{ 
      
      print(cat(crayon::bold(crayon::green(
        paste0('Loading diagnostics output file.\n')))))
      
      general_diagnostics_file = readxl::read_excel(
        path = paste0(output_file),
        sheet = 'General diagnostics')
      
      vif_file = readxl::read_excel(
        path = paste0(output_file),
        sheet = 'Multicollinearity')
      
      general_diagnostics_file = dplyr::bind_rows(
        general_diagnostics_file, table_1_to_append)
      
      vif_file = dplyr::bind_rows(
        vif_file, table_2_to_append)
      
      wb = openxlsx::createWorkbook()
      
      # Add the first sheet with table_1
      openxlsx::addWorksheet(wb, "General diagnostics")
      writeData(wb, "General diagnostics", general_diagnostics_file)
      
      # Add the second sheet with table_2
      openxlsx::addWorksheet(wb, "Multicollinearity")
      writeData(wb, "Multicollinearity", vif_file)
      
      openxlsx::saveWorkbook(
        wb, save_to, overwrite = TRUE)
      
    }
    
    
  }
  
}

# Compare proportions in categorical variables between observed and imputed data
# visually (using ggplot)
#
# Parameters:
# x: mids object (from mice)
# formula: formula describing which variables to plot
# facet: either "wrap" for facet_wrap or "grid" for facet_grid
# ...: additional parameters passed to theme()
#
# Note: if the formula is not specified, all imputed categorical variables are
# plotted. 
# 
# A formula has the structure:
# categorical variables ~ faceting variables | color variable
# 
# By default, .imp (imputation set identifier) will be used as color variable.
#
# This function uses the following packages:
# - mice
# - reshape2
# - RColorBrewer
# - ggplot2
propplot <- function(
    x, formula, facet = "wrap",
    label_size = 10, show_prop = FALSE, prop_size = 2, ...) {
  library(ggplot2)
  
  cd <- data.frame(mice::complete(x, "long", include = TRUE))
  cd$.imp <- factor(cd$.imp)
  
  r <- as.data.frame(is.na(x$data))
  
  impcat <- x$meth != "" & sapply(x$data, is.factor)
  vnames <- names(impcat)[impcat]
  
  if (missing(formula)) {
    formula <- as.formula(paste(paste(vnames, collapse = "+",
                                      sep = ""), "~1", sep = ""))
  }
  
  tmsx <- terms(formula[-3], data = x$data)
  xnames <- attr(tmsx, "term.labels")
  xnames <- xnames[xnames %in% vnames]
  
  if (paste(formula[3]) != "1") {
    wvars <- gsub("[[:space:]]*\\|[[:print:]]*", "", paste(formula)[3])
    # wvars <- all.vars(as.formula(paste("~", wvars)))
    wvars <- attr(terms(as.formula(paste("~", wvars))), "term.labels")
    if (grepl("\\|", formula[3])) {
      svars <- gsub("[[:print:]]*\\|[[:space:]]*", "", paste(formula)[3])
      svars <- all.vars(as.formula(paste("~", svars)))
    } else {
      svars <- ".imp"
    }
  } else {
    wvars <- NULL
    svars <- ".imp"
  }
  
  for (i in seq_along(xnames)) {
    xvar <- xnames[i]
    select <- cd$.imp != 0 & !r[, xvar]
    cd[select, xvar] <- NA
  }
  
  
  for (i in which(!wvars %in% names(cd))) {
    cd[, wvars[i]] <- with(cd, eval(parse(text = wvars[i])))
  }
  
  meltDF <- reshape2::melt(cd[, c(wvars, svars, xnames)], id.vars = c(wvars, svars))
  meltDF <- meltDF[!is.na(meltDF$value), ]
  
  
  wvars <- if (!is.null(wvars)) paste0("`", wvars, "`")
  
  a <- plyr::ddply(meltDF, c(wvars, svars, "variable", "value"), plyr::summarize,
                   count = length(value))
  b <- plyr::ddply(meltDF, c(wvars, svars, "variable"), plyr::summarize,
                   tot = length(value))
  mdf <- merge(a,b)
  mdf$prop <- mdf$count / mdf$tot
  
  plotDF <- merge(unique(meltDF), mdf)
  plotDF$value <- factor(plotDF$value,
                         levels = unique(unlist(lapply(x$data[, xnames], levels))),
                         ordered = T)
  
  p <- ggplot(plotDF, aes(x = value, fill = get(svars), y = prop)) +
    geom_bar(position = "dodge", stat = "identity") +
    theme(
      axis.text.x = element_text(angle = 10, hjust = 1, size = label_size),
      legend.position = "bottom", ...) +
    ylab("proportion") +
    scale_fill_manual(name = "",
                      values = c("red",
                                 colorRampPalette(
                                   RColorBrewer::brewer.pal(9, "Blues"))(x$m + 3)[1:x$m + 3])) +
    guides(fill = guide_legend(nrow = 1)) +
    ggtitle('Ethnic proportions: observed versus imputed data')
  
  if(isTRUE(show_prop)){
    
    p <- p + geom_text(
      aes(label = round(prop, 2)), 
      position = position_dodge(width = 0.9),   # Align text with bars
      vjust = -0.5,
      size = prop_size)
    
  }
  
  if (facet == "wrap")
    if (length(xnames) > 1) {
      return(p + facet_wrap(c("variable", wvars), scales = "free"))
    } else {
      if (is.null(wvars)) {
        return(p)
      } else {
        return(p + facet_wrap(wvars, scales = "free"))
      }
    }
  
  if (facet == "grid")
    if (!is.null(wvars)) {
      return(p + facet_grid(paste(paste(wvars, collapse = "+"), "~ variable"),
                           scales = "free"))
    }
  
    
}
