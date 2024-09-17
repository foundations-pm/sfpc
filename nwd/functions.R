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
        
        dplyr::mutate(summary_tb,
                      covariate = var) })
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
  
  table = dplyr::mutate(
    table,
    strength_of_association = case_when(
      cramers_v < 0.1 ~ 'weak',
      cramers_v >= 0.1 & cramers_v < 0.3 ~ 'moderate',
      cramers_v >= 0.3 & cramers_v < 0.5 ~ 'strong',
      cramers_v >= 0.5 ~ 'very strong'),
    stat_significance = ifelse(
      chi2_p_value < 0.05, 'Significant', 'Not significant'))
  
  return(table)
  
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
