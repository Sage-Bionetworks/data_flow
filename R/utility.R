###############################
## Generic Utility Functions ##
###############################

#' Convert a list to a dataframe
#'
#' @param list A list of items. Only works where list entries all follow the same pattern.
#' @param col_names Desired column names. Ex: `c("id", "name")`
#' 
#' @export

list_to_dataframe <- function(list,
                              col_names = NULL) {
  
  # convert list to dataframe
  df <- data.frame(do.call(rbind, list))
  
  if (length(list) != 1){
    # for some reason the columns of this df are class = list
    # this hasn't really caused a problem, but I want class = char
    df <- data.frame(apply(df, 2, unlist))
  }
  
  # if colnames are specified, add them
  if (!is.null(col_names)) {
    names(df) <- col_names
  }
  
  return(df)
}

#' Convert date columns from date to string
#'
#' @param df a data flow status manifest.
#' @param col_names columns to update
#' @param type date, character, integer, logical, icon, or factor
#' 
#' @export

convert_column_type <- function(df,
                                col_names,
                                type) {
  if (type == "date") {
    df[col_names] <- lapply(df[,col_names], as.Date)
  } else if (type == "character") {
    df[col_names] <- sapply(df[,col_names], as.character)
  } else if (type == "factor") {
    df[col_names] <- lapply(df[,col_names], as.factor)
  } else if (type == "logical") {
    df[col_names] <- sapply(df[,col_names], as.logical)
  } else if (type == "integer") {
    df[col_names] <- sapply(df[,col_names], as.integer)
  } else if (type == "icon") {
    df[col_names] <- sapply(df[,col_names], true_false_icon)
  } else {
    stop(paste0(type, " is not a supported date type"))
  }
  
  return(df)
}

#' Convert a vector of TRUE/FALSE to icon html
#'
#' @param vec A vector of TRUE/FALSE
#' 
#' @export

true_false_icon <- function(vec) {
  
  # if true assign checkmark icon
  # if false assign x icon
  true_icon <- as.character(icon("check", lib = "font-awesome"))
  false_icon <- as.character(icon("xmark", lib = "font-awesome"))
  
  ifelse(vec == TRUE, true_icon, false_icon)
}

#' Rearrange a dataframe. Expected columns are ordered to match `expected_col_names` and moved to the front of the dataframe. Unexpected columns are moved to the back of the dataframe.
#'
#' @param df a dataframe
#' @param expected_col_names a vector of expected column names in the order you would like them to appear.
#' 
#' @export

rearrange_dataframe <- function(df,
                                expected_col_names) {
  
  # capture column names of unexpected columns
  other_column_names <- setdiff(names(df), expected_col_names)
  
  # create separate dataframes
  # order expected columns to match expected_col_names
  exected_df <- df[expected_col_names]
  other_df <- df[other_column_names]
  
  # bind back together
  dplyr::bind_cols(exected_df, other_df)
  
}