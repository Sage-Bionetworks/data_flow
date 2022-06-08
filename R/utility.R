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
  
  # for some reason the columns of this df are class = list
  # this hasn't really caused a problem, but I want class = char
  df <- data.frame(apply(df, 2, unlist))
  
  # if colnames are specified, add them
  if (!is.null(col_names)) {
    names(df) <- col_names
  }
  
  return(df)
}