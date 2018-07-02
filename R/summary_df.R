#' Get the summary of data, which includes Column_Name, Data_Type, 
#' N_Unique (Number of unique values), Values (range for non-factor and non-character,
#' and categories for factor and character), and NA_Ratio. The result is returned 
#' as a data frame.
#' 
#' \code{summary_df} provides the useful statistics for all columns
#' 
#' @param data The data frame to be summarized
#' @examples
#' data <- rpart::stagec
#' summary_df(data)
#' @export
 
summary_df <- function(data) {
  # str_dt <- capture.output(str(data))
  # 
  # split_dt <- lapply(str_dt[-1], function(x) {
  #   split_pos <- regexpr(':', x)
  #   Data_Type <- substr(x, start = split_pos + 2, stop = nchar(x))[[1]][1]
  # })
  dt_summary <- data.frame(
    Column_Name = colnames(data), 
    Data_Type = sapply(data, class), 
    N_Unique = sapply(data, function(x) length(unique(x))),
    Values = sapply(data, function(x) {
      if(!is.factor(x) & !is.character(x)) {
        paste0('[', min(x, na.rm=T), ', ', max(x, na.rm = T), ']') 
      } else {
        categories <- names(sort(-table(x)))  # sort descendingly
        if(length(categories) <= 5) {
          paste0(categories, collapse = ', ')
        } else {
          paste0(c(categories[1:5], '...'), collapse = ', ')
        }
      }}), 
    NA_Ratio = sapply(data, function(x) sum(is.na(x)) / length(x)), 
    row.names = NULL
  )
  
  return(dt_summary)
}

# dt <- gamclass::FARS
# dt.org <- read.csv('../student.performance/data/student.grade.csv')
# get_data_summary(dt)
