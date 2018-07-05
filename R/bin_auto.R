#' Bin numeric variables using \code{bin.rpart} and \code{bin.custom} automatically
#' 
#' \code{bin.rpart} is tried first. If no bin is found, then \code{bin.custom} is 
#' used to bin the variable using the cut pionts of 20%, 40%, 60%, and 80% quantiles. 
#' 
#' \code{bin.auto} automatically bin a list of numeric variables, and return the 
#' cut points and bins
#' 
#' @param data The data frame used for binning
#' @param x_num The list of numeric variables. If it is NULL, then all variables except y 
#'    are used. 
#' @param y The binary outputs  
#' 
#' @return a list containing cut points and bins
#'    
#' @examples
#' data <- rpart::stagec
#' x_num <- c('age', 'pgtime')
#' bin.auto(data, x_num = x_num, y = 'pgstat')
#' @export

bin.auto <- function(data, x_num=NULL, y) {
  if(is.null(x_num)) {
    x_num = setdiff(colnames(data), y)
  }
  
  x_bin <- lapply(x_num, function(x) {
    # if no cut point is found, bin.rpart will return 'No Bin'
    # if there is an error, tryCatch will also return 'No Bin'
    rs <- tryCatch(
      bin.rpart(as.formula(paste(y, '~', x)), dt,
        rcontrol = rpart.control(minbucket = 0.02 * nrow(data))), 
      error = function(e) {'No Bin'}
    )
    
    if(identical(rs, 'No Bin')) {
      cut.points = unique(quantile((data[, x]), 1:5 * .2, na.rm = T))
      bins = bin.custom(data[, x], cut.p = cut.points)
      rs = list(cut.points = cut.points, bins = bins)   
    }
    
    print(paste(x, 'cut.points:', paste(rs$cut.points, collapse = ' ')))
    print(table(rs$bins))
    print('\n==========\n')
    return(rs)
  })
  
  names(x_bin) <- x_num
  return(x_bin)
}

