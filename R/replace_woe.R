#' replace original levels with Weight of Evidence (WOE)
#' 
#' \code{replace.woe} replace the original levels with the calculated WOE from 
#' the \code{level.stat} function. It will replace all variables that can be 
#' found from \emph{stat}
#' 
#' @param data The data frame needs to be replaced
#' @param stat The statistics generated from \code{level.stat}
#' @param replace Whether to replace (replace = TRUE) the original values or 
#' add a new column to the end of the data (replace = FALSE). 
#' 
#' 
#' @return a data frame with WOE values.  
#'    
#' @examples
#' data <- rpart::stagec
#' st <- level.stat(data, x = c('eet', 'ploidy'), y = 'pgstat')
#' replace.woe(data, stat = st)
 
replace.woe <- function(data, stat, replace = FALSE) {
  # This function is used to replace the categorical variables with the
  # corresponding woe value claculated with level.stat function
  # Args:
  #    data: the input data.frame
  #    stat: the data.frame output from the level.stat function
  #    replace: FALSE/TRUE. If FALSE, the woe values will be added to the end.
  #             if TRUE, the original levels will be replaced by woe values
  # Return: a data.frame with woe
  
  dt.rp <- data[, intersect(colnames(data), stat$Variable)]
  dt.rp.woe <- lapply(colnames(dt.rp), function(x) {
    woe.x <- stat[stat$Variable == x, ]
    group.match <- match(dt.rp[, x], woe.x$Group)
    woe.x$WOE[group.match]
  })
  
  dt.rp.woe <- do.call(cbind, dt.rp.woe)
  
  if(replace == FALSE) {
    colnames(dt.rp.woe) <- paste0(colnames(dt.rp), '_woe')
    return(data.frame(data, dt.rp.woe))
  } else {
    data[, colnames(dt.rp)] <- dt.rp.woe
    return(data)
  }
}
