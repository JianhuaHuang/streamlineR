#' Plot the distribution (geom_histogram) of each column in the given data frame. If the data is 
#' categorical data (factor or character), the statistics is set as "count". 
#' Otherwise, it is set as "bin"
#' 
#' \code{plot.distr} plot the histogram/bar for numeric/categorical variables
#' 
#' @param data The data frame used for plot
#' @param n_sample Number of samples used for the plot. If it is NULL, then all 
#' records are used
#' @param ncol Number of columns for the plot layout 
#' @param sample_seed The seed used for sampling
#' 
#' @return ggplot of the histogram/bar for all variables
#'    
#' @examples
#' data <- rpart::stagec
#' plot.distr(data)
#' @export

plot.distr <- function(data, n_sample=NULL, ncol=3, sample_seed = 123, ...) {
  set.seed(sample_seed)
  if(is.null(n_sample)) {
    n_sample <- nrow(data)
  }
  dt_sample <- data[sample(nrow(data), min(n_sample, nrow(data))), ]
  
  plot_list <- lapply(colnames(dt_sample), function(col_x) {
    cat_flag <- is.factor(data[, col_x]) | is.character(data[, col_x])
    ggplot(dt_sample, aes_string(x = col_x)) + 
      geom_histogram(stat = ifelse(cat_flag, 'count', 'bin'), ...) 
  })
  
  cowplot::plot_grid(plotlist = plot_list, ncol = ncol)
}

