#' Display some frequently-used point shapes for ggplot
#'
#' The code is based on http://sape.inf.usi.ch/quick-reference/ggplot2/shape, 
#' with slight pattern change. 
#' 
#' @return A figure with point shapes and corresponding index
#'    
#' @examples
#' display.pch()
#' @export

display.pch <- function() {
  # Display all possible pch (point shape) used for ggplot
  # reference to http://sape.inf.usi.ch/quick-reference/ggplot2/shape
  d=data.frame(p=c(0:25, 32:127))
  ggplot(data=d) +
    scale_y_continuous(name = '', breaks = NULL) +
    scale_x_continuous(name = '', breaks = NULL) +
    scale_shape_identity() +
    geom_point(aes(x=p%%16, y=7 - p%/%16, shape=p), size=5, fill="red") +
    geom_text(aes(x=p%%16, y=7 - p%/%16+0.25, label=p), size=3) +
    theme_ws()
}
