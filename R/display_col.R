#' Display the color options for ggplot
#'
#' The code is based on http://sape.inf.usi.ch/quick-reference/ggplot2/colour, 
#' with slight pattern change. 
#' 
#' @return A figure with colors and the corresponding names
#'    
#' @examples
#' display.col()
#' @export

display.col <- function() {
  # Display all possible colors in ggplot2
  # reference to http://sape.inf.usi.ch/quick-reference/ggplot2/colour
  d = grep('gray|grey.*[1-9]$|grey100', colors(), value = T, invert = T) %>%
    matrix(ncol = 9) %>%
    melt
  colnames(d) <- c('y', 'x', 'c')
  d$y = max(d$y) - d$y
  
  ggplot(data = d) +
    scale_x_continuous(name = '', breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(name = '', breaks = NULL, expand = c(0, 0)) +
    scale_fill_identity() +
    geom_rect(aes(
      xmin = x, xmax = x + 1, ymin = y, ymax = y + 1), fill = "white") +
    geom_rect(aes(
      xmin = x + 0.05, xmax = x + 0.95, ymin = y + 0.5, ymax = y + 1, fill=c)) +
    geom_text(aes(
      x = x + 0.5, y = y, label = c), nudge_y = .55, color = 'black', size=3) +
    theme_ws()
}
