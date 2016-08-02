#' a classic simple theme with x and y axis
#'
#' @return A ggplot with simple theme
#'    
#' @examples
#' ggplot(mtcars, aes(x = mpg, y = disp, color = as.factor(cyl))) + 
#'   geom_point() + 
#'   theme_simple()

theme_simple <- function(...) {
  theme_bw() +
    theme(axis.line.x = element_line(size = .5),
      axis.line.y = element_line(size = .5),
      strip.text = element_text(face = 'bold'),
      panel.border = element_blank(),
      strip.background = element_blank(),
      axis.title = element_text(face = 'bold'),
      axis.title.y=element_text(margin=margin(0,10,0,0)),
      legend.title = element_text(face = 'bold'),
      panel.grid.major = element_line(linetype = 'dashed'),
      panel.grid.minor = element_blank(),
      ...)
}
