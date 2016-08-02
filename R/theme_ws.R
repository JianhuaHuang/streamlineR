#' A theme like Wall Street Journal figures
#' 
#' A theme for ggplot, which looks like the Wall Street Journal figures. 
#' It is similar to the \emph{theme_wsj} in the \emph{ggthemes} package, with the
#' font simplified. 
#'
#' @return A ggplot with Wall-Street-Journal theme
#'    
#' @examples
#' ggplot(mtcars, aes(x = mpg, y = disp, color = as.factor(cyl))) + 
#'   geom_point() + 
#'   theme_ws()
#' @export

theme_ws <- function(background = 'ivory', ...) {
  theme_bw() +
    theme(axis.line.x = element_line(size = .5),
      rect = element_rect(fill = background, linetype = 0, color = NA),
      panel.border = element_blank(),
      strip.text = element_text(family = 'sans', face = 'bold.italic'),
      strip.background = element_blank(),
      axis.title = element_text(face = 'bold'),
      axis.title.y=element_text(margin=margin(0,10,0,0)),
      legend.title = element_text(face = 'bold'),
      legend.position = 'top',
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linetype = 'dashed', color = 'grey50'),
      axis.ticks.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = background),
      ...)
}