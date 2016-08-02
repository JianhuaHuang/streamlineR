#' Generate a ggplot for the statistics in different levels (groups)
#' 
#' \code{level.stat} provides the useful statistics for different levels (groups) 
#' in the given independent variables. 
#' 
#' @param data The data frame used for the ggplot. It is usually the output from 
#'    \code{level.stat}. It can be other data frame, if the structure 
#'    of the data is appropriate. 
#' @param var The column represents the variables, which is used as the head in
#'    each plot panel. 
#' @param x The variable used as x axis
#' @param y The variable used as y axis
#' @param y.label The text used to label the y value
#' @param y.label.col The color of the \emph{y.label} text
#' @param y.title The title of y axis for the whole plot
#' @param bar.col The color of the bar
#' @param width The column used to represents the width of the bar, or a contant
#'    numeric value. 
#' @param width.label The text used to label the Width of the bar
#' @param width.label.col The color of the \emph{width.label} text
#' @param ncol Number of column for the plot
#' @param theme The theme of the plot. It can be 'classic' or 'ws' (Wall Street)
#' @param background The background color
#' 
#' @return a data frame inclduing frequency, rates, Weight of Evidence (WOE), 
#'    and Information Value (IV) for each level. The return data is already
#'    ranked by the IV values from high to low. 
#'    
#' @examples
#' data <- rpart::stagec
#' st <- level.stat(data, x = c('eet', 'ploidy'), y = 'pgstat')
#' ggstat(st, theme = 'ws')
#' @export

ggstat <- function(data, var = 'Variable.IV', x = 'Group', y = 'Rate.1',
  y.label = 'Perc.1', y.label.col = 'red', y.title = NULL, 
  bar.col = 'cornflowerblue', width = 'Rate.group', width.label = 'Perc.group', 
  width.label.col = 'black', ncol = NULL, theme = c('classic', 'ws'), 
  background = 'white') {
  
  theme = match.arg(theme)
  
  data$var = data[, var]
  data$x = data[, x]
  data$y = data[, y]
  
  if(is.null(y.label)) {
    data$y.label = ''
  } else {
    data$y.label = data[, y.label]
  }
  
  if(is.numeric(width)) {
    data$width = width
  } else {
    data$width = data[, width]
  }
  
  if(is.null(width.label)) {
    data$width.label = ''
  } else {
    data$width.label = data[, width.label]
  }
  
  if(is.null(ncol)) ncol <- ceiling(sqrt(length(unique(data$var))))
  
  y.range <- max(data$y) -  min(data$y)
  y.min <- min(data$y) - y.range * .15
  y.max <- max(data$y) + y.range * .15
  
  p <- ggplot(data, aes(x = x, y = y)) +
    facet_wrap(~ var, scale = 'free', ncol = ncol) +
    geom_bar(aes(width = width + .1), stat = 'identity',
      fill = bar.col, color = bar.col) +
    geom_text(aes(y = y, label = y.label), size = 3, color = y.label.col,
      nudge_y = ifelse(data$y > 0, .06, -.06) * y.range) +
    geom_text(aes(y = y.min, label = width.label), size = 3, 
      color = width.label.col) +
    labs(x = NULL, y = y.title) +
    scale_y_continuous(limits = c(y.min, y.max), oob = rescale_none)
  
  if(theme == 'classic') {
    p <- p + theme_classic() +
      theme(axis.text.x = element_text(angle=25, hjust=1),
        rect = element_rect(fill = background, linetype = 0, color = NA),
        panel.background = element_rect(fill = background),
        axis.line.x = element_line(), axis.line.y = element_line(),
        axis.title = element_text(size=12,face="bold"),
        text = element_text(size = 10), strip.text = element_text(face = 'bold'),
        strip.background = element_blank())}
  
  if(theme == 'ws') {
    p <- p + theme_ws(background = background) + 
      theme(axis.text.x = element_text(angle=25, hjust=1),
        axis.title = element_text(size=12,face="bold"),
        text = element_text(size = 10), strip.text = element_text(face = 'bold'),
        strip.background = element_blank())}
  
  p
} 

