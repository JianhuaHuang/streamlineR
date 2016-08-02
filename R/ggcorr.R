#' Generate the ggplot for a correlation matrix
#' 
#' @param cor.mat A correlation matrix, usually generated with cor() function
#' @param lower Whether to plot the lower triangle only
#' @param psize The poit size for the dots
#' @param high The color represents the high (positive) correlation
#' @param low The color represents the low (negative) correlation
#' @param digits The number of digits shown for the correlation values
#' @param var.position The position to put the variable names. 
#'    If the value is "axis", the variable names will be at the x and y axis. 
#'    If the value is "diagonal", the variable names will be put along the diagonal
#'    line. 
#' @param var.angle The angle of the text for the variable names on the X axis or
#'    The diagonal line. 
#' @param add.legend Whether to add the legend for color (TRUE/FALSE)
#' @return A ggplot of the correlation matrix
#' @examples
#' xx <- matrix(runif(100), 10)
#' colnames(xx) <- paste0('Variable ', 1:ncol(xx))
#' cor.mat <- cor(xx)
#' 
#' ggcorr(cor.mat)  # default plot
#' ggcorr(cor.mat, var.position = 'diagonal', high = 'blue', low = 'green',
#'   add.legend = FALSE)
#' ggcorr(cor.mat, lower = TRUE, var.position = 'diagonal')
#' 
#' @export

ggcorr <- function(cor.mat, lower = FALSE, psize = NULL, high = 'red', 
  low = 'blue', digits = 2, var.position = c('axis', 'diagonal'), 
  var.angle = 30, add.legend = TRUE) {
  
  var.position <- match.arg(var.position)
  
  if(lower == 'TRUE') {
    cor.mat[lower.tri(cor.mat)] <- NA
  }
  
  cor.df <- na.omit(melt(cor.mat))
  colnames(cor.df) <- c('x', 'y', 'corr')
  cor.df <- mutate(cor.df, x = factor(x), y = factor(y, levels = rev(unique(y)))) 
  cor.df0 <- filter(cor.df, corr != 1)
  cor.df1 <- filter(cor.df, corr == 1) 
  
  cor.df$corr[cor.df$corr == 1] <- NA
  
  if(is.null(psize)) {
    psize = 1 / max(abs(cor.df0$corr)) 
  }
  
  p <- ggplot(cor.df, aes(x, y)) + 
    geom_tile(color = 'grey90', fill = 'white') +
    geom_point(data = cor.df0, aes(fill = corr, size = abs(corr) * 10 * psize), 
      color = 'white', pch = 21, alpha = .5, show.legend = F) + 
    scale_size_identity() +
    scale_fill_gradient2(name = NULL, high = high, low = low, 
      limits = c(-1, 1), guide = FALSE) + 
    geom_text(aes(label = round(corr, digits))) +
    labs(x = NULL, y = NULL) + 
    coord_equal() + 
    theme_bw() +
    theme(
      axis.text.x = element_text(angle=var.angle, face = 'bold', 
        hjust = ifelse(var.angle == 0, .5, 1)),
      axis.text.y = element_text(face = 'bold'), 
      axis.ticks = element_blank(),
      panel.border = element_blank(), panel.grid = element_blank())
  
  if(var.position == 'diagonal') {
    p <- p + geom_text(data = cor.df1, aes(x = x, y = y, label = y),
      fontface = 2, size = 3, angle = var.angle) + 
      theme_void()
  } else {
    p <- p + geom_text(data = cor.df1, aes(x = x, y = y), label = 1)
  }
  
  if(add.legend == TRUE) {
    p <-  p + guides(fill = guide_colorbar(barwidth = add.legend, barheight = 10))
  }  
  
  suppressWarnings(print(p))
}
# xx <- matrix(runif(100), 10)
# colnames(xx) <- paste0('Variable ', 1:ncol(xx))
# cor.mat <- cor(xx)
# 
# ggcorr(cor.mat)
# ggcorr(cor.mat, var.position = 'diagonal', high = 'blue', low = 'green',
#   add.legend = FALSE)
# ggcorr(cor.mat, lower = TRUE, var.position = 'diagonal')

