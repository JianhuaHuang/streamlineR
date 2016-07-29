test <- dt.train[, col.x]
cor.mat <- cor(test)

ggcorr <- function(cor.mat, lower = FALSE, psize = NULL, color.high = 'red', 
  color.low = 'blue', digit = 2, var.position = c('axis', 'diagonal'), 
  var.angle = 30, add.legend = TRUE) {
  
  var.position <- var.position[1]
  
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
    scale_fill_gradient2(name = NULL, high = color.high, low = color.low, 
      limits = c(-1, 1), guide = FALSE) + 
    geom_text(aes(label = round(corr, digit))) +
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


# aa <- matrix(runif(100), 10)
# colnames(aa) <- paste0('Variable ', 1:ncol(aa))
# cor.mat <- cor(aa)
# ggcorr(cor(aa), lower = FALSE, color.high = 'blue', color.low = 'green', var.angle = 20,
#   add.legend = F)

ggcorr(cor.mat, lower = FALSE, var.position = 'diagonal', color.high = 'blue', 
  color.low = 'green')
