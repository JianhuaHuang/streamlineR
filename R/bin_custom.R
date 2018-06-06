#' Generate bins based on given cut points
#' 
#' bin.custom() cuts numerical data into different bins based on the given cut 
#' points. It also gives you the flexibility of naming the bins. 
#' 
#' @param x The numerical data needs to be cut
#' @param cut.p The cut poins used for the cutting
#' @param names The names for the corresponding bins The poit size for the dots
#' @param name.style Either "report" or "scientific" style. The "report" style is 
#'    easier easier to understand for the non-technical audience. While the 
#'    "scientific" style is more succinct. 
#' @return Bins the original values fall within
#' @examples
#' bin.custom(1:100, cut.p = c(20, 50, 80))
#' @export

bin.custom <- function(x, cut.p, names = NULL,
  name.style = c('report', 'scientific')) {
  # This function is used to bin the data with given cut points
  # Args:
  #    x: a single column of data to be cut
  #    cut.p: the given cut points.
  #    names: the names given to the bins after cutting. If no names is
  #               given, the default name.style is used
  #    name.style: the naming style used, can be either
  #                'scientific' (e.g., (1, 2]) or 'report' (e.g., 1 < · <= 2)
  # Return:
  #    A vector of binned values
  
  x.num <- as.numeric(x)
  cut.p.num <- as.numeric(cut.p)
  
  # add ends to the cut.p
  cut.p_ends <- unique(c(-Inf, cut.p.num, max(x.num, na.rm = T)))
  x.bins <- cut(x.num, cut.p_ends, include.lowest = T)
  
  # change the levels from -Inf to the actual minimum value
  if(is.null(names)) {
    
    name.style = match.arg(name.style)
    
    if(name.style == 'scientific') {
      levels(x.bins) <- gsub(paste0('[-Inf,', cut.p[1], ']'),
        paste0('[', min(x, na.rm = T), ',', cut.p[1], ']'),
        levels(x.bins), fixed = T)
    }
    
    if(name.style == 'report') {
      bin.names <- c(paste('<=', cut.p[1]), rep(NA, length(cut.p) - 1),
        paste('>', cut.p[length(cut.p)]))
      
      if(length(cut.p) > 1) {
        for (i in 2:length(cut.p)) {
          bin.names[i] <- paste(cut.p[i - 1], '< \u00B7 <=', cut.p[i])
        }}  # \u00B7 is the unicode for mid-dot, \u2022 is for bullet point
      levels(x.bins) <- bin.names[1:nlevels(x.bins)]
    }
      x.bins <- droplevels(x.bins)  # some bins without values, remove it
  }
  # if there is only one unique value in the level, change the x.bins to
  # that value
  x.bin.unique <- data.frame(x, x.bins) %>%
    filter(!is.na(x)) %>%
    group_by(x.bins) %>%
    summarise(unique.x = length(unique(x)), min.x = min(x))
  
  # levels(x.bins) <- ifelse(x.bin.unique$unique.x == 1, x.bin.unique$min.x,
  #  levels(x.bins))
  
  if(!is.null(names)) levels(x.bins) <- names # coerce to change levels
  
  # preserve the levels order
  lvs <- levels(x.bins)
  x.bins <- as.character(x.bins)
  x.bins[is.na(x.bins)] <- 'Missing'
  x.bins <- factor(x.bins, levels = c(lvs, 'Missing'))
  x.bins <- droplevels(x.bins)
  return(x.bins)
}
# df <- data.frame(x = 1:100, y = sample(c(1, 0), 100, replace = T))
# df <- data.frame(x = 1:100, y = rep(c(1, 0), each = 50))
# bin.custom(x = c(1:50, NA, NA, 51:100), cut.p = c(1, 2, 99), name.style = 'report')
# rs <- bin.custom(dt$Grad_Year_Month, as.yearmon(c('Aug 2012', 'Aug 2013')))
