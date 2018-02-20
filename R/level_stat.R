#' Statistics for binary outputs in different levels and variables
#' 
#' \code{level.stat} provides the useful statistics for different levels (groups) 
#' in the given independent variables. 
#' 
#' @param data The data frame used for the statistics
#' @param x The independent variable. If it is NULL, then all variables except y 
#'    are used. 
#' @param y The binary outputs  
#' @param flag.0 The value representing 0 in y
#' @param flag.1 The value representing 1 in y
#' 
#' @return a data frame inclduing frequency, rates, Weight of Evidence (WOE), 
#'    and Information Value (IV) for each level. The return data is already
#'    ranked by the IV values from high to low. 
#'    
#' @examples
#' data <- rpart::stagec
#' level.stat(data, x = c('eet', 'ploidy'), y = 'pgstat')
#' @export

level.stat <- function(data, x = NULL, y, flag.0 = 0, flag.1 = 1) {
  # Used to calculate the rates, WOE, and IV for different variables
  # Args:
  #    data: the input data.frame
  #    x: the independent variables, can be a single variable or a vector
  #       including multiple variables
  #    y: the binary flag variable (e.g., Progression_Flag)
  #    flag.0: the value representing 0 in y
  #    flag.1: the value representing 1 in y
  # Return: a data.frame inclduing frequency, rates, WOE, and IV for each x level
  
  if(is.null(x)) x <- setdiff(colnames(data), y)
  
  rs.x <- lapply(x, function(xx) {
    # check whether Y is binary value
    if(length(unique(data[, y])) > 2) stop('Y is not binary value')
    
    dt <- data[, c(xx, y)]
    
    if(is.factor(dt[, y])) dt[, y] <- as.character(dt[, y])
    
    dt[, y] <- ifelse(dt[, y] == flag.0, 0, 1)
    
    dt.iv <- table(dt) %>%
      as.data.frame.matrix %>%
      transmute(Variable = xx,
        Group = row.names(.),
        Freq.0 = `0`,
        Freq.1 = `1`,
        Freq.group = Freq.0 + Freq.1,
        Rate.0 = Freq.0 / Freq.group,
        Rate.1 = Freq.1 / Freq.group,
        Rate.group = Freq.group / sum(Freq.group),
        Perc.0 = ifelse(Rate.0 > .01,
          paste0(round(Rate.0 * 100), '%'),
          paste0(round(Rate.0 * 100, 1), '%')),
        Perc.1 = ifelse(Rate.1 > .01,
          paste0(round(Rate.1 * 100), '%'),
          paste0(round(Rate.1 * 100, 1), '%')),
        Perc.group = ifelse(Rate.group > .01,
          paste0(round(Rate.group * 100), '%'),
          paste0(round(Rate.group * 100, 1), '%')),
        Distr.0 = Freq.0 / sum(Freq.0),
        Distr.1 = Freq.1 / sum(Freq.1),
        WOE = log(Distr.1 / Distr.0),
        WOE.round = round(WOE, 2), 
        IV = sum((Distr.1 - Distr.0) * WOE))
  })
  
  rs <- do.call(rbind, rs.x) %>%
    arrange(desc(IV)) %>%
    mutate(Variable.IV = paste0(Variable, ' (IV: ', round(IV, 3), ')'),
      Variable = factor(Variable, levels = unique(Variable)),
      Variable.IV = factor(Variable.IV, levels = unique(Variable.IV)),
      Group = factor(Group,
        levels = c(setdiff(unique(Group), 'Missing'), 'Missing')))
  return(rs)
}
