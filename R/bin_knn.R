#' Bin numerical variables based on KNN
#' 
#' The numerical independent varaible (x) is firstly divided into small buckets 
#' with approximate equal number of records. Then a univariate regression model 
#' is built using the bucketed x and dependent variable (y). The buckets with 
#' similar coefficients are classied into Visualize the binning for survival/logistic model based on model coefficients. 
#' The KNN algorithm is used to bin the small buckets into bigger groups, which 
#' takes into account both the orders and coefficients of the buckets. 
#' 
#' @param formula The formula for logistic (y ~ x) or survival model 
#'    (Surv(time, status) ~ x). 
#' @param data The data frame used for binning
#' @param n.group Number of binning groups
#' @param min.bucket The minimum proportion of population in the buckets
#'    (a value between 0 and 1)
#' @return Shows a ggplot with the regression coefficients and the binned groups
#' @examples
#' data <- rpart::stagec
#' bin.knn(pgstat ~ age, data = data, n.group = 4, min.bucket = .1)
#' # can be combine with the manipulate::manipulate function to change the 
#' # binning interactively
#' library(manipulate)
#' manipulate(bin.knn(pgstat ~ age, data = data, n.group, min.bucket),
#'   n.group = slider(1, 10, step = 1, initial = 5, label = 'Number of groups'),
#'   min.bucket = slider(0.01, 1, step = 0.01, initial = 0.05,
#'   label = 'Minimum Population Size (%)'))

bin.knn <- function(formula, data, n.group = 5, min.bucket = .05) {
  
  x <- as.character(formula[3])
  y <- as.character(formula[2])
  
  # This function is used to explore the clustering of bins based on KNN method
  cut.q <- unique(quantile(data[, x], seq(0, 1, length.out = 1 / min.bucket),
    na.rm = T))
  data[, x] <- cut(data[, x], cut.q, include.lowest = TRUE)
  data[, x] <- droplevels(data[, x])
  data[, x] <- addNA(data[, x], ifany = T)
  
  # If y is a survival object, coxph is used to estimate the coefficients
  # Otherwise (y is a binary vector), the logistic model is used
  if(grepl('^Surv', y)) {
    mod <- coxph(formula, data = data)
  } else {
    mod <- glm(formula, data = data, family=binomial(link='logit'))
  }
  
  # construct the coef and population dataframe
  x.pop <- data.frame(Variable = x, data.frame(table(data[, x])))
  colnames(x.pop) <- c('Variable', 'Bin', 'Freq')
  
  coef <- data.frame(Variable = x,
    Bin = gsub(x, '', names(mod$coefficients)),
    Coef = mod$coefficients)
  
  coef.pop <- merge(x.pop, coef, all.x = T)
  coef.pop$Coef[1] <- 0  # the first value is the reference, its coef is zero
  
  # knn groups based on the order and coefficients
  x.knn <- data.frame(Ord = 1:nrow(coef.pop), Coef = coef.pop$Coef)
  
  coef.pop$KNN_Group <- as.factor(
    kmeans(scale(x.knn), n.group, nstart = 20)$cluster)
  
  ggplot(coef.pop, aes(x = Bin, y = Coef, fill = KNN_Group,
    color = KNN_Group, width = 3 * Freq / nrow(data))) +
    geom_bar(stat = 'identity') +
    labs(x = 'Buckets', y = 'Model Coefficients',
      title = paste(x, ':', nrow(coef.pop), 'buckets clustered into',
        n.group, 'groups')) +
    theme_bw() +
    scale_fill_discrete(name = 'KNN Group') +
    scale_color_discrete(name = 'KNN Group') +
    theme(axis.text.x = element_text(angle=30, hjust=1))
}
