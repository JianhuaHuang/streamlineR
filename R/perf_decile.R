#' Check model performance based on predicated and actual rates
#' 
#' \code{perf.decile} takes the actual status (actual), and the predicted 
#' probability (pred) as inputs, divided the data into 10 decile dependent on 
#' the ranking of the predicted values, and calculate the average predicted and
#' actual rates in each decile. 
#' 
#' @param actual A vector containing the actual status for each record
#' @param pred The predicted probability for each record
#' @param plot Whether to show the ggplot figure  
#' @param add.legent Whether to add a legend for the decile color
#' @return The predicted and actual rates in each decile and a ggplot
#'    
#' @examples
#' data <- rpart::stagec
#' data <- data[sample(nrow(data), 10000, replace = TRUE), ]
#' data <- na.omit(data)
#' 
#' ind.train <- caret::createDataPartition(data$pgstat, p = .7, list = FALSE)
#' dt.train <- data[ind.train, ]
#' dt.test <- data[-ind.train, ]
#' mod <- glm(pgstat ~ ., dt.train, family=binomial(link='logit'))
#' 
#' pred.test <- predict(mod, newdata = dt.test, type = 'response')
#' perf.decile(actual = dt.test$pgstat, pred = pred.test)

perf.decile <- function(actual, pred, plot = TRUE, add.legend = TRUE) {
  # check the model performance based on actual and predicted rates
  # Args:
  #    actual: a vector containing the actual status for each record
  #    pred: predicted probability for each record
  # Return:
  #    plot the figure of model performance by decile
  #    return the actual and predicted rates for each decile
  rate <- data.frame(Actual = actual , Predict = pred) %>%
    arrange(Predict) %>%
    mutate(Decile = rep(1:10, table(cut(1:nrow(.), 10)))) %>%
    group_by(Decile) %>%
    summarise(Actual.rate = mean(Actual)* 100, Predict.rate = mean(Predict) * 100,
      Freq.1 = sum(Actual), Freq.0 = n() - Freq.1,  Freq.group = n())
  
  min.xy <- min(rate[, c('Predict.rate', 'Actual.rate')])
  max.xy <- max(rate[, c('Predict.rate', 'Actual.rate')])
  
  p <- ggplot(rate, aes(x = Actual.rate, y = Predict.rate)) +
    geom_point(aes(color = as.factor(Decile)), size = 4, show.legend = add.legend) +
    geom_abline(slope = 1, linetype = 2) +
    coord_equal() +
    scale_x_continuous(limits = c(min.xy, max.xy)) +
    scale_y_continuous(limits = c(min.xy, max.xy)) +
    scale_color_brewer(name = 'Decile', palette = 'RdYlBu', direction = -1) +
    labs(x = 'Actual Rate (%)', y = 'Predicted Rate (%)') +
    guides(color = guide_legend(reverse = T, override.aes = list(size = 5))) +
    theme_bw()
  
  if(plot == TRUE) print(p)
  return(rate)
}
# perf.decile(actual = dt$Progression, pred = mod$fitted.values)
