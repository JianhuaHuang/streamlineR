#' Check GLM or survival model performance based on AUC
#' 
#' \code{perf.auc} checks model performance for a given model object, 
#' using the training and test dataset.  
#' 
#' @param model The GLM or survival model (coxph) object. 
#' @param train The training dataset
#' @param test The test dataset
#' @return For a GLM model, it returns the ROC curve and the AUC values for
#'    the training and test datasets respectively. For a survival model, it 
#'    returns the time-dependent AUC at each time step and the integrated 
#'    AUC value (iAUC)
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
#' perf.auc(mod, dt.train, dt.test)

perf.auc <- function(model, train, test) {
  # check the coxph or logistic (glm) model performance based on AUC
  # For coxph model, the time-dependent AUC and the iAUC (integrated AUC)
  # is calculated and plotted. For logistic (glm) model, the ROC curve is plotted
  # Args:
  #    model: the coxph or logistic model based on full dataset
  #    train: the training dataset
  #    test: the test dataset
  # Return: A plot of time-dependet AUC (for coxph), or ROC (for logistic)
  
  mod.class <- class(model)[1]  # glm model belongs to two classes, use the 1st
  if(!mod.class %in% c('coxph', 'glm')) {
    stop('the model class is not "coxph" or "glm"')
  }
  
  mod.train <- update(model, data = train)
  
  pred.train <- predict(mod.train,
    type = ifelse(mod.class == 'coxph', 'lp', 'response'))
  
  pred.test <- predict(mod.train, newdata = test,
    type = ifelse(mod.class == 'coxph', 'lp', 'response'))
  
  if(mod.class == 'coxph') {
    # use the time-dependent AUC for coxph model
    time <- all.vars(model$formula)[1]
    status <- all.vars(model$formula)[2]
    
    auc.train <- risksetAUC(Stime = train[, time], status = train[, status],
      marker = pred.train, tmax = max(train[, time]), plot = FALSE)
    
    auc.test <- risksetAUC(Stime = test[, time], status = test[, status],
      marker = pred.test, tmax = max(test[, time]), plot = FALSE)
    
    auc <- rbind(data.frame(auc.train, Data = 'Train'),
      data.frame(auc.test, Data = 'Test'))
    auc$Data <- factor(auc$Data, levels = unique(auc$Data))
    
    p <- ggplot(auc, aes(x = utimes, y = AUC, color = Data)) +
      geom_line(size = 1.5) +
      scale_size_continuous(guide = F) +
      scale_color_discrete(name = NULL,
        labels = c(paste0('Train (iAUC: ', round(auc.train$Cindex, 3), ')'),
          paste0('Test (iAUC: ', round(auc.test$Cindex, 3), ')'))) +
      theme_ws(legend.position = c(.2, .85)) +
      labs(x = 'Survival Time', y = 'AUC') +
      scale_y_continuous(limits = c(.5, 1))
  }
  
  if(mod.class == 'glm') {
    col.y <- all.vars(mod.train$formula)[1]
    
    roc.train <- roc(actual.train ~ pred.train,
      data = data.frame(actual.train = train[, col.y], pred.train))
    roc.train$auc
    
    roc.test <- roc(actual.test ~ pred.test,
      data = data.frame(actual.test = test[, col.y], pred.test))
    roc.test$auc
    
    ## ROC and AUC compare
    roc <- data.frame(
      FP = 1-roc.train$specificities,
      TP = roc.train$sensitivities,
      AUC = as.numeric(roc.train$auc),
      Data = 'Train') %>%
      rbind(data.frame(
        FP = 1-roc.test$specificities,
        TP = roc.test$sensitivities,
        AUC = as.numeric(roc.test$auc),
        Data = 'Test')) %>%
      mutate(Data = factor(Data, levels = unique(Data)))
    
    p <- ggplot(roc, aes(x = FP, y = TP, color = Data)) +
      geom_line() +
      scale_color_discrete(name = NULL, labels = c(
        paste0('Train (AUC: ', round(roc.train$auc, 3), ')'),
        paste0('Test (AUC: ', round(roc.test$auc, 3), ')'))) +
      labs(x = 'False Positive', y = 'True Positive') +
      theme_ws()
    theme_simple(legend.position = c(.2, .85))
  }
  
  print(p)
}