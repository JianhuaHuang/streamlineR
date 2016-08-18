#' Convert coefficient to rates
#' 
#' \code{coef2rate} is designed to convert regression
#'  coefficients back to the good/bad rates for each group and variables, 
#'  so that the non-technical audience can understand it easily. 
#' The coef2rate function works in two different ways dependent on whether 
#' force.change is FALSE or TRUE.
#' 
#' If force.change is set to FALSE, the function will estimate the predicted 
#' value for each record of the given data using the given model. Then, average 
#' the predicted value for each group and variable.
#' 
#' If the force.change is set to TRUE, the function will go through each 
#' predictor, force the value in this predictor to be one of its group and keep 
#' all other predictors unchanged, and then calculate the predicted value for 
#' each record. By averaging the values for all record, we get a single 
#' Pred.Rate.1 for the given group and predictor. We can get the average 
#' predicted value for all groups and predictors, by going through them one by one.
#' The idea behind force.change originates from the interpretation of regression 
#' coefficients - keep all other variables unchanged, and only change the value 
#' for one predictor. By doing this, we can get the pure effect of that variable.
#'  
#' @param model The GLM/survival (coxph) model object 
#' @param data The data used to calculate the rates
#' @param stat The statistics output from the \code{level.stat} function. 
#'    The group/WOE is used to link the predicted rates back to the original
#'    values. 
#' @param force.change Whether to force the model to use all data, and change the
#'    value in each variable to be one of its levels. 
#' @param time The length of time to predict. It is only required in survival
#'    model. 
#' 
#' @return The predicted rates for each group and variable, together with the 
#'    frequency of records in each group, and the information value passed to the 
#'    stat argument. 
#'    
#' @examples
#' data <- rpart::stagec
#' data <- na.omit(data)
#' mod <- glm(pgstat ~ eet + grade + ploidy, data, family=binomial(link='logit'))
#' st <- level.stat(data, y = 'pgstat')
#' coef2rate(mod, data, st)
#' @export

coef2rate <- function(model, data, stat, force.change = TRUE,
  time = NULL) {
  xs <- labels(model$terms)
  
  pred.x.list <- lapply(xs, function(x) {
    
    if(force.change == TRUE) {
      groups = unique(data[, x])
      pred <- sapply(groups, function(y) {
        dt.temp <- data
        dt.temp[x] <- y
        if(class(model)[1] == 'coxph') {
          if(is.null(time)) stop('Prediction time is needed for coxph model')
          pred <- survexp(formula = ~ 1, data = dt.temp, ratetable = model,
            times = time)
          return(1 - pred$surv)
        } else {
          pred <- predict(model, newdata = dt.temp, type = 'response')
          return(mean(pred))
        }
      })
      pred.x <- data.frame(Variable = x, Group = groups, Pred.Rate.1 = pred)
    }
    
    if(force.change == FALSE) {
      if(class(model)[1] == 'coxph') {
        if(is.null(time)) stop('Prediction time is needed for coxph model')
        
        pred <- survexp(formula = as.formula(paste('~', x)), data = data,
          ratetable = model, times = time)
        
        pred.x <- data.frame(Variable = x,
          Group = gsub(paste0(x, '='), '', names(pred$surv)),
          Pred.Rate.1 = 1 - pred$surv)
        # the result is exactly the same as prediction using surexp.obo
        # then calculate the mean value for each group
        # pred.obo <- survexp.obo(data = data, ratetable = model)
        # pred.12 <- data.frame(College = data$College, Pred = pred.obo[, 12])
        # group_by(pred.12, College) %>% summarise(P = 1 - mean(Pred))
      } else {
        pred <- predict(model, newdata = data, type = 'response')
        pred.x <- data.frame(
          Variable = x, Group = data[, x], Pred.Rate.1 = pred) %>%
          group_by(Variable, Group) %>%
          summarise(Pred.Rate.1 = mean(Pred.Rate.1))
      }
    }
    return(pred.x)
  })
  
  pred.xs <- do.call(rbind, pred.x.list)
  
  # calculate population
  freq <- melt(data[, xs], id.vars = NULL) %>%
    group_by(Variable = variable, Group = value) %>%
    summarise(Freq.group = n()) %>%
    mutate(
      Rate.group = Freq.group / nrow(data), 
      Perc.group = ifelse(Rate.group > .01, 
        paste0(round(Rate.group * 100), '%'),
        paste0(round(Rate.group * 100, 1), '%')))
  
  # check whether WOE is used for modeling  
  if(all(pred.xs$Group %in% stat$WOE)) {
    pred.xs <- rename(pred.xs, WOE = Group)
  }
  
  # in case the _woe columns are used for model, remove the _woe to match
  # with the original variable names
  pred.xs$Variable <- gsub('_woe', '', pred.xs$Variable)
  
  pred.stat <- left_join(stat, pred.xs) %>% 
    left_join(freq) %>%
    filter(!is.na(Pred.Rate.1)) %>%
    mutate(Variable = factor(Variable, levels = unique(Variable)),
      Group = factor(Group, levels = unique(Group)),
      Variable.IV = factor(Variable.IV, levels = unique(Variable.IV)),
      Group = factor(Group,
        levels = c(setdiff(unique(Group), 'Missing'), 'Missing')), 
      Pred.Perc.1 = ifelse(Pred.Rate.1 > .01,
        paste0(round(Pred.Rate.1 * 100), '%'),
        paste0(round(Pred.Rate.1 * 100, 1), '%'))) %>%
    select(Variable, Variable.IV, Group, Freq.group, Rate.group, Perc.group,
      Pred.Rate.1, Pred.Perc.1) %>% 
    data.frame
  
  return(pred.stat)
}

# data <- rpart::stagec
# data <- na.omit(data)
# mod <- glm(pgstat ~ eet + grade + ploidy, data, family=binomial(link='logit'))
# st <- level.stat(data, y = 'pgstat')
# rs <- coef2rate(mod, data, st)
# ggstat(rs, y = 'Pred.Rate.1', y.label = 'Pred.Perc.1')
