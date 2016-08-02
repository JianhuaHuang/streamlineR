#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

############################# little functions #################################
## add.row
add.row <- function(df, new.row) {
  # This function is used to add a new row to the end of a data.frame.
  # Each element of the new row has the same class as the column it belongs to
  # However, factor will be convert to character firstly
  # Args:
  #   df: The data.frame to be added to
  #   new.row: a vector of the new values added to the end of df.
  #            The length of the new.row should be equal to number of df columns
  # Return:
  #   df with new row added to the end. The classes of each column is preserved
  f <- sapply(df, is.factor)
  df[f] <- lapply(df[f], as.character)
  
  df.class <- lapply(df, class)
  new.row <- as.list(new.row)
  if(length(df.class) != length(new.row)) {
    stop('Length does not match number of columns')
  }
  
  new.row.list <- lapply(1:length(new.row), function(x) {
    new.class <- as(new.row[[x]], df.class[[x]])
  })
  
  df[nrow(df) + 1, ] <- new.row.list
  
  return(df)
}
# df <- data.frame(x = c('A', 'B', 'C'), y = 1:3, z = 4:6, stringsAsFactors = F)
# df <- data.frame(x = c('A', 'B', 'C'), y = 1:3, z = 4:6)
# new.row <- c('Total', colSums(df[, -1]))
# add.row(df = df, new.row = new.row)

## paste.mat
# paste elements to a matrix, and return a matrix with the same dims
paste.mat <- function(x, ...) {
  # This function is used to paste matrix/data.frame, and return a matrix with
  # the same dim as x.
  # Arg:
  #   x: the first matrix/data.frame, which is used to determine the dimension
  #   ...: any other elements that are being pasted to the x, it can be a
  #        single value, a vector, and a matrix. (data.frame must be converted
  #        to matrix before being passed into the function)
  # Return:
  #   a matrix with the same dimension as x, column name reserved.
  if(is.data.frame(x)) x <- as.matrix(x)
  nrow.x <- nrow(x)
  value <- paste0(x, ...)
  out.mat <- matrix(value, nrow = nrow.x)
  colnames(out.mat) <- colnames(x)
  return(out.mat)
}
# df <- data.frame(a=1:3, b = 4:6, c = 7:9)
# paste.mat(df, '(', as.matrix(df), ')')
# paste.mat(df, '_', 11:19)

# ifelse prevent data type change
ifelse.safe <- function(cond, yes, no) {
  # The ifelse function may change the returned data format (e.g., date -> num)
  # this simple function can prevent the change
  # reference to:
  # http://stackoverflow.com/questions/6668963/how-to-prevent-ifelse-from-turning-
  # date-objects-into-numeric-objects
  structure(ifelse(cond, yes, no), class = class(no))
}



## KM curve
km.curve <- function(data, time, status, x, plot = TRUE) {
  dt.km.obo <- lapply(x, function(xx) {
    sf.x <- survfit(as.formula(paste0('Surv(', time, ',', status, ') ~', xx)),
      data = data)
    df.x <- data.frame(time = sf.x$time, surv = sf.x$surv, variable = xx,
      group = rep(gsub(paste0(xx, '='), '', names(sf.x$strata)), sf.x$strata))
  })
  
  dt.km.obo <- do.call(rbind, dt.km.obo)
  
  out <- by(data = dt.km.obo, INDICES = dt.km.obo$variable, FUN = function(m) {
    m <- droplevels(m)
    m <- ggplot(m, aes(x = time, y = surv, color = group)) +
      geom_line() +
      scale_color_discrete(name = '') +
      labs(x = NULL, y = NULL) +
      facet_wrap(~variable, nrow = 4, scales = 'free') +
      theme_simple(plot.margin=unit(c(.5, 0, .5, .5), "cm"))
  })
  
  if(plot == TRUE) do.call(grid.arrange, out)
  return(dt.km.obo)
}
# sf <- km.curve(data = dt.conv, time = 'Conversion_Time_Months',
#   status = 'Conversion_Status', x = col.x[1:6], plot = TRUE)

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

## calculate the survival probability at each time for each record one by one
survexp.obo <- function(data, ratetable, ...) {
  # calculate the survival table for each record one by one
  # Args:
  #    the same as the survexp model, except that the formula is not required
  # Return:
  #    suvival function at each time step, for each record
  
  id = split(1:nrow(data), cut(1:nrow(data), ceiling(nrow(data) / 5000)))
  
  pred.obo <- lapply(id, function(x) {
    pred <- survexp(~ ID, ratetable = ratetable,
      data = data.frame(data[x, ], ID = x), ...)
    t(pred$surv)
  })
  
  pred.all <- do.call(rbind, pred.obo)
}
# rs <- survexp.obo(data = dt.conv.test, ratetable = cox.conv.train)

## conver the regression coefficients to meaningful rates
coef2rate <- function(model, data, level.stat.output, force.change = TRUE,
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
  if(all(pred.xs$Group %in% level.stat.output$WOE)) {
    pred.xs <- rename(pred.xs, WOE = Group)
  }
  
  # in case the _woe columns are used for model, remove the _woe to match
  # with the original variable names
  pred.xs$Variable <- gsub('_woe', '', pred.xs$Variable)
  
  pred.stat <- left_join(level.stat.output, pred.xs) %>%
    filter(!is.na(Pred.Rate.1)) %>%
    mutate(Variable = factor(Variable, levels = unique(Variable)),
      Group = factor(Group, levels = unique(Group)),
      Variable.IV = factor(Variable.IV, levels = unique(Variable.IV)),
      Group = factor(Group,
        levels = c(setdiff(unique(Group), 'Missing'), 'Missing'))) %>%
    data.frame
  
  return(pred.stat)
}
# conv.lvs <- level.stat(data = dt.conv, x = col.x, y = 'Conversion_Status_1yr')
# conv.pred.stat <- coef2rate(data = dt.conv, model = cox.aic,
#   level.stat.output = conv.lvs, time = 12, force.change = TRUE)

################################## plot ########################################
## ggplot themes
# a classic simple theme with x and y axis
theme_simple <- function(...) {
  theme_bw() +
    theme(axis.line.x = element_line(size = .5),
      axis.line.y = element_line(size = .5),
      strip.text = element_text(face = 'bold'),
      panel.border = element_blank(),
      strip.background = element_blank(),
      axis.title = element_text(face = 'bold'),
      axis.title.y=element_text(margin=margin(0,10,0,0)),
      legend.title = element_text(face = 'bold'),
      panel.grid.major = element_line(linetype = 'dashed'),
      panel.grid.minor = element_blank(),
      ...)
}

# a ggplot theme similar to the figures published on the Wall Street Journal
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
# rr <- ggplot(mtcars) + geom_point(aes(x = wt, y = mpg, colour=factor(gear)))
# rr + theme_ws()
# rr + facet_wrap(~carb, ncol = 2, scale = 'free')
# rr + theme_simple() + scale_y_continuous(limits = c(0, 35))

display.col <- function() {
  # Display all possible colors in ggplot2
  # reference to http://sape.inf.usi.ch/quick-reference/ggplot2/colour
  d = grep('gray|grey.*[1-9]$|grey100', colors(), value = T, invert = T) %>%
    matrix(ncol = 9) %>%
    melt
  colnames(d) <- c('y', 'x', 'c')
  d$y = max(d$y) - d$y
  
  ggplot(data = d) +
    scale_x_continuous(name = '', breaks = NULL, expand = c(0, 0)) +
    scale_y_continuous(name = '', breaks = NULL, expand = c(0, 0)) +
    scale_fill_identity() +
    geom_rect(aes(
      xmin = x, xmax = x + 1, ymin = y, ymax = y + 1), fill = "white") +
    geom_rect(aes(
      xmin = x + 0.05, xmax = x + 0.95, ymin = y + 0.5, ymax = y + 1, fill=c)) +
    geom_text(aes(
      x = x + 0.5, y = y, label = c), nudge_y = .55, color = 'black', size=3) +
    theme_ws()
}

display.pch <- function() {
  # Display all possible pch (point shape) used for ggplot
  # reference to http://sape.inf.usi.ch/quick-reference/ggplot2/shape
  d=data.frame(p=c(0:25, 32:127))
  ggplot(data=d) +
    scale_y_continuous(name = '', breaks = NULL) +
    scale_x_continuous(name = '', breaks = NULL) +
    scale_shape_identity() +
    geom_point(aes(x=p%%16, y=7 - p%/%16, shape=p), size=5, fill="red") +
    geom_text(aes(x=p%%16, y=7 - p%/%16+0.25, label=p), size=3) +
    theme_ws()
}

## beatufied correlation plot
corrplot.beautify <- function(cor.mat) {
  # The layout and fonts of the default corrplot output doesn't look good
  # this function is used to beautify the corrplot
  # Arg:
  #    cor.mat: correlation matrix generated by cor() function
  par(cex = .8)
  corrplot(round(cor.mat, 2), type = 'lower', tl.srt = 15, addCoef.col = "black",
    cl.cex = 1.5, tl.cex = 1.2, tl.col = 'black', mar = c(0, 0, 0, 0),
    col=colorRampPalette(c("blue","white","red"))(200))
  par(cex = 1)
}
