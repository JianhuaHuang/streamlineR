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

############################## Binning and WOE #################################
bin.custom <- function(x, cut.p, names = NULL,
  name.style = c('report', 'scientific'), ...) {
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
  cut.p_ends <- c(-Inf, cut.p.num, max(x.num, na.rm = T))
  x.bins <- cut(x.num, cut.p_ends, include.lowest = T)
  
  # change the levels from -Inf to the actual minimum value
  if(is.null(names)) {
    
    name.style = name.style[1]
    
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
      levels(x.bins) <- bin.names
    }
  }
  # if there is only one unique value in the level, change the x.bins to
  # that value
  x.bin.unique <- data.frame(x, x.bins) %>%
    filter(!is.na(x)) %>%
    group_by(x.bins) %>%
    summarise(unique.x = length(unique(x)), min.x = min(x))
  
  levels(x.bins) <- ifelse(x.bin.unique$unique.x == 1, x.bin.unique$min.x,
    levels(x.bins))
  
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

## bin.knn
bin.knn <- function(formula, data, n.group, min.pop) {
  # Visualize the binning for survival/logistic model based on model coefficients
  # Can be combined with the manipulate function to check the binning of 
  # coefficients interactively
  # Args:
  #    formula: the model used for binning (can be glm or coxph)
  #    data: the data frame used for binning
  #    n.group: number of binned groups
  #    min.pop: the minimum ratio of population in each group (can be 
  #             a value between 0 to 1)
  # Return:
  #    Shows a ggplot with the regression coefficients and the binned groups
  
  x <- as.character(formula[3])
  y <- as.character(formula[2])
  
  # This function is used to explore the clustering of bins based on KNN method
  cut.q <- unique(quantile(data[, x], seq(0, 1, length.out = 1 / min.pop),
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
    labs(x = 'Bins', y = 'Model Coefficients',
      title = paste(x, ':', nrow(coef.pop), 'coefficients clustered into',
        n.group, 'groups')) +
    theme_bw() +
    scale_fill_discrete(name = 'KNN Group') +
    scale_color_discrete(name = 'KNN Group') +
    theme(axis.text.x = element_text(angle=30, hjust=1))
}
# manipulate(bin.knn(status ~ platelet, data = dt.train, n.group, min.pop),
#   n.group = slider(1, 10, step = 1, initial = 5, label = 'Number of groups'),
#   min.pop = slider(0.01, 1, step = 0.01, initial = 0.05,
#     label = 'Minimum Population Size (%)'))

## rpart.bin
bin.rpart <- function(formula, data, rcontrol = rpart.control(), n.group = NULL, ...) {
  # This function is used to bin the numerical variable for survival model
  # Arg:
  #    formula: the formula used for rpart
  #    data: the dataset used for rpart
  #    n.group: the acceptable number of groups (NA group not counted for)
  #    rcontrol: the control used for rpart
  
  # The NA values are removed by the rpart function automatically
  row.names(data) <- 1:nrow(data)
  
  vars <- all.vars(formula)
  x.num <- vars[length(vars)]
  
  # if the minbucket is the default value 7, then update it to 1% of the data
  # if(rcontrol$minbucket == 7) {
  #   rcontrol$minbucket <- .01 * nrow(data)
  # }
  
  rp.tree <- rpart(formula, data, control = rcontrol, ...)
  # rp.tree <- rpart(formula, data, control = rcontrol)
  
  ## if n.group is NULL, and no group is found, return 'No Bin'
  if(is.null(n.group) & length(unique(rp.tree$where)) == 1) {
    cat(c(x.num, ': No Bin \n'))
    return('No Bin')
  }
  
  ## if n.group is not NULL, change cp to find the possible bins within n.group
  while((!is.null(n.group)) & (!length(unique(rp.tree$where)) %in% n.group)) {
    multipler <- ifelse(length(unique(rp.tree$where)) > median(n.group), 1.1, .9)
    rcontrol$cp <- rcontrol$cp * multipler
    rp.tree <- rpart(formula, data, control = rcontrol, ...)
    # rp.tree <- rpart(formula, data, control = rcontrol)
  }
  
  tree.where <- data.frame(Where = rp.tree$where)
  
  tree.value <- data.frame(Value = data[, x.num], Where = 'Missing',
    stringsAsFactors = F)
  tree.value[row.names(tree.where), 'Where'] <- tree.where$Where
  
  tree.cut <- group_by(tree.value, Where) %>%
    summarise(Cut_Start = min(Value), Cut_End = max(Value)) %>%
    arrange(Cut_End)
  
  if(is.na(tree.cut$Cut_End[nrow(tree.cut)])) {
    cut.p <- tree.cut$Cut_End[1:(nrow(tree.cut) - 2)]
  } else {
    cut.p <- tree.cut$Cut_End[1:(nrow(tree.cut) - 1)]
  }
  
  cat(c(x.num, ':', cut.p, '\n'))
  
  bin.names <- c(paste('<=', cut.p[1]), rep(NA, length(cut.p) - 1),
    paste('>', cut.p[length(cut.p)]))
  
  if(length(cut.p) > 1) {
    for (i in 2:length(cut.p)) {
      bin.names[i] <- paste(cut.p[i - 1], '< \u00B7 <=', cut.p[i])
    }}  # \u00B7 is the unicode for mid-dot, \u2022 is for bullet point
  
  # check whether the Cut_Start and Cut_End are the same
  # if the same, the <, =, or > signs is not needed
  bin.names <- ifelse(tree.cut$Cut_Start[1:length(bin.names)] ==
      tree.cut$Cut_End[1:length(bin.names)], tree.cut$Cut_End, bin.names)
  
  tree.cut$Bin <- 'Missing'
  tree.cut$Bin[1:length(bin.names)] <- bin.names
  
  x.bins <- factor(tree.cut$Bin[match(tree.value$Where, tree.cut$Where)],
    levels = tree.cut$Bin)
  
  return(list(cut.points = cut.p, bins = x.bins))
}
# dt <- read.csv('C:/Projects/AlumniConvProg/data/Data_Associate.csv')
# bin.rpart(formula = Surv(Conversion_Time_Months, Conversion_Status) ~ WF_Count,
#   rcontrol = rpart.control(cp = 0.0001, minbucket = nrow(dt.conv) * .01),
#   data = dt.conv, n.group = 3:7)

## Rates by level
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
    if(length(unique(data[, y])) != 2) stop('Y is not binary value')
    
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

## ggplot the level.stat output by variable and level
ggstat <- function(data, var = 'Variable.IV', x = 'Group', y = 'Rate.1',
  y.label = 'Perc.1', y.min.0 = FALSE, y.title = NULL, bar.width = 'Rate.group',
  bar.width.label = 'Perc.group', n.col = NULL) {
  # Plot the stat (statistics, e.g., Rate.1, Rate.0, and WOE) for each varaible
  # Args:
  #    data: the input dataset
  #    var: the varaibles plotted in different panel
  #    x: the variable used as x axis
  #    y: the varaible used as y axis
  #    y.label: the text used to label the y value
  #    y.min.0: whether to plot the bar from 0
  #    y.title: the title for y axis
  #    bar.width: the column used to represent bar width. If NULL, the bar width
  #               is set to 0.1
  #    bar.width.label: the column used to label the bar.width. If NULL, it is
  #                     not labelled.
  #    n.col: number of panel column
  # Return: ggplot
  
  data$var = data[, var]
  data$x = data[, x]
  data$y = data[, y]
  
  if(is.null(y.label)) {
    data$y.label = ''
  } else {
    data$y.label = data[, y.label]
  }
  
  if(is.null(bar.width)) {
    data$bar.width = 0
  } else {
    data$bar.width = data[, bar.width]
  }
  
  if(is.null(bar.width.label)) {
    data$bar.width.label = ''
  } else {
    data$bar.width.label = data[, bar.width.label]
  }
  
  if(is.null(n.col)) n.col <- ceiling(sqrt(length(unique(data$var))))
  
  y.range <- max(data$y) - ifelse(y.min.0 == TRUE, 0, min(data$y))
  y.min <- ifelse(y.min.0 == TRUE, 0, min(data$y) - y.range * .15)
  y.max <- (max(data$y)) + y.range * .1
  
  ggplot(data, aes(x = x, y = y)) +
    facet_wrap(~ var, scale = 'free', ncol = n.col) +
    geom_bar(aes(width = bar.width + .1), stat = 'identity',
      fill = 'cornflowerblue', color = 'cornflowerblue') +
    # geom_text(aes(y = y, label = y.label), size = 3, color = 'red',
    #   vjust = ifelse(data$y > 0, -.25, .85)) +
    geom_text(aes(y = y, label = y.label), size = 3, color = 'red',
      nudge_y = ifelse(data$y > 0, .06, -.06) * y.range) +
    geom_text(aes(y = y.min, label = bar.width.label), size = 3, vjust = 0) +
    labs(x = NULL, y = y.title) +
    scale_y_continuous(limits = c(y.min, y.max), oob = rescale_none) +
    theme_classic() +
    theme(axis.text.x = element_text(angle=25, hjust=1),
      axis.line.x = element_line(), axis.line.y = element_line(),
      axis.title = element_text(size=12,face="bold"),
      text = element_text(size = 10), strip.text = element_text(face = 'bold'),
      strip.background = element_blank())
}
# ggstat(lvs, y = 'Rate.1', y.label = 'Perc.1', bar.width = NULL)
# ggstat(lvs, y = 'WOE', y.label = NULL)
# ggstat(level.stat.prog, y.title = 'Progression Rate (%)')

## replace the categorical levels with their corresponding woe values
replace.woe <- function(data, level.stat.output, replace = FALSE) {
  # This function is used to replace the categorical variables with the
  # corresponding woe value claculated with level.stat function
  # Args:
  #    data: the input data.frame
  #    level.stat.output: the data.frame output from the level.stat function
  #    replace: FALSE/TRUE. If FALSE, the woe values will be added to the end.
  #             if TRUE, the original levels will be replaced by woe values
  # Return: a data.frame with woe
  
  dt.rp <- data[, intersect(colnames(data), level.stat.output$Variable)]
  dt.rp.woe <- lapply(colnames(dt.rp), function(x) {
    woe.x <- level.stat.output[level.stat.output$Variable == x, ]
    group.match <- match(dt.rp[, x], woe.x$Group)
    woe.x$WOE[group.match]
  })
  
  dt.rp.woe <- do.call(cbind, dt.rp.woe)
  
  if(replace == FALSE) {
    colnames(dt.rp.woe) <- paste0(colnames(dt.rp), '_woe')
    return(data.frame(data, dt.rp.woe))
  } else {
    data[, colnames(dt.rp)] <- dt.rp.woe
    return(data)
  }
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


## check model performance: perf.auc & perf.decile
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
# perf.auc(model = cox.conv.aic, train = dt.conv.tr, test = dt.conv.test)
# perf.auc(model = lg.prog.woe.aic, train = dt.train, test = dt.test)

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
coef2rate <- function(data, model, level.stat.output, force.change = TRUE,
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
