#' Bin numerical or factor values based on recursive partitioning (rpart)
#' 
#' \code{bin.rpart} relies on \code{\link[rpart]{rpart}}
#' function to split numerical or factor values into different nodes. According to the 
#' tree-structure splits generated form \code{rpart}, \code{bin.rpart} further 
#' divides the values into different bins. 
#' The usage of \code{bin.rpart} is similar to \code{rpart}, except that the 
#' argument of \emph{control} in \code{rpart} is named as \emph{rcontrol} in 
#' \code{bin.rpart}
#' 
#' @param formula The formula for rpart
#' @param data The data frame used for binning
#' @param rcontrol The arguments passed into \code{\link[rpart]{rpart.control}} 
#' @param n.group Number of acceptable binning groups. It can be NULL,
#'    a single number (e.g., 5), or a vector (e.g., 3:7). If the value is NULL, it 
#'    returns the output with the default \emph{rpart.control}
#'    settings. If the n.group is a numeric value, it will change the \emph{cp} 
#'    value within \emph{rpart.control} automatically, 
#'    until it gets the desirable number of groups 
#' @param ... All other arguments that can be passed to \code{rpart} 
#' 
#' @return The cut points (\emph{cut.points}) and \emph{bins}. 
#' @examples
#' data <- rpart::stagec
#' bin.rpart(pgstat ~ age, data = data)
#' bin.rpart(pgstat ~ ploidy, data = data)
#' @export

bin.rpart <- function(formula, data, rcontrol = rpart.control(), n.group = NULL, 
  ...) {
  # This function is used to bin the numerical or factor variable for survival model
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
  
  if (class(data[, x.num])=="factor") {
    # If the values are factored
    cut.p <- unique(tree.where$Where)

    # Create group names
    bin.names <- rep(NA, length(cut.p))
    for (i in 1:length(cut.p)) {
      bin.names[i] <- paste("group",i)
    }

    # Map groups to factors
    x.bins <- factor(bin.names[match(tree.where$Where, cut.p)], levels = bin.names)

    # Build group names
    tree.cut <- dplyr::group_by(tree.value, Where) %>% summarise(group=paste(unique(Value), collapse=","))
    cut.p <- tree.cut$group

  } else {
    tree.cut <- dplyr::group_by(tree.value, Where) %>%
      dplyr::summarise(Cut_Start = min(Value), Cut_End = max(Value)) %>%
      dplyr::arrange(Cut_End)
  
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
  }
  
  return(list(cut.points = cut.p, bins = x.bins))
}
# dt <- read.csv('C:/Projects/AlumniConvProg/data/Data_Associate.csv')
# bin.rpart(formula = Surv(Conversion_Time_Months, Conversion_Status) ~ WF_Count,
#   rcontrol = rpart.control(cp = 0.0001, minbucket = nrow(dt.conv) * .01),
#   data = dt.conv, n.group = 3:7)
