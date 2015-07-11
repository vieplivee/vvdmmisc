#' split a data frame into training and testing sets
#'
#' @param dataframe a data frame
#' @param seed (optional) an integer - seeding the split. default = 10001
#' @param ratio (optional) an integer - 1/ratio of total set is designated to training.
#'   default = 2
#' @return a list of a trainset and a testset

splitdf <- function(dataframe, seed = 10001, ratio = 2) {
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/ratio))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset = trainset, testset = testset)
}

#' remove factors from a data frame
#'
#' @param dataframe a data frame
#' @param nlevels an integer - number of max levels of factor columns allowed
#' @return a data frame whose factor columns with either > nlevels
#' of levels or one level are removed

removefactors <- function(dataframe, nlevels = 50) {
    for (name in names(dataframe[sapply(dataframe, is.factor)])) {
        levels <- length(levels(dataframe[[name]]))
        if ((levels > nlevels) || (levels == 1)) {
            dataframe <- dataframe[, !names(dataframe) %in% c(name)]
        }
    }
    return(dataframe)
}


#' expand all levels for factor columns
#'
#' @param dataframe a data frame
#' @param factorcols a list of factor columns to expand
#'     if it's not provided, will use all factor columns
#' @return a dataframe with factor columns expanded

expandlevels <- function(dataframe, factorcols) {
  if (length(factorcols) == 0) {
    factorcols <- names(which(sapply(dataframe, is.factor)))
  }
  for (name in factorcols){
    for (level in levels(dataframe[[name]])){
      dataframe[[paste(name, level, sep='-')]] <- as.factor(
        ifelse(dataframe[[name]] == level, 1, 0))
    }
    dataframe <- dataframe[, -which(names(dataframe) %in% c(name))]
  }
  return(dataframe)
}
