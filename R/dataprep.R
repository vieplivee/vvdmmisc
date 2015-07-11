#' split a data frame into training and testing sets
#'
#' @param dataframe
#' @param seed (optional) - seeding the split. default = 10001
#' @param ratio (optional) - 1/ratio of total set is designated to training.
#'   default = 2
#' @return list - a trainset and a testset

splitdf <- function(dataframe, seed = 10001, ratio = 2) {
    index <- 1:nrow(dataframe)
    trainindex <- sample(index, trunc(length(index)/ratio))
    trainset <- dataframe[trainindex, ]
    testset <- dataframe[-trainindex, ]
    list(trainset = trainset, testset = testset)
}

#' remove factors from a data frame
#'
#' @param dataframe
#' @param nlevels - number of max levels of factor columns allowed
#' @return dataframe whose factor columns with either > nlevels of levels or one
#'   level are removed

removefactors <- function(dataframe, nlevels = 50) {
    for (name in names(dataframe[sapply(dataframe, is.factor)])) {
        levels <- length(levels(dataframe[[name]]))
        if ((levels > nlevels) || (levels == 1)) {
            dataframe <- dataframe[, !names(dataframe) %in% c(name)]
        }
    }
    return(dataframe)
}
