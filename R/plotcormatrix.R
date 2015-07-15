#' plot correlation matrix
#'
#' @param dataframe a data frame
#' @param roundn (optional) an integer number - of decimal places to round
#'   correlation coefficients, default = 2
#' @param type (optional) a string input - "lower" (default) for lower triangular matrix,
#'   'upper' for upper triangular matrix, and "full" for full matrix
#' @param corfix (optiona) a boolean - "FALSE" (default) for natural output, "TRUE" for
#'   square output
#' @return a plot of correlation matrix

plotcormatrix <- function(
  dataframe,
  roundn = 2,
  type = "lower",
  corfix = FALSE
  ) {

    ###### packages ######

    if (!require(ggplot2)) {
        install.packages(ggplot2)
    }
    require(ggplot2)

    if (!require(reshape2)) {
        install.packages(reshape2)
    }
    require(reshape2)

    ###### side functions ######

    get_lower_tri <- function(cormat) {
        cormat[upper.tri(cormat, diag = TRUE)] <- NA
        return(cormat)
    }

    get_upper_tri <- function(cormat) {
        cormat[lower.tri(cormat, diag = TRUE)] <- NA
        return(cormat)
    }

    ###### main ######

    dataframe <- dataframe[sapply(dataframe, is.numeric)]
    sdall <- sapply(dataframe, sd)
    sdall <- sdall[sdall == 0]
    if (length(sdall) > 0) {
      dataframe <- dataframe[, -which(names(dataframe) %in% names(sdall))]
    }
    if (length(dataframe) <= 2) {
      stop("Need at least 3 valid columns in the dataframe to proceed. Check length(dataframe)!")
    }

    cordata <- round(cor(dataframe), roundn)

    if (type == "upper") {
        cordata <- get_upper_tri(cordata)
    } else if (type == "full") {
        cordata <- cordata
    } else {
        cordata <- get_lower_tri(cordata)
    }

    for (i in 1:nrow(cordata)) {
        cordata[i, i] = NaN
    }

    p <- ggplot(
          data = melt(cordata),
          aes(x = Var1, y = Var2, fill = value)
        ) +
        geom_tile(color = "white") +
        scale_fill_gradient2(
            low = "red",
            high = "green",
            mid = "white",
            midpoint = 0,
            limit = c(-1, 1),
            name = "Pearson\nCorrelation"
        ) +
        geom_text(aes(label = value)) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))

    if (corfix == TRUE) {
        p <- p + coord_fixed()
    }

    return(p)
}
