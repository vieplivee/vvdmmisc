#' plot correlation matrix
#'
#' @param dataframe
#' @param roundn (optional) - an integer number of decimal places to round correlation coefficients, default = 2
#' @param type (optional) - 'lower' (default) for lower triangular matrix, 'upper' for upper triangular matrix, and 'full' for full matrix
#' @param corfix (optiona) - 'FALSE' (default) for natural output, 'TRUE' for square output
#' @return correlation matrix
#'
plotcormatrix <- function(dataframe, roundn = 2, type = "lower", corfix = FALSE) {
    
    ###### variables ######
    
    roundn = 2
    type = "lower"
    corfix = FALSE
    
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
    dataframe <- dataframe[, -which(names(dataframe) %in% names(sdall))]
    
    cor.dataframe <- round(cor(dataframe), roundn)
    
    if (type == "upper") {
        cor.dataframe <- get_upper_tri(cor.dataframe)
    } else if (type == "full") {
        cor.dataframe <- cor.dataframe
    } else {
        cor.dataframe <- get_lower_tri(cor.dataframe)
    }
    
    for (i in 1:nrow(cor.dataframe)) {
        cor.dataframe[i, i] = NaN
    }
    
    p <- ggplot(dataframe = melt(cor.dataframe), aes(x = Var1, y = Var2, fill = value)) + geom_tile(color = "white") + 
        scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Pearson\nCorrelation") + 
        theme_minimal() + theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1))
    
    if (corfix == TRUE) {
        p <- p + coord_fixed()
    }
    
    return(p)
} 
