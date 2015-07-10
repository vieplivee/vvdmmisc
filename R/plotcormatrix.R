plotcormatrix <- function(data, roundn = 2, type = "lower", corfix = FALSE) {
    
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
    
    data <- data[sapply(data, is.numeric)]
    sdall <- sapply(data, sd)
    sdall <- sdall[sdall == 0]
    data <- data[, -which(names(data) %in% names(sdall))]
    
    cor.data <- round(cor(data), roundn)
    
    if (type == "upper") {
        cor.data <- get_upper_tri(cor.data)
    } else if (type == "full") {
        cor.data <- cor.data
    } else {
        cor.data <- get_lower_tri(cor.data)
    }
    
    for (i in 1:nrow(cor.data)) {
        cor.data[i, i] = NaN
    }
    
    p <- ggplot(data = melt(cor.data), aes(x = Var1, y = Var2, fill = value)) + geom_tile(color = "white") + 
        scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, 
            limit = c(-1, 1), name = "Pearson\nCorrelation") + theme_minimal() + theme(axis.text.x = element_text(angle = 45, 
        vjust = 1, size = 12, hjust = 1))
    
    if (corfix == TRUE) {
        p <- p + coord_fixed()
    }
    
    return(p)
} 
