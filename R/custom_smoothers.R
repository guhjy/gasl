############################ adapted from gam::lo xvar is a matrix of predictors to which a linear model will be fit
linterms <- function(xvar) {
    
    locall <- sys.call()
    chcall <- deparse(locall)
    
    xvar <- as.matrix(xvar)
    xnames <- deparse(locall[[2]])
    nc <- ncol(xvar)
    
    if (is.null(dimnames(xvar)[[2]])) {
        
        dxnames <- xnames
        if (nc > 1) 
            dxnames <- paste(xnames, 1:nc, sep = ".")
        dimnames(xvar) <- list(NULL, dxnames)
    }
    
    
    nobs <- dim(xvar)[1]
    nas <- is.na(xvar[, 1:nc])
    if (any(nas)) {
        if (p > 1) 
            nas <- nas %*% array(1, c(nc, 1))
        attr(xvar, "NAs") <- seq(nobs)[nas > 0]
    }
    real.call <- substitute(gam.linterms(data[[chcall]], z, w))
    atts <- c(attributes(xvar), list(call = real.call))
    attributes(xvar) <- atts
    class(xvar) <- c("smooth", "matrix")
    
    return(xvar)
}

gam.linterms <- function(x, z, w) {
    r <- lm.wfit(x, z, w)
    r$nl.df <- 1
    r$var <- NA
    
    r
}
