############################ adapted from gam::lo xvar is a matrix of predictors to which a linear model will be fit
#' @export
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

#' @export
gam.linterms <- function(x, z, w) {
    r <- lm(z ~ . - 1, data = x, weights = w)
    r$nl.df <- 1
    r$var <- NA
    r
}

############################ adapted from gam::lo xvar is a matrix of predictors to which a linear model will be fit
#' @export
slterms <- function(xvar, slargs) {
    
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
    real.call <- substitute(gam.slterms(data[[chcall]], z, w, slargs), list(chcall = chcall, slargs = slargs))
    atts <- c(attributes(xvar), list(call = real.call))
    attributes(xvar) <- atts
    class(xvar) <- c("smooth", "matrix")
    
    return(xvar)
}

#' @export
gam.slterms <- function(x, z, w, slargs) {
    all_args <- c(list(Y = z, X = as.data.frame(x), obsWeights = w), slargs)
    # todo: think about how cross-validation is being used here
    sl_obj <- do.call(origami_SuperLearner, all_args)
    sl_obj$fitted <- predict(sl_obj, newdata = x)$pred
    sl_obj$residuals <- z - sl_obj$fitted
    sl_obj$nl.df <- 1
    sl_obj$var <- NA
    
    return(sl_obj)
}
