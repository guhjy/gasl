print.gasl <- function(object) {
    cat(sprintf("%s + %s\n\n\n", format(object$formula), format(object$coefficients)))
    result <- foreach(i = seq_along(object$smoother_fits)) %do% {
        cat(names(object$smooth.frame)[[i]])
        print(object$smoother_fits[[i]])
        cat("\n\n")
    }
    
    gam:::print.gam(object)
    return(NULL)
    
}
