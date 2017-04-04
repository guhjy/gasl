
# models supported by gasl
gasl.slist <- c("linterms", "slterms")

# run one more iteration of each smoother and return the underlying fit object (gam::gam.fit does not return these)
extract_fits <- function(fit) {
    
    data <- fit$smooth.frame
    w <- fit$weights
    smooth.labels <- names(data)
    smooth.wanted <- smooth.labels[match(smooth.labels, labels(fit), 0) > 0]
    residuals <- fit$residuals
    smoother_fits <- foreach(TT = smooth.wanted) %do% {
        Call <- attr(data[[TT]], "call")
        # Call$xeval <- substitute(smooth.frame[[TT]], list(TT = TT))
        z <- residuals + fit$smooth[, TT]
        eval(Call)
    }
    return(smoother_fits)
}

# generate a gam dataset splitting the covariates into factor variables and the remaining 'covariate' covariates'
split_x <- function(X, factor_vars) {
    
    factor_cols <- match(factor_vars, colnames(X))
    
    x_factor <- X[, factor_cols, drop = F]
    x_covariate <- X[, -1 * factor_cols, drop = F]
    gam_data <- list(x_factor = x_factor, x_covariate = x_covariate)
    
}

#' GAM with Super Learner smoother
#'
#' @param X predictor variables as a matrix
#' @param Y outcome variable, a vector.
#' @param factor_vars a character vector indicating which variables to include in the factor fit. All other variables in \code{X} will be included in the covariate fit.
#' @param factor_args Either \code{'lm'} (for a linear smoother) or a list of arguments for \code{\link{origami::origami_SuperLearner}}, indicating the smoother used for the factor variables
#' @param covariate_args Either \code{'lm'} (for a linear smoother) or a list of arguments for \code{\link{origami::origami_SuperLearner}}, indicating the smoother used for the covariate variables
#' @param gam_control A list of parameters to control the gam fit. Usually generated with \code{\link{gam::gam.control}}
#' @return A gasl object
#' @export
gasl <- function(X, Y, factor_vars, factor_args = "lm", covariate_args = "lm", gam_control = gam::gam.control(trace = F, 
    bf.epsilon = 0.001)) {
    
    call <- match.call()
    
    n <- length(Y)
    gam_data <- split_x(X, factor_vars)
    gam_data$Y <- Y
    
    
    # construct gam formula
    factor_term <- ifelse(factor_args == "lm", "linterms(x_factor)", "slterms(x_factor,factor_args)")
    covariate_term <- ifelse(covariate_args == "lm", "linterms(x_covariate)", "slterms(x_covariate,covariate_args)")
    formula <- formula(sprintf("Y ~ %s + %s", factor_term, covariate_term))
    
    mt <- terms(formula, gasl.slist, data = gam_data)
    mf <- model.frame(mt, gam_data)
    
    
    # force a constant, but no covariate terms
    
    # departs from gam package practice, which includes main linear terms for all covariates
    X_const <- as.matrix(rep(1, n), ncol = 1, nrow = n)
    colnames(X_const) <- "(Intercept)"
    
    
    fit <- gam::gam.fit(x = X_const, y = Y, smooth.frame = mf, family = binomial(), control = gam_control)
    
    method <- "general.wam"
    fit <- c(fit, list(call = call, formula = formula, terms = mt, model = mf, data = gam_data, offset = offset, control = gam_control, 
        method = method, factor_vars = factor_vars, contrasts = attr(X_const, "contrasts"), xlevels = .getXlevels(mt, 
            mf)))
    class(fit) <- c("gasl", "gam", "glm", "lm")
    
    # actually store smoother fits
    fit$smoother_fits <- extract_fits(fit)
    
    fit
    
    
}
