gasl.slist <- c("linterms", "slterms")

gasl <- function(x_factor, x_other, y, factor_model, other_model) {
    # for now, let's hardcode SL for other, and lm for factor
    data <- list(x_factor = x_factor, x_other = x_other, y = y)
    n <- length(y)
    
    formula <- y ~ linterms(x_factor) + linterms(x_other)
    mt <- terms(formula, gasl.slist, data = data)
    mf <- model.frame(mt, data)
    
    # try to force faster convergence in backfitting
    control <- gam::gam.control(trace = T, bf.epsilon = 1e-05)
    
    # force a constant, but no other terms fit using a linear model departs from gam package practice
    X_const <- as.matrix(rep(1, n), ncol = 1, nrow = n)
    # debugonce('gam.fit')
    fit <- gam::gam.fit(x = X_const, y = y, smooth.frame = mf, family = binomial(), control = control)
    
    method <- "general.wam"
    fit <- c(fit, list(call = call, formula = formula, terms = mt, model = mf, data = data, offset = offset, control = control, method = method, 
        contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, mf)))
    class(fit) <- c("gam", "glm", "lm")
    
    return(fit)
}
