gasl <- function(x_factor, x_other, y, factor_model, other_model) {
    # for now, let's hardcode SL for other, and lm for factor
    
    # define split between index variables and other variables
    data$fac_mat <- as.matrix(data[, c("W2", "W3")])
    data$other_mat <- as.matrix(data[, c("W1", "Aind", "AW1")])
    
    # debugonce(general.wam)
    formula <- Y ~ linterms(fac_mat) + linterms(other_mat)
    mt <- terms(formula, gam.slist, data = data)
    mf <- model.frame(mt, data)
    # debugonce(general.wam)
    control <- gam.control(trace = T, bf.epsilon = 1e-05)
    X <- as.matrix(rep(1, n), ncol = 1, nrow = n)
    fit <- gam.fit(x = X, y = data$Y, smooth.frame = mf, family = binomial(), control = control)
    
    method <- "general.wam"
    fit <- c(fit, list(call = call, formula = formula, terms = mt, model = mf, data = data, offset = offset, control = control, method = method, 
        contrasts = attr(X, "contrasts"), xlevels = .getXlevels(mt, mf)))
    class(fit) <- c("gam", "glm", "lm")
    
    return(fit)
}
