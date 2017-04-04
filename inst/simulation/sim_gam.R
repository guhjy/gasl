# extract coefs write vignette

library(gasl)
source(system.file("simulation", "DGD.R", package = "gasl"))

n <- 1000
sim_data <- gen_data(n)
test_data <- gen_data(n * 10)

# fit basic models
true_model_fit <- glm(Y ~ A * W1 + W2 + W3, sim_data, family = "binomial")
test_data$true_pred <- predict(true_model_fit, newdata = test_data, type = "response")
test_data$null_pred <- mean(sim_data$Y)
test_data$Q0 <- test_data$Q0aW[cbind(seq_along(test_data$A), test_data$A + 1)]

# fit gasl
X <- sim_data[, c("A", "W1", "W2", "W3")]
X$AW1 <- X$A * X$W1
X <- as.matrix(X)
Y <- sim_data$Y

factor_vars <- c("W2", "W3")
covariate_args <- list(SL.library = c("SL.glm", "SL.mean"), folds = make_folds(n))

gasl_fit <- gasl(X, Y, factor_vars, covariate_args = covariate_args)

test_X <- test_data[, c("A", "W1", "W2", "W3")]
test_X$AW1 <- test_X$A * test_X$W1
test_X <- test_X

test_data$gasl_pred <- predict(gasl_fit, newdata = test_X, type = "response")

sim_data$gasl_pred <- predict(gasl_fit, newdata = X, type = "response")
sim_data$gasl_pred2 <- predict(gasl_fit, type = "response")


logit_risk(test_data$Y, test_data$null_pred)
logit_risk(test_data$Y, test_data$gasl_pred)
logit_risk(test_data$Y, test_data$true_pred)
logit_risk(test_data$Y, test_data$Q0)
