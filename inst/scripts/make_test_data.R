source(system.file("simulation", "DGD.R", package = "gasl"))

set.seed(1)

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

test_X <- test_data[, c("A", "W1", "W2", "W3")]
test_X$AW1 <- test_X$A * test_X$W1
test_X <- as.matrix(test_X)

gasl_test_sim <- list(sim_data = sim_data, test_data = test_data, X = X, Y = Y, test_X = test_X)
use_data(gasl_test_sim, overwrite = TRUE)
