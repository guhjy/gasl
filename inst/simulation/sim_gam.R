source(system.file("simulation", "DGD.R", package = "gasl"))

sim_data <- gen_data()

# fit basic models
true_fit <- glm(Y ~ A * W1 + W2 + W3, sim_data, family = "binomial")
sim_data$true_pred <- predict(true_fit, type = "response")
sim_data$null_pred <- mean(sim_data$Y)
sim_data$min_pred <- sim_data$Q0aW[cbind(1:nrow(sim_data), as.numeric(sim_data$A))]

# fit gasl define split between index variables and other variables
x_factor <- model.matrix(Y ~ W2 + W3 - 1, sim_data)
x_other <- model.matrix(Y ~ A * W1, sim_data)
y <- sim_data$Y

gasl_fit <- gasl(x_factor, x_other, y)
sim_data$gasl_pred <- predict(gasl_fit, type = "response")

dev <- function(Y, pred) {
    dev_sign <- ifelse(Y == 1, 1, -1)
    dev_sign * sqrt(-2 * (Y * log(pred) + (1 - Y) * log(1 - pred)))
}

risk <- function(Y, pred) {
    mean(dev(Y, pred)^2)
}


risk(sim_data$Y, sim_data$null_pred)
risk(sim_data$Y, sim_data$gasl_pred)
risk(sim_data$Y, sim_data$true_pred)
risk(sim_data$Y, sim_data$min_pred)
