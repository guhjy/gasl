# logistic risk
logit_dev <- function(Y, pred) {
    dev_sign <- ifelse(Y == 1, 1, -1)
    dev_sign * sqrt(-2 * (Y * log(pred) + (1 - Y) * log(1 - pred)))
}

logit_risk <- function(Y, pred) {
    mean(logit_dev(Y, pred)^2)
}
