library(origami)

Qbar0_W1 <- function(A, W1) {
    plogis(-5 * (A == 1) * (W1 + 0.5) + 5 * (A == 0) * (W1 - 0.5))
}

Qbar0 <- function(A, W) {
    W1 <- W[, 1]
    W2 <- W[, 2]
    W3 <- W[, 3]
    Qbar <- plogis(qlogis(Qbar0_W1(A, W1)) + 5 * W2 + 4 * W3)
    return(Qbar)
}

g0 <- function(W) {
    W1 <- W[, 1]
    W2 <- W[, 2]
    W3 <- W[, 3]
    
    # rep(0.5, nrow(W))
    scale_factor <- 0.8
    A <- plogis(scale_factor * (W1 + W2 + W3))
}

gen_data <- function(n = 1000, p = 3) {
    p <- max(p, 3)
    W <- matrix(rnorm(n * p), nrow = n)
    colnames(W) <- paste("W", seq_len(p), sep = "")
    g0W <- g0(W)
    A <- rbinom(n, 1, g0W)
    A_vals <- sort(unique(A))
    u <- runif(n)
    Y <- as.numeric(u < Qbar0(A, W))
    
    Q0aW <- sapply(A_vals, Qbar0, W)
    d0 <- max.col(Q0aW)
    Yd0 <- as.numeric(u < Qbar0(d0, W))
    df <- data.frame(W, A, Y, d0, Yd0)
    
    df$g0W <- g0(W)
    df$Q0aW <- Q0aW
    
    return(df)
}
