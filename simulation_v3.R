# simulation_v3.R

# This script includes all previous simulation functions and adds a new "tricky" scenario.

# Scenario 1: Perfect Collinearity
simulate_perfect_collinearity <- function() {
  n <- 500
  p <- 1000
  b <- rep(0, p)
  b[200] <- 1
  b[800] <- 1
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X[, 400] <- X[, 200]
  X[, 600] <- X[, 800]
  y <- X %*% b + rnorm(n)
  
  causal_indices <- c(200, 800)
  
  return(list(X = X, y = y, true_coef = b, p = p, causal_indices = causal_indices))
}

# Scenario 2: Moderate LD
simulate_moderate_ld <- function(n = 500, p = 1000) {
  b <- rep(0, p)
  causal_indices <- c(100, 200)
  b[causal_indices] <- 1
  
  ld_block_size <- 10
  ld_block <- matrix(0.6, nrow = ld_block_size, ncol = ld_block_size)
  diag(ld_block) <- 1
  
  R <- diag(p)
  R[95:104, 95:104] <- ld_block
  R[195:204, 195:204] <- ld_block
  
  L_R <- chol(R)
  X_uncorr <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X <- X_uncorr %*% L_R
  X <- scale(X, center = TRUE, scale = TRUE)
  
  y <- drop(X %*% b + rnorm(n))
  
  return(list(X = X, y = y, true_coef = b, p = p, causal_indices = causal_indices))
}

# Scenario 3: Multiple LD Blocks
simulate_multiple_ld_blocks <- function(n = 500, p = 1000) {
  b <- rep(0, p)
  causal_indices <- c(50, 150, 250)
  b[causal_indices] <- c(1, 1, 1.5)
  
  R <- diag(p)
  
  ld_block_1 <- matrix(0.99, nrow = 10, ncol = 10)
  diag(ld_block_1) <- 1
  R[45:54, 45:54] <- ld_block_1
  
  ld_block_2 <- matrix(0.6, nrow = 10, ncol = 10)
  diag(ld_block_2) <- 1
  R[145:154, 145:154] <- ld_block_2
  
  ld_block_3 <- matrix(0.3, nrow = 10, ncol = 10)
  diag(ld_block_3) <- 1
  R[245:254, 245:254] <- ld_block_3
  
  L_R <- chol(R)
  X_uncorr <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X <- X_uncorr %*% L_R
  X <- scale(X, center = TRUE, scale = TRUE)
  
  y <- drop(X %*% b + rnorm(n))
  
  return(list(X = X, y = y, true_coef = b, p = p, causal_indices = causal_indices))
}

# Scenario 4: Tricky Scenario (High LD and Weak Signals)
simulate_tricky_scenario <- function(n = 500, p = 1000) {
  b <- rep(0, p)
  
  # Three weak signals in a high LD region
  causal_indices <- c(455, 460, 465)
  b[causal_indices] <- c(0.1, 0.15, 0.2)
  
  # High LD block
  ld_block_size <- 50
  ld_block <- matrix(0.95, nrow = ld_block_size, ncol = ld_block_size)
  diag(ld_block) <- 1
  
  R <- diag(p)
  R[450:499, 450:499] <- ld_block
  
  L_R <- chol(R)
  X_uncorr <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X <- X_uncorr %*% L_R
  X <- scale(X, center = TRUE, scale = TRUE)
  
  y <- drop(X %*% b + rnorm(n))
  
  return(list(X = X, y = y, true_coef = b, p = p, causal_indices = causal_indices))
}
