# SuSiEShinyApp/simulation_v2.R

# Scenario 1: Perfect Collinearity (from user prompt)
simulate_perfect_collinearity <- function() {
  set.seed(1)
  n <- 500
  p <- 1000
  b <- rep(0, p)
  b[200] <- 1
  b[800] <- 1
  X <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X[, 400] <- X[, 200] # variant 200 and 400 are indistinguishable
  X[, 600] <- X[, 800] # variant 600 and 800 are indistinguishable
  y <- X %*% b + rnorm(n)
  
  # The true causal indices are 200 and 800.
  # The indistinguishable ones are 400 and 600.
  causal_indices <- c(200, 800)
  
  return(list(X = X, y = y, beta = b, causal_indices = causal_indices, 
              data_type = "individual", p = p))
}

# Scenario 2: Moderate LD
simulate_moderate_ld <- function(n = 500, p = 1000) {
  set.seed(2)
  b <- rep(0, p)
  causal_indices <- c(100, 200)
  b[causal_indices] <- 1
  
  # Create a block of moderately correlated variables
  ld_block_size <- 10
  ld_block <- matrix(0.6, nrow = ld_block_size, ncol = ld_block_size)
  diag(ld_block) <- 1
  
  R <- diag(p)
  # Place two LD blocks
  R[95:104, 95:104] <- ld_block
  R[195:204, 195:204] <- ld_block
  
  L_R <- chol(R)
  X_uncorr <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X <- X_uncorr %*% L_R
  X <- scale(X, center = TRUE, scale = TRUE)
  
  y <- drop(X %*% b + rnorm(n))
  
  return(list(X = X, y = y, beta = b, causal_indices = causal_indices, 
              data_type = "individual", p = p))
}

# Scenario 3: Multiple LD Blocks with different LD
simulate_multiple_ld_blocks <- function(n = 500, p = 1000) {
  set.seed(4)
  b <- rep(0, p)
  causal_indices <- c(50, 150, 500)
  b[causal_indices] <- c(1, 1, 1.5)
  
  R <- diag(p)
  
  # Block 1: High LD (0.999)
  ld_block_1 <- matrix(0.999, nrow = 10, ncol = 10)
  diag(ld_block_1) <- 1
  R[45:54, 45:54] <- ld_block_1
  
  # Block 2: Moderate LD (0.6)
  ld_block_2 <- matrix(0.6, nrow = 10, ncol = 10)
  diag(ld_block_2) <- 1
  R[145:154, 145:154] <- ld_block_2
  
  # Block 3: Low LD (0.3)
  ld_block_3 <- matrix(0.3, nrow = 10, ncol = 10)
  diag(ld_block_3) <- 1
  R[245:254, 245:254] <- ld_block_3
  
  L_R <- chol(R)
  X_uncorr <- matrix(rnorm(n * p), nrow = n, ncol = p)
  X <- X_uncorr %*% L_R
  X <- scale(X, center = TRUE, scale = TRUE)
  
  y <- drop(X %*% b + rnorm(n))
  
  return(list(X = X, y = y, beta = b, causal_indices = causal_indices, 
              data_type = "individual", p = p))
}

