# create_example_datasets_v3.R

# This script generates example datasets that can be used to test the
# file upload functionality of the SuSiE Shiny app.

# 1. Example Individual-level Dataset
set.seed(101)
n <- 300
p <- 500
beta <- rep(0, p)
causal_indices <- c(50, 150, 250)
beta[causal_indices] <- c(0.5, 1, 1.5)
X <- matrix(rnorm(n * p), nrow = n, ncol = p)
y <- X %*% beta + rnorm(n)

write.table(X, "example_X.txt", row.names = FALSE, col.names = FALSE)
write.table(data.frame(y = y), "example_y.txt", row.names = FALSE, col.names = FALSE)

# 2. Example Summary Statistics Dataset
set.seed(102)
n_sumstat <- 500
p_sumstat <- 200
beta_sumstat <- rep(0, p_sumstat)
causal_indices_sumstat <- c(20, 80)
beta_sumstat[causal_indices_sumstat] <- c(0.8, 1.2)
X_sumstat <- matrix(rnorm(n_sumstat * p_sumstat), nrow = n_sumstat, ncol = p_sumstat)
y_sumstat <- X_sumstat %*% beta_sumstat + rnorm(n_sumstat)

# Compute summary statistics
R <- cor(X_sumstat)
ss <- susieR::univariate_regression(X_sumstat, y_sumstat)
z <- ss$betahat / ss$sebetahat

write.table(R, "example_R.txt", row.names = FALSE, col.names = FALSE)
write.table(data.frame(z = z), "example_z.txt", row.names = FALSE, col.names = FALSE)

print("Example datasets created successfully:")
print("- example_X.txt")
print("- example_y.txt")
print("- example_R.txt")
print("- example_z.txt")
