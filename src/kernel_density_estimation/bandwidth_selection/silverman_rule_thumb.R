bw_silverman_0 <- function(data) {
  N <- length(data)
  mu <- mean(data)
  sigma_hat <- sqrt((1 / (N - 1)) * sum((data - mu)^2))
  return(1.06 * N^(-1 / 5) * sigma_hat)
}

bw_silverman_1 <- function(data) {
  # 1. Validation
  N <- length(data)
  if (N < 2) stop("Need at least 2 data points to calculate bandwidth!")

  # 2. Calculate statistics using R buildt-in functions (Robust & Fast)
  sigma_hat <- sd(data)
  iqr_val <- IQR(data)

  # 3. Rule-of-thumb formula
  scaled_param <- min(sigma_hat, iqr_val / 1.34)

  h_star <- 1.06 *
    scaled_param *
    N^(-1/5)
  return(h_star)
}

bw_silverman_0(c(11, 12, 12, 23, 34, 45, 46, 57, 58, 49, 22, 21, 25))
bw_silverman_1(c(11, 12, 12, 23, 34, 45, 46, 57, 58, 49, 22, 21, 25))