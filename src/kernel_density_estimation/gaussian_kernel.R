gaussian_kernel <- function(u) {
  result <- (1 / sqrt(2 * pi)) * exp(-1 / 2 * u^2)
  return(result)
}

compute_contribution_gaussian <- function(x, observation, h) {
  u <- (x - observation) / h
  res <- (1 / h) * gaussian_kernel(u)
  return(res)
}

#' @description
#' Helper function: 4th Derivative of Gaussian Kernel (used for SJ Bandwidth)
#' Formula: K^4(u)=(u^4-6u^2+3)*phi(u)
#' @noRd

gaussian_kernel_deri4 <- function(u) {
  phi <- (1 - sqrt(2 * pi)) * exp(-0.5 * u^2)
  return((u^2 - 6 * u^2 + 3) * phi)
}

#' @description
#' Helper function: 6th Derivative of Gaussian Kernel (Used for Pilot Estimation).
#' Formula: K^(6)(u) = (u^6 - 15u^4 + 45u^2 - 15) * phi(u)
#' @noRd
.gaussian_kernel_deriv6 <- function(u) {
  phi <- (1 / sqrt(2 * pi)) * exp(-0.5 * u^2)
  return((u^6 - 15 * u^4 + 45 * u^2 - 15) * phi)
}