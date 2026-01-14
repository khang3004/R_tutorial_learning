gaussian_kernel <- function(u) {
  result <- (1 / sqrt(2 * pi)) * exp(-1 / 2 * u^2)
  return(result)
}

compute_contribution <- function(x, observation, h) {
  u <- (x - observation) / h
  res <- (1 / h) * gaussian_kernel(u)
  return(res)
}

compute_contribution(3, c(0, 1, 2), 0.3)

# # POP kde Point Estimation funciton
# kde_point_estimate <- function(x, observate, h){
#   gau_contribution <- compute_contribution(x, observate, h)
#   res <- mean(gau_contribution)
#   return(res)
# }
# kde_point_estimate(3,c(0,1,2),0.3)