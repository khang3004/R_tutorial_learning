epanechikov_kernel <- function(u) {
  # ifelse(abs(u) <= 1, 0.75*(1 - u^2), 0)
  (abs(u)<=1)*(0.75*(1-u^2))
}

compute_contribution_epanechnikov <- function(x, data, h) {
  N <- length(data)
  (1 / (N * h)) * sum(epanechikov_kernel((x - data) / h))
}

# compute_contribution_epanechnikov(x = 29, data = c(100, 100, 102, 102, 103, 104, 104, 105,
#                                                  99, 92), h = 0.3)