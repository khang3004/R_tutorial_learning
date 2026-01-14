source('src/kernel_density_estimation/gaussian_kernel.R')
source("src/kernel_density_estimation/kde_point_estimation.R")
# # 1Prepare data
# data <- c(10, 12, 11, 15, 12, 14, 16, 11, 12)
# # 2. Create Grid
# x_grid <- seq(1, 30, 0.1)
# h <- 0.8
#
# #3. Calculate Density
# estimated_values <- sapply(x_grid, kde_point_estimate, observate = data, h = h)
# estimated_values
#
# # 4. Visualiza
# plot(x_grid, estimated_values, type = "l",
#      col = "blue", lwd = 2,
#      main = "My Scratch KDE Implementation",
#      xlab = "Value", ylab = "Density")
#
#
# rug(data, col = 'red')

#' Define specialize plot function for KDE_Model class
plot.KDE_Model <- function(x) {
  # x is a KDE_Model object
  #1: Automate generate grid
  # Get range from data to expand more 30%
  data_range <- range(x$data)
  padding <- diff(data_range) * 0.3
  grid <- seq(data_range[1] - padding, data_range[2] + padding, length.out = 200)

  # Revoke predict function
  y <- predict(x, grid)

  #3. Plot
  plot(grid, y, type = 'l', lwd = 2, col = 'darkblue',
       main = paste("KDE Plot (h =", x$h, ")"),
       xlab = "Data Values", ylab = "Density")
  #Add rug plot for illustrate data
  rug(x$data, col = 'red')
}
