source("src/kernel_density_estimation/kde_point_estimation.R")
source("src/kernel_density_estimation/gaussian_kernel.R")
source("src/kernel_density_estimation/kde_visualization.R")

#1 Instantiate
my_model <- KDE(data=c(10,12,11,15,12,14,16,11,12), h=1.5)

#2. Use the model
print(class(my_model))

plot(my_model)

density_at_13 <- predict(my_model, x_grid = 13)
print(paste("Density at 13 is: ", density_at_13))