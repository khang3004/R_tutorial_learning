source("src/kernel_density_estimation/kde_point_estimation.R")
source("src/kernel_density_estimation/gaussian_kernel.R")
source("src/kernel_density_estimation/kde_visualization.R")

# 1. Instantiate (Khởi tạo)
# Thử cố tình viết sai chữ hoa xem nó có tự sửa không?
my_model <- KDE(data = c(10, 12, 11, 15, 12, 14, 16, 11, 12),
                kernel_type = "epanechnikov")

# 2. Verify Object
print(paste("Model Type:", class(my_model)))
print(paste("Selected Bandwidth:", round(my_model$h, 4)))

# 3. Predict & Plot
# Tạo lưới dữ liệu
grid <- seq(8, 18, 0.1)
y <- predict(my_model, grid)

# Vẽ hình
plot(grid, y, type = "l", col = "purple", lwd = 2,
     main = "Epanechnikov KDE (Optimized Code)")
rug(my_model$data)