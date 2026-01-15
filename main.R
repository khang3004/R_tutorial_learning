# main.R
# ==============================================================================
# PROJECT: KERNEL DENSITY ESTIMATION FROM SCRATCH
# AUTHOR: [Your Name]
# DESCRIPTION: Main entry point for training and visualizing KDE models.
# ==============================================================================
# main.R

# 1. Load Modules
# recursive = TRUE: Đào sâu vào mọi ngóc ngách thư mục con
# pattern = "\\.R$": Chỉ lấy file có đuôi .R (bỏ qua file rác, folder)
R_files <- list.files(path = "src", pattern = "\\.R$", full.names = TRUE, recursive = TRUE)

# Debug: In ra để xem nó tìm được gì
message("Found modules:")
print(R_files)

# Source tất cả
lapply(R_files, source)

# 2. Data Preparation
# Using sample data (or loading from CSV/DB in real scenarios)
raw_data <- c(10, 12, 11, 15, 12, 14, 16, 11, 12, 9, 10, 13)

# 3. Model Instantiation (The OOP way)
# Scenario A: Gaussian Kernel with Auto Bandwidth
print("--- Initializing Gaussian Model ---")
gaussian_model <- KDE(data = raw_data, kernel_type = "gaussian")

# Scenario B: Epanechnikov Kernel with Manual Bandwidth
print("--- Initializing Epanechnikov Model ---")
epanech_model <- KDE(data = raw_data, h = 0.8, kernel_type = "epanechnikov")

# 4. Visualization & Reporting
# Set up a 1x2 plotting area
par(mfrow = c(1, 2))

# Plot 1
plot(gaussian_model, main = "Gaussian Kernel (Auto bw)")

# Plot 2
plot(epanech_model, main = "Epanechnikov Kernel (h=0.8)", col = "purple")

# Reset plotting area
par(mfrow = c(1, 1))

print("--- Execution Completed Successfully ---")