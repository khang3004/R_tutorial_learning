source('src/kernel_density_estimation/gaussian_kernel.R')
source("src/kernel_density_estimation/kde_point_estimation.R")

#' Plot Method for KDE_Model
#'
#' @description
#' Visualizes the estimated density function.
#'
#' @param x An object of class \code{KDE_Model}.
#' @param main (Optional) The title of the plot.
#' @param ... Additional graphical parameters passed to \code{\link{plot}} (e.g., col, lwd, ylim).
#'
#' @importFrom graphics plot rug
#' @export
plot.KDE_Model <- function(x, main = NULL, ...) {
  # --- 1. TYPE SAFETY ---
  if (!inherits(x, "KDE_Model")) stop("Object must be of class 'KDE_Model'")

  # --- 2. AUTOMATION (Grid Generation) ---
  data_range <- range(x$data)
  padding <- diff(data_range) * 0.3
  grid <- seq(from = data_range[1] - padding,
              to = data_range[2] + padding,
              length.out = 200)

  # --- 3. CALCULATION ---
  y <- predict(x, grid)

  # --- 4. TITLE HANDLING ---
  # Nếu user KHÔNG truyền main, ta tự tạo.
  # Nếu user CÓ truyền, biến 'main' ở tham số đã hứng lấy nó,
  # nên nó KHÔNG còn nằm trong '...' nữa (Tránh lỗi duplicate argument).
  if (is.null(main)) {
    main_title <- paste0("KDE Plot (h = ", round(x$h, 4), " - ", toupper(x$kernel_type), ")")
  } else {
    main_title <- main
  }

  # --- 5. VISUALIZATION ---
  # Pass '...' (màu sắc, độ dày...) xuống hàm plot gốc
  plot(grid, y, type = "l",
       main = main_title,  # Truyền title đã xử lý
       xlab = "Data Values",
       ylab = "Density",
       ...)                # Truyền các tham số còn lại (col, lwd...)

  # Add rug plot
  # Lưu ý: rug() cũng có thể nhận màu từ ... nếu muốn đồng bộ
  rug(x$data, col = "red")
}