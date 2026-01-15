source("src/kernel_density_estimation/gaussian_kernel.R")
source('src/kernel_density_estimation/kernel_selection/epanechnikov_kernel.R')
#' Create a Kernel Density Estimation (KDE) Model
#'
#' @description
#' This function initializes a KDE model structure using Gaussian kernels.
#' It encapsulates the data and bandwidth into an S3 object for further processing.
#'
#' @param data A numeric vector. The observed data points used to estimate density.
#' @param kernel_type: A charater string. "Gaussian" or "Epanechnikov".
#' @param h A numeric value. The bandwidth (smoothing parameter). Must be positive.
#'
#' @return An object of class \code{"KDE_Model"} containing:
#' \item{data}{The original data points.}
#' \item{h}{The bandwidth used.}
#' \item{n}{The number of observations.}
#'
#' @examples
#' # Initialize a model with bandwidth 1.5
#' my_model <- KDE(data = c(10, 12, 11), h = 1.5)
#' print(class(my_model))
#'

KDE <- function(data, h = NULL, kernel_type = 'gaussian') {
  #1. Validation
  if (is.null(h)) {
    h <- bw_silverman_1(data)
    message(paste("Bandwidth automatically selected (Silverman)", round(h, 4)))
  }
  if (!is.numeric(data)) stop("data must be numeric")
  if (h <= 0) stop("Bandwith h must pe positive")
  valid_kernels <- c('gaussian', 'epanechnikov')
  if (!(kernel_type %in% valid_kernels)) stop("Unknown kernel type!")

  # 2. Ecapsolution
  obj <- list(data = data, h = h, kernel_type = kernel_type, n = length(data))

  # 3. Brading (Label Class)
  class(obj) <- "KDE_Model"
  return(obj)
}

predict.KDE_Model <- function(object, x_grid) {
  # --- SENIOR OPTIMIZATION ---
  # Decision making happens ONCE (outside the loop)
  # Quyết định dùng hàm nào TRƯỚC khi chạy vòng lặp

  if (object$kernel_type == 'gaussian') {
    kernel_fn <- compute_contribution_gaussian
  } else {
    kernel_fn <- compute_contribution_epanechnikov
  }

  # Define worker function that uses the selected kernel_fn
  point_estimate <- function(x) {
    # Gọi hàm đã được chọn ở trên
    contributions <- kernel_fn(x, object$data, object$h)
    mean(contributions)
  }

  #vectorization logic
  densities <- sapply(x_grid, point_estimate)
  return(densities)
}

