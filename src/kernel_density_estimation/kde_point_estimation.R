source("src/kernel_density_estimation/gaussian_kernel.R")
#' Create a Kernel Density Estimation (KDE) Model
#'
#' @description
#' This function initializes a KDE model structure using Gaussian kernels.
#' It encapsulates the data and bandwidth into an S3 object for further processing.
#'
#' @param data A numeric vector. The observed data points used to estimate density.
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

KDE <- function(data, h) {
  #1. Validation
  if (!is.numeric(data)) stop("data must be numeric")
  if (h <= 0) stop("Bandwith h must pe positive")

  # 2. Ecapsolution
  obj <- list(data=data, h=h, n=length(data))

  # 3. Brading (Label Class)
  class(obj) <- "KDE_Model"
  return(obj)
}

predict.KDE_Model <- function(object, x_grid){
  point_estimate <- function(x){
    contributions <- compute_contribution(x, object$data, object$h)
    mean(contributions)
  }

  #vectorization logic
  densities <- sapply(x_grid, point_estimate)
  return(densities)
}
