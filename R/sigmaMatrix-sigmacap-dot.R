#' Consistent Estimate of the Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix.
#'   Unbiased estimate of the covariance matrix.
#' @param n Positive integer.
#'   Sample size.
#' @family Covariance Functions
#' @keywords sigmaMatrix covariance dot
#' @noRd
.sigmacap <- function(x,
                      n) {
  x * (
    n - 1
  ) / n
}
