#' Consistent Estimate of the Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric matrix, data frame, or vector.
#'   Data.
#' @examples
#' data(iris)
#' sigmacap(iris[, 1:4])
#' covc(iris[, 1:4])
#' @family Covariance Functions
#' @keywords sigmaMatrix covariance
#' @export
sigmacap <- function(x) {
  stopifnot(
    is.matrix(x) || is.data.frame(x) || is.vector(x)
  )
  if (is.vector(x)) {
    return(
      matrix(
        data = .sigmacap(
          x = stats::var(x),
          n = length(x)
        ),
        ncol = 1
      )
    )
  }
  .sigmacap(
    x = stats::cov(x),
    n = dim(x)[1]
  )
}
