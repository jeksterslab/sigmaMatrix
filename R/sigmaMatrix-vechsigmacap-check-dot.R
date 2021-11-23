#' Sanity Check for the Half-Vectorization
#' of the Covariance Matrix
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @param x Numeric vector.
#'   Half-vectorization of the covariance matrix.
#' @param return_k Logical.
#'   Return the dimension of the covariance matrix.
#' @family Covariance Functions
#' @keywords sigmaMatrix covariance check
#' @noRd
.check_vechsigmacap <- function(x,
                                return_k = FALSE) {
  k <- .check_vech(
    x = x,
    return_k = TRUE
  )
  stopifnot(
    all(
      .diag_of_vech(
        x = x,
        k = k,
        loc = FALSE
      ) > 0
    )
  )
  if (return_k) {
    return(k)
  }
}
