#' Diagonals of A from vech(A) - Dot Function
#'
#' @inherit diag_of_vech description details references return
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams diag_of_vech
#' @inheritParams dcap
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot
#' @noRd
.diag_of_vech <- function(x,
                          k,
                          loc = FALSE) {
  if (length(x) == 1) {
    if (loc) {
      return(1)
    } else {
      return(x[1])
    }
  }
  j <- 0.5 * (
    2 * k * 1:k - 2 * k + 3 * 1:k - (1:k) * (1:k)
  )
  if (loc) {
    return(j)
  } else {
    return(x[j])
  }
}
