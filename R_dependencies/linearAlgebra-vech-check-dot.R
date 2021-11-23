#' Sanity Checks for the Half-Vectorization
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inheritParams sym_of_vech
#' @param return_k Logical.
#'   Return valid `k`.
#' @family Vectorization Functions
#' @keywords linearAlgebra check
#' @noRd
.check_vech <- function(x,
                        return_k = FALSE) {
  stopifnot(
    is.vector(x)
  )
  k <- 0.5 * (
    sqrt(
      1 + 8 * length(x)
    ) - 1
  )
  stopifnot(
    k %% 1 == 0
  )
  if (return_k) {
    return(k)
  }
}
