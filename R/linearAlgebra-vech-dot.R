#' Half-Vectorize - Dot Function
#'
#' @author Ivan Jacob Agaloos Pesigan
#'
#' @inherit vech description details references return
#' @inheritParams vech
#'
#' @family Vectorization Functions
#' @keywords linearAlgebra vectorization dot
#' @noRd
.vech <- function(x) {
  x[
    lower.tri(
      x = x,
      diag = TRUE
    )
  ]
}
