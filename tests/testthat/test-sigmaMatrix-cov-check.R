## ---- test-sigmaMatrix-cov-check
lapply(
  X = seq_len(3),
  FUN = function(k,
                 n,
                 text,
                 tol) {
    text <- paste(
      text,
      k
    )
    message(text)
    x <- stats::cov(
      matrix(
        data = stats::runif(
          n = n * k
        ),
        ncol = k
      )
    )
    testthat::test_that(
      paste(text),
      {
        testthat::expect_equal(
          k,
          .check_cov(
            x,
            return_k = TRUE
          )
        )
      }
    )
    x[k, k] <- x[k, k] * -1
    testthat::test_that(
      paste(text, "negative variance"),
      {
        testthat::expect_error(
          .check_cov(x)
        )
      }
    )
    if (k == 1) {
      testthat::test_that(
        paste(text, "asymmetric"),
        {
          testthat::expect_error(
            .check_cov(
              matrix(
                data = 1,
                nrow = 2,
                ncol = 3
              )
            )
          )
        }
      )
    }
  },
  n = 1000,
  text = "test-sigmaMatrix-cov-check",
  tol = 0.001
)
