## ---- test-sigmaMatrix-vechcov-check
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
          .check_vechcov(
            .vech(x),
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
          .check_vechcov(
            .vech(x)
          )
        )
      }
    )
    if (k == 1) {
      testthat::test_that(
        paste(text, "invalid length"),
        {
          testthat::expect_error(
            .check_vechcov(
              rep(x = 1, times = 2)
            )
          )
        }
      )
      testthat::test_that(
        paste(text, "invalid length"),
        {
          testthat::expect_error(
            .check_vechcov(
              rep(x = 1, times = 4)
            )
          )
        }
      )
    }
  },
  n = 1000,
  text = "test-sigmaMatrix-vechcov-check",
  tol = 0.001
)
