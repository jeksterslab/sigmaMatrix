## ---- test-sigmaMatrix-covc
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
    x <- matrix(
      data = stats::runif(
        n = n * k
      ),
      ncol = k
    )
    if (k == 1) {
      testthat::test_that(
        text,
        {
          testthat::expect_true(
            all(
              abs(
                covc(.vec(x)) - stats::cov(x)
              ) <= tol
            )
          )
        }
      )
    }
    testthat::test_that(
      text,
      {
        testthat::expect_true(
          all(
            abs(
              covc(x) - stats::cov(x)
            ) <= tol
          )
        )
      }
    )
  },
  n = 1000,
  text = "test-sigmaMatrix-covc",
  tol = 0.001
)
