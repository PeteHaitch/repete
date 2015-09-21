context("lsos")

test_that("lsos() works", {
  # NOTE: This will ERROR if the new environment isn't created and used for
  # this test. It's something complicated about what environment the lsos()
  # looks up when run via testthat (which itself sets up some environments).
  # While this is not ideal, it is sufficient for testing purposes.
  env <- new.env()
  env$x <- matrix(1:100, ncol = 4)
  env$y <- data.frame(a = letters[1:10], b = 10:1)
  env$z <- seq_len(10)
  # TODO: Remove once test fixed on windows (see
  # https://github.com/PeteHaitch/repete/issues/1)
  print(lsos(env))
  expect_identical(lsos(env),
                   data.frame(Name = c("y", "x", "z"),
                              Type = c("data.frame", "matrix", "integer"),
                              Size = c(1840, 600, 88),
                              PrettySize = c("1.8 Kb", "600 bytes", "88 bytes"),
                              Rows = c(10, 25, 10),
                              Columns = c(2, 4, NA),
                              stringsAsFactors = FALSE))
})