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

  # The size of objects differs on 32-bit and 64-bit platforms. I think this
  # conditional handles the situation appropriately (although it might fall
  # over on Solaris?)
  val <- lsos(env)
  if (identical(R.Version()$arch, "x86_64")) {
    expect_identical(val,
                     data.frame(Name = c("y", "x", "z"),
                                Type = c("data.frame", "matrix", "integer"),
                                Size = c(1688, 600, 88),
                                PrettySize = c("1.69 Kb", "600 B",
                                               "88 B"),
                                Rows = c(10, 25, 10),
                                Columns = c(2, 4, NA),
                                stringsAsFactors = FALSE))
  } else if (identical(R.Version()$arch, "i386")) {
    expect_identical(val,
                     data.frame(Name = c("y", "x", "z"),
                                Type = c("data.frame", "matrix", "integer"),
                                Size = c(1112, 512, 72),
                                PrettySize = c("1.1 Kb", "512 B",
                                               "72 B"),
                                Rows = c(10, 25, 10),
                                Columns = c(2, 4, NA),
                                stringsAsFactors = FALSE))
  } else {
    stop("Unexpected architecture")
  }
})
