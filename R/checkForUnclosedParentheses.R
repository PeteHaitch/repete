# Based on https://stat.ethz.ch/pipermail/r-devel/2018-March/075661.html
checkForUnclosedParentheses <- function(file, open = "(", close = ")") {
  text <- readLines(file)
  letters <- strsplit(text, "")
  line <- unlist(lapply(seq_along(letters),
                        function(i) rep(i, length(letters[[i]]))))
  column <- unlist(lapply(seq_along(letters),
                          function(i) seq_len(length(letters[[i]]))))
  letters <- unlist(letters)
  sum <- cumsum(open) - cumsum(close)

  result <- FALSE
  report <- function(msg, where) {
    message(msg, paste(file, line[where], column[where], sep = ":"))
    message(text[line[where]])
    message(paste(c(rep(" ", column[where] - 1), "^"), collapse = ""))
    rstudioapi::navigateToFile(file, line[where], column[where])
  }
  if (any(sum < 0)) {
    report("Extra close paren: ", match(TRUE, sum < 0))
    result <- TRUE
  }
  if (sum[length(sum)] > 0) {
    report("Extra open paren: ",
           length(sum) - match(TRUE, rev(sum == 0)) + 2)
    result <- TRUE
  }
  result
}
