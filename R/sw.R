#' Shortcut to sum the widths of a \linkS4class{GenomicRanges} object
#'
#' @param x A \linkS4class{GenomicRanges} object.
#'
#' @return `sum(as.numeric(width(x))`
#' @importFrom GenomicRanges width
#' @export
sw <- function(x) {
  sum(as.numeric(width(x)))
}
