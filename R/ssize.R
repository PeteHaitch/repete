#' Size of objects in slots of S4 object
#'
#' @param x An S4 object
#' @param recursive A logical(1) indicating whether the function should
#' recursively be called on slots that are themselves S4 objects
#' @param depth An numeric(1) indicating how many levels to recurse
#'
#' @return A nested named list with the size of the objects in each slot
#'
#' @importFrom pryr object_size
#' @importFrom methods slot slotNames
#' @export
ssize <- function(x, recursive = TRUE, depth = Inf) {
  stopifnot(isS4(x))
  sapply(slotNames(x), function(sn) {
    xs <- pryr::object_size(slot(x, sn))
    if (recursive && isS4(slot(x, sn)) && depth > 0) {
      ys <- ssize(slot(x, sn), recursive = TRUE, depth = depth - 1)
      xs <- c(overall = list(xs), ys)
    }
    xs
  }, simplify = FALSE, USE.NAMES = TRUE)
}

# TODO: unit tests
# TODO: Option to sort data at each level (which preserving units)
# TODO: Better printing of nested list
# TODO: Support non-S4 objects in slot