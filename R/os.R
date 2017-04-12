#' A shortcut to \code{pryr::object_size}
#' @inheritParams pryr::object_size
#' @importFrom pryr object_size
#' @export
os <- function(..., env) {
  pryr::object_size(..., env)
}