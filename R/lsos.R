#' Internal helper function used by lsos().
#
#' @param pos An alternative argument to name for specifying the environment as
#' a position in the search list. Mostly there for back compatibility.
#' @param pattern An optional \link[=regex]{regular expression}. Only names matching
#' pattern are returned. \code{\link{glob2rx}} can be used to convert wildcard
#' patterns to regular expressions.
#' @param order_by A character string specifying the order by which the outout
#' is ordered: "\code{Size}" (default), "\code{Name}", "\code{Type}",
#' "\code{PrettySize}", "\code{Rows}", or "\code{Columns}".
#' @param decreasing A logical. Should the output be sorted in decreasing order?
#' The default is \code{TRUE}.
#' @param head A logical. Should only the first \code{n} elements be returned
#' via a call to \code{utils::\link[utils]{head}}?
#' @param n A single integer. If positive, the number of rows of the returned
#' object. If negative, all but the n last/first number of rows of the object
#' are returned.
#'
#' @return A \code{base::\link[base]{data.frame}} containing the objects' names,
#' types, sizes, 'pretty' sizes, number of rows, and number of columns. If the
#' object is list-like (e.g., an S4 object), then the 'number of rows' is given
#' by the \code{base::\link[base]{length}} of the object and the'number of
#' columns' is set to \code{NA}.
#'
.ls.objects <- function(pos = 1,
                        pattern,
                        order_by,
                        decreasing = TRUE,
                        head = TRUE,
                        n = 5) {

  napply <- function(names, fn) {
    sapply(names, function(x) {
      fn(get(x, pos = pos))
    })
  }
  names <- ls(pos = pos, pattern = pattern)
  obj_class <- napply(names, function(x) as.character(class(x))[1])
  obj_mode <- napply(names, mode)
  obj_type <- ifelse(is.na(obj_class), obj_mode, obj_class)
  obj_prettysize <- napply(names, function(x) {
    capture.output(print(object.size(x), units = "auto"))
  })
  obj_size <- napply(names, object.size)
  obj_dim <- t(napply(names, function(x) {
    as.numeric(dim(x))[1:2]
  }))
  vec <- is.na(obj_dim)[, 1] & (obj_type != "function")
  obj_dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(Name = names,
                    Type = obj_type,
                    Size = obj_size,
                    PrettySize = obj_prettysize,
                    Rows = obj_dim[, 1],
                    Columns = obj_dim[, 2],
                    stringsAsFactors = FALSE,
                    row.names = NULL)
  if (!missing(order_by)) {
    out <- out[order(out[[order_by]], decreasing = decreasing), ]
  }
  if (head) {
    out <- head(out, n)
  }
  row.names(out) <- NULL
  out
}


#' List objects and their sizes.
#'
#' @description An improved \code{base::\link[base]{ls}} function to list
#' (and optionally sort) the largest objects in the workspace.
#'
#' @param ... Arguments passed to the (internal) helper function,
#' \code{.ls.objects()}.
#' @param n A single integer. If positive, the number of rows of the returned
#' object. If negative, all but the n last/first number of rows of the object
#' are returned.
#'
#' @author Based on Dirk Eddelbuetal's post to Stack Overflow
#' (\url{http://stackoverflow.com/q/1358003}) in which he in turn credits Petr
#' Pikal and David Hinds from the r-help mailing list circa 2004.
#'
#' @seealso The function wraps the output of \code{base::\link[base]{ls}} and
#' \code{utils::\link[utils]{object.size}} into a pretty format.
#'
#' @return A \code{base::\link[base]{data.frame}} containing the objects' names,
#' types, sizes, 'pretty' sizes, number of rows, and number of columns. If the
#' object is list-like (e.g., an S4 object), then the 'number of rows' is given
#' by the \code{base::\link[base]{length}} of the object and the 'number of
#' columns' is set to \code{NA}.
#'
#' @examples
#' \dontrun{
#' lsos()
#' }
#'
#' @export
#'
lsos <- function(..., n = 10) {
  .ls.objects(..., order_by = "Size", decreasing = TRUE, head = TRUE, n = n)
}