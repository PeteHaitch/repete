#' Scatter plot with transparency
#'
#' An alternative to a \code{graphics::\link[graphics]{smoothScatter}} that
#' actually plots all points but uses transparency to avoid overplotting.
#' Recommended to me by Kasper Hansen.
#'
#' @param x,y The \code{x} and \code{y} values to be plotted
#' @param col The colour transparency to be used
#' @param pch,cex See \code{graphics::\link[graphics]{points}}
#' @param ... Other arguments passed to \code{graphics::\link[graphics]{plot}}
#'
#' @return The plot
#'
#' @author Peter Hickey based on recommendation of Kasper Hansen
#'
#' @examples
#' spwt(rpois(10000, 5) + rnorm(10000, sd = 0.1), rpois(10000, 5) + rnorm(10000, sd = 0.1))
#'
#' @importFrom graphics plot
#' @importFrom scales alpha
#' @export
spwt <- function(x, y, col = scales::alpha("black", 0.01), pch = 16, cex = 0.5,
                ...) {
  plot(x, y, col = col, pch = pch, cex = cex, ...)
}