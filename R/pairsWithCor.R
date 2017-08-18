# NOTE: Helper adapted from example in graphics::pairs()
panel.cor <- function(x, y, use = "complete.obs", method = "pearson",
                      digits = 2, prefix = "",
                      cex.cor, ...) {
  usr <- graphics::par("usr")
  on.exit(graphics::par(usr))
  graphics::par(usr = c(0, 1, 0, 1))
  r <- abs(stats::cor(x, y, use = use, method = method))
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if (missing(cex.cor)) {
    cex.cor <- 0.8 / graphics::strwidth(txt)
  }
  graphics::text(0.5, 0.5, txt, cex = cex.cor * r)
}

#' Scatterplot Matrices with Correlation
#'
#' A wrapper around `graphics::pairs()` that includes correlation estimates on
#' the lower diagonal and a [stats::lowess()] smooth of the scatterplots on the
#' upper triangle with [graphics::panel.smooth()].
#' @inherit graphics::pairs
#' @inheritParams stats::cor
#'
#' @export
#' @examples
#' pairsWithCor(USJudgeRatings)
# TODO: Figure out how to pass `use` and `method` down to panel.cor()
pairsWithCor <- function(x, use = "everything",
                         method = c("pearson", "kendall", "spearman"), ...) {
  graphics::pairs(x,
                  ...,
                  upper.panel = graphics::panel.smooth,
                  lower.panel = panel.cor)
}
