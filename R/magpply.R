#' @title magpply
#' @description apply command for magpieobjects. Very efficient for replacing loops.
#'
#' @param X magpie object
#' @param FUN function that shall be applied X
#' @param MARGIN dimension over which FUN shall be applied (like a loop over that dimension).
#' This dimension will be preserved in the output object (see also \code{DIM}).
#' @param DIM dimension in which FUN shall be applied. This dimension will be missing in the output. DIM and MARGIN
#' are opposite ways of expressing the dimensions to be addressed and you must only use one of them with MARGIN
#' excluding dimensions from the calculation and DIM including them.
#' @param ... further parameters passed on to FUN
#' @param INTEGRATE if TRUE, the output will be filled into an magpie object of the same dimensionality as X
#' @return magpie object
#'
#' @author Jan Philipp Dietrich, Benjamin Leon Bodirsky
#' @examples
#' pop <- maxample("pop")
#' magpply(pop, FUN = sum, MARGIN = 2)
#' fourdim <- pop * setNames(pop, c("jkk", "lk"))
#' magpply(fourdim, FUN = sum, MARGIN = c(1, 3.1))
#' @family SelectionCalculation
#' @export
magpply <- function( # nolint: cyclocomp_linter.
  X, FUN, MARGIN = NULL, DIM = NULL, ..., INTEGRATE = FALSE # nolint: object_name_linter.
) {
  x <- X
  margin <- MARGIN
  dim <- DIM
  if (!is.magpie(x)) {
    stop("Input is not a MAgPIE object!")
  }
  if (!is.null(margin) && !is.null(dim)) {
    stop("Please specify either MARGIN or DIM, not both at the same time!")
  }
  if (!is.null(margin)) {
    # converting margin to dim
    margin <- dimCode(margin, x)
    dim <- NULL
    for (i in 1:3) {
      if (!is.element(i, floor(margin))) {
        dim <- c(dim, i)
      } else if (is.element(i, floor(margin)) && !is.element(i, margin)) {
        dim <- c(dim, setdiff(i + seq_len(ndim(x, dim = i)) / 10, margin))
      }
    }
  }
  dim <- sort(dimCode(dim, x), decreasing = TRUE)
  if (any(dim == 0)) {
    stop("Invalid dimension(s) specified")
  }
  if (length(x) == 0) {
    return(NULL)
  }
  if (INTEGRATE) {
    xIn <- x
  }
  for (d in dim) {
    getItems(x, dim = d, raw = TRUE) <- NULL
  }
  noNames <- which(vapply(dimnames(x), is.null, logical(1)))
  for (i in noNames) {
    getItems(x, dim = i) <- rep("dummy", dim(x)[i])
  }
  xd <- as.data.frame.table(x)
  out <- new("magpie", tapply(xd[[4]], xd[1:3], FUN, ...))
  for (i in noNames) {
    getItems(out, dim = i) <- NULL
  }
  if (INTEGRATE) {
    out <- magpie_expand(out, xIn)
  }
  return(out)
}
