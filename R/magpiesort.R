#' MAgPIE-Sort
#'
#' Brings the spatial and temporal structure of MAgPIE objects in the right
#' order. This function is especially useful when you create new MAgPIE objects
#' as the order typically should be correct for MAgPIE objects.
#'
#' @param x MAgPIE object which might not be in the right order.
#' @return The eventually corrected MAgPIE object (right order in spatial in
#' temporal dimension)
#'
#' @author Jan Philipp Dietrich
#' @seealso \code{"\linkS4class{magpie}"}
#' @examples
#' pop <- maxample("pop")
#' a <- magpiesort(pop)
#' @family DataBinding
#' @export
magpiesort <- function(x) {
  if (!is.magpie(x)) {
    stop("Input is not a MAgPIE object!")
  }

  if (dim(x)[1] == 1) {
    spatialOrder <- 1
  } else if (length(grep("\\.[0-9]*$", dimnames(x)[[1]])) == dim(x)[1]) {
    spatialOrder <- order(as.numeric(gsub("^.*\\.", "", dimnames(x)[[1]])))
  } else {
    spatialOrder <- order(dimnames(x)[[1]])
  }

  if (!is.null(dimnames(x)[[2]])) {
    temporalOrder <- order(dimnames(x)[[2]])
  } else {
    temporalOrder <- seq_len(dim(x)[2])
  }

  return(x[spatialOrder, temporalOrder, ])
}
