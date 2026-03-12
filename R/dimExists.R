#' dimExists
#'
#' This function checks whether a dimension given by name or number exists.
#'
#' @param dim A vector of dimension numbers or dimension names which should be
#' checked for.
#' @param x MAgPIE object in which the dimensions should be searched for.
#' @param sep A character separating joined dimension names.
#' @return Boolean indicating whether the dimension exists or not.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link{dimCode}}
#' @examples
#'
#' pop <- maxample("pop")
#' dimExists(c("t", "scenario", "blablub"), pop)
#'
#' @family ObjectInfo
#' @export
dimExists <- function(dim, x, sep = ".") {
  if (is.null(dim)) {
    return(FALSE)
  }
  if (length(dim) > 1) {
    return(vapply(dim, dimExists, x, sep = sep, FUN.VALUE = logical(1)))
  }

  d <- dimCode(dim, x, sep = sep)
  if (d == 0) {
    return(FALSE)
  }
  if (d %in% c(1:3, (1:3) + 0.1)) {
    return(TRUE)
  }

  maindim <- as.integer(d)
  subdim <- as.integer(substring(d, 3))

  maxsubdim <- nchar(gsub(paste0("[^\\", sep, "]*"), "", dimnames(x)[[maindim]][1])) + 1
  return(subdim <= maxsubdim)
}
