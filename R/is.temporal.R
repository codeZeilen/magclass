#' is.temporal, is.spatial
#'
#' Functions to find out whether a vector consists of strings consistent with
#' the definition for auto-detection of temporal or spatial data.
#'
#'
#' @aliases is.temporal is.spatial
#' @param x A vector
#' @return Returns TRUE or FALSE
#' @author Jan Philipp Dietrich
#' @examples
#'
#' is.temporal(1991:1993)
#' is.spatial(c("GLO", "AFR"))
#'
#' @family ObjectInfo
#' @export
is.temporal <- function(x) { # nolint: object_name_linter.
  return(length(grep("^[a-z]?[0-9]{4}$", x)) == length(x))
}
