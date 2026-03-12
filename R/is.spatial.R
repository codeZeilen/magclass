#' @family ObjectInfo
#' @export
is.spatial <- function(x) { # nolint: object_name_linter.
  return(length(grep("^(([A-Z]{3})|(glob)|([A-Z]+[\\._][0-9]+))$", x)) == length(x))
}
