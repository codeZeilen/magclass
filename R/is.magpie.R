#' @family ObjectInfo
#' @importFrom methods is
#' @export
is.magpie <- function(x) { # nolint: object_name_linter.
  return(is(x, "magpie"))
}
