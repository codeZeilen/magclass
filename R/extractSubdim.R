#' extractSubdim
#'
#' Extracts the given dot-separated subdimension from a vector of dimnames. Elements with
#' fewer subdimensions than requested are returned unchanged, matching the behavior of the
#' regex this function replaces.
#'
#' @param x A character vector of dot-separated dimnames.
#' @param subdim The subdimension to extract (1-based).
#' @return A character vector with the requested subdimension.
#' @author Jan Philipp Dietrich
extractSubdim <- function(x, subdim = 1L) {
  if (subdim > 1L) {
    stripped <- sub(paste0("^([^.]*\\.){", subdim - 1L, "}"), "", x, perl = TRUE)
    # elements with fewer subdimensions than requested are returned unchanged
    keep <- which(nchar(stripped) == nchar(x))
  } else {
    stripped <- x
    keep <- integer(0)
  }
  pos <- regexpr(".", stripped, fixed = TRUE)
  out <- substr(stripped, 1L, pos - 1L)
  noSep <- which(pos < 0L)
  out[noSep] <- stripped[noSep]
  out[keep] <- x[keep]
  return(out)
}
