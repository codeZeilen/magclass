#' extractSubdim
#'
#' Extracts the given dot-separated subdimension from a vector of dimNames. Elements with
#' fewer subdimensions than requested are returned unchanged, matching the behavior of the
#' regex this function replaces.
#'
#' @param dimNames A character vector of dot-separated dimNames.
#' @param subdim The subdimension to extract (1-based).
#' @return A character vector with the requested subdimension.
#' @author Jan Philipp Dietrich
extractSubdim <- function(dimNames, subdim = 1L) {
  if (subdim > 1L) {
    stripped <- sub(paste0("^([^.]*\\.){", subdim - 1L, "}"), "", dimNames, perl = TRUE)
    # elements with fewer subdimensions than requested are returned unchanged
    keep <- which(nchar(stripped) == nchar(dimNames))
  } else {
    stripped <- dimNames
    keep <- integer(0)
  }
  pos <- regexpr(".", stripped, fixed = TRUE)
  out <- substr(stripped, 1L, pos - 1L)
  noSep <- which(pos < 0L)
  out[noSep] <- stripped[noSep]
  out[keep] <- dimNames[keep]
  return(out)
}
