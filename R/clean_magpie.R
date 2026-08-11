#' MAgPIE-Clean
#'
#' Function cleans MAgPIE objects so that they follow some extended magpie
#' object rules (currently it makes sure that the dimnames have names and
#' removes cell numbers if it is purely regional data)
#'
#'
#' @param x MAgPIE object which should be cleaned.
#' @param what term defining what type of cleaning should be performed. Current
#' modes are "cells" (removes cell numbers if the data seems to be regional -
#' this should be used carefully as it might remove cell numbers in some cases
#' in which they should not be removed), "sets" (making sure that all
#' dimensions have names), "items" (replace empty elements with single spaces " ")
#' and "all" (performing all available cleaning methods)
#' @param maindim main dimension(s)  the cleaning should get applied to.
#' @return The eventually corrected MAgPIE object
#' @author Jan Philipp Dietrich
#' @seealso \code{"\linkS4class{magpie}"}
#' @examples
#'
#' pop <- maxample("pop")
#' a <- clean_magpie(pop)
#' @family ObjectCreation
#' @export
clean_magpie <- function(x, what = "all", maindim = 1:3) { # nolint: object_name_linter, cyclocomp_linter.
  availableTypes <- c("cells", "items", "sets")
  if ("all" %in% what) what <- availableTypes
  if (any(!is.element(what, availableTypes))) stop('Unknown setting for argument what ("', what, '")!')

  # remove cell numbers if data is actually regional
  if ("cells" %in% what) {
    cells <- dimnames(x)[[1]]
    if (!is.null(cells)) {
      hasSubdims <- grepl(".", cells[1], fixed = TRUE)
      # nothing to remove unless there are subdimensions or stray element names
      if (hasSubdims || !is.null(names(cells))) {
        regions <- if (hasSubdims) extractSubdim(cells) else cells
        if (!anyDuplicated(regions)) {          # equivalent to ncells(x) == nregions(x)
          names(regions) <- NULL
          dimnames(x)[[1]] <- regions
          if (hasSubdims && !is.null(names(dimnames(x)))) {
            names(dimnames(x))[1] <- extractSubdim(names(dimnames(x))[1])
          }
        }
      }
    }
  }
  # make sure that all dimensions have names
  if ("sets" %in% what) {
    dn <- dimnames(x)
    if (is.null(names(dn))) names(dn) <- rep(NA, 3)

    # counts dots via codepoint comparison; much cheaper than a regex for these short strings
    .countSubdim <- function(x) {
      if (length(x) == 0) return(0)
      sum(utf8ToInt(x) == utf8ToInt(".")) + 1L
    }

    .fixNames <- function(names, ndim, key = "data") {
      if (is.na(names) || names == "" || names == "NA") {
        tmp <- rep(key, max(1, ndim))
        names <- paste(make.unique(tmp, sep = ""), collapse = ".")
      } else {
        cdim <- .countSubdim(names)
        if (ndim != cdim) {
          if (ndim > cdim) {
            names <- paste(c(names, rep(key, ndim - cdim)), collapse = ".")
          } else {
            search <- paste0(c(rep("\\.[^\\.]*", cdim - ndim), "$"), collapse = "")
            names <- sub(search, "", names)
          }
          names <- paste0(make.unique(strsplit(names, "\\.")[[1]], sep = ""), collapse = ".")
        }
      }
      return(names)
    }

    names <- names(dn)
    keys <- c("region", "year", "data")

    for (i in maindim) {
      names[i] <- .fixNames(names[i], ndim = .countSubdim(dn[[i]][1]), key = keys[i])
    }
    names(dimnames(x)) <- names
  }

  if ("items" %in% what) {
    .fixEmptySubims <- function(x, dim) {
      items <- dimnames(x)[[dim]]
      if (is.null(items)) return(x)
      # cheap fixed-string pre-check; the regex below is considerably more expensive
      if (!(any(!nzchar(items), na.rm = TRUE) || any(startsWith(items, "."), na.rm = TRUE) ||
              any(endsWith(items, "."), na.rm = TRUE) || any(grepl("..", items, fixed = TRUE), na.rm = TRUE))) {
        return(x)
      }
      pattern <- "(^|\\.)(\\.|$)"
      while (any(grepl(pattern, items))) {
        items <- gsub(pattern, "\\1 \\2", items, perl = TRUE)
      }
      dimnames(x)[[dim]] <- items
      return(x)
    }
    for (i in maindim) {
      x <- .fixEmptySubims(x, dim = i)
    }
  }

  return(x)
}
