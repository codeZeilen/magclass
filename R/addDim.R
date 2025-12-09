#' addDim
#'
#' This function adds a named dimension as dimension number "dim" with
#' the name "dimName" with an empty data column with the name "item".
#'
#' @param x MAgPIE object which should be extended.
#' @param dim The dimension number of the new dimension (e.g. 3.1)
#' @param dimName The name of the new dimension
#' @param item One or more names of items in the new dimension. If more than one
#' is given, behavior depends on the expand argument.
#' @param expand If TRUE, each item from the item argument is added to each item
#' already present, resulting in e.g. `c("A.d1", "B.d1", "A.d2", "B.d2")`.
#' Otherwise, length of item must equal the number of items already present and
#' they are simply added, resulting in e.g. `c("A.d1", "B.d2")`.
#'
#' @return The extended MAgPIE object
#' @author Jan Philipp Dietrich, Benjamin Bodirsky, Pascal Sauer
#' @seealso \code{\link{add_columns}},\code{\link{mbind}}
#' @examples
#'
#' a <- maxample("animal")
#' str(addDim(a, dim = 3.2))
#' str(addDim(a, dim = 2.3, item = paste0("d", 1:3)))
#' @export
addDim <- function(x, dim = 3.1, dimName = NULL, item = "dummy", expand = TRUE) {
  x <- clean_magpie(x, what = "sets")
  if (is.null(dimName)) {
    # create non-existing variant of dimension name starting with "new"
    sets <- getSets(x, fulldim = TRUE)
    dimName <- tail(make.unique(c(sets, "new"), sep = ""), 1)
  } else if (dimName %in% getSets(x, fulldim = TRUE)) {
    stop("Dimension \"", dimName, "\" does already exist. Please use a different name!")
  }
  maindim <- floor(dim)
  subdim  <- as.integer(sub("^.\\.", "", as.character(dim)))
  if (length(item) > 1) {
    if (expand) {
      expansion <- rep(seq_len(dim(x)[maindim]), length(item))
      x <- x[expansion, dim = maindim]
      item <- rep(item, each = dim(x)[maindim] / length(item))
    } else if (length(item) != dim(x)[maindim]) {
      stop("length(item) != number of items already present in x in dim ", maindim,
           "; actual numbers: ", length(item), "!=", dim(x)[maindim])
    }
  }
  if (is.null(getItems(x, dim = maindim))) {
    getItems(x, dim = maindim, raw = TRUE) <- item
    getSets(x, fulldim = FALSE)[maindim] <- dimName
  } else if (subdim == 1) {
    getItems(x, dim = maindim, raw = TRUE) <- paste0(item, ".", getItems(x, dim = maindim, full = TRUE))
    getSets(x, fulldim = FALSE)[maindim] <- paste0(dimName, ".", getSets(x, fulldim = FALSE)[maindim])
  } else if (subdim > ndim(x, maindim)) {
    getItems(x, dim = maindim, raw = TRUE) <- paste0(getItems(x, dim = maindim, full = TRUE), ".", item)
    getSets(x, fulldim = FALSE)[maindim] <- paste0(getSets(x, fulldim = FALSE)[maindim], ".", dimName)
  } else {
    # this else branch can solve any case, the previous 3 are just faster implementations for common special cases
    items <- getItems(x, dim = maindim, split = TRUE, full = TRUE)
    before <- seq_len(subdim - 1)
    after <- setdiff(seq_along(items), before)
    items <- c(items[before], list(item), items[after])
    names(items)[subdim] <- dimName
    items <- Filter(Negate(is.null), items)
    getItems(x, dim = maindim, raw = TRUE) <- do.call(function(...) paste(..., sep = "."), items)
    getSets(x, fulldim = FALSE)[maindim] <- paste(names(items), collapse = ".")
  }
  return(x)
}

#' add_dimension
#'
#' This function adds a named dimension as dimension number "dim" with
#' the name "add" with an empty data column with the name "nm".
#' Please start using this function's successor \code{\link{addDim}}. add_dimension
#' might eventually become deprecated.
#'
#' @inherit addDim
#' @param add The name of the new dimension
#' @param nm One or more names of items in the new dimension.
#' @export
add_dimension <- function(x, dim = 3.1, add = NULL, nm = "dummy", expand = TRUE) { # nolint: object_name_linter.
  return(addDim(x = x, dim = dim, dimName = add, item = nm, expand = expand))
}
