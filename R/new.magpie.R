#' new.magpie
#'
#' Creates a new MAgPIE object
#'
#'
#' @param cells_and_regions Either the region names (e.g. "AFR"), or the cells
#' (e.g. 1:10), or both in combination (e.g. "AFR.1"). NULL means no spatial
#' element.
#' @param years dimnames for years in the format "yXXXX" or as integers. NULL
#' means one year which is not further specified
#' @param names dimnames for names. NULL means one data element which is not
#' further specified
#' @param fill Default value for the MAgPIE object
#' @param sort Bolean. Decides, wheher output should be sorted or not.
#' @param sets A vector of dimension names. See \code{\link{getSets}} for more
#' information.
#' @param unit deprecated
#' @return an empty magpie object filled with fill, with the given dimnames
#' @author Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso \code{\link{as.magpie}}
#' @examples
#'
#' a <- new.magpie(1:10, 1995:2000)
#' b <- new.magpie(c("AFR", "CPA"), "y1995", c("bla", "blub"), sets = c("i", "t", "value"))
#' c <- new.magpie()
#' @family ObjectCreation
#' @export
#' @importFrom methods new
new.magpie <- function(cells_and_regions = "GLO", # nolint: object_name_linter.
                       years = NULL, names = NULL, fill = NA, sort = FALSE, sets = NULL, unit = NULL) {
  cellsAndRegions <- cells_and_regions
  if (!is.null(unit)) warning("Argument \"unit\" is deprecated and will be ignored!")
  ncells <- length(cellsAndRegions)
  nyears <- ifelse(is.null(years), 1, length(years))
  ndata  <- ifelse(is.null(names), 1, length(names))
  if (all(!grepl("\\.", cellsAndRegions)) && !is.null(cellsAndRegions)) {
    if (all(is.numeric(cellsAndRegions))) {
      cellsAndRegions <- paste("GLO", cellsAndRegions, sep = ".")
    }
  }
  if (all(is.numeric(years)) && all(nchar(years) <= 4)) {
    years <- gsub(" ", "0", format(years, width = 4))
    years <- paste("y", years, sep = "")
  }

  object <- new("magpie", array(fill, dim = c(ncells, nyears, ndata)))
  getItems(object, dim = 1, raw = TRUE) <- as.vector(cellsAndRegions)
  getItems(object, dim = 2, raw = TRUE) <- as.vector(years)
  getItems(object, dim = 3, raw = TRUE) <- as.vector(names)
  names(dimnames(object)) <- NULL
  if (sort) object <- magpiesort(object)
  object <- clean_magpie(object, "sets")
  if (!is.null(sets)) getSets(object) <- sets
  return(object)
}
