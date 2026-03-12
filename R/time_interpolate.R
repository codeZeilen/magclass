#' time_interpolate
#'
#' Function to extrapolate missing years in MAgPIE objects.
#'
#'
#' @param dataset An MAgPIE object
#' @param interpolated_year Vector of years, of which values are required. Can
#' be in the formats 1999 or y1999.
#' @param integrate_interpolated_years FALSE returns only the dataset of the
#' interpolated year, TRUE returns the whole dataset, including all years of
#' data and the itnerpolated year
#' @param extrapolation_type Determines what happens if extrapolation is
#' required, i.e. if a requested year lies outside the range of years in
#' \code{dataset}. Specify "linear" for a linear extrapolation. "constant" uses
#' the value from dataset closest in time to the requested year.
#' @return Uses linear extrapolation to estimate the values of the interpolated
#' year, using the values of the two surrounding years. If the value is before
#' or after the years in data, the two closest neighbours are used for
#' extrapolation.
#' @author Benjamin Bodirsky, Jan Philipp Dietrich
#' @seealso \code{\link{convergence}}
#' @examples
#'
#' p <- maxample("pop")
#' time_interpolate(p, "y2000", integrate = TRUE)
#' time_interpolate(p, c("y1980", "y2000"), integrate = TRUE, extrapolation_type = "constant")
#' @family TemporalOperations
#' @importFrom abind abind
#' @export
time_interpolate <- function(dataset, interpolated_year, # nolint: object_name_linter.
                             integrate_interpolated_years = FALSE, # nolint: object_name_linter.
                             extrapolation_type = "linear") { # nolint: object_name_linter.
  if (!is.magpie(dataset)) {
    stop("Invalid data format of measured data. Has to be a MAgPIE-object.")
  }
  interpolatedYear <- interpolated_year
  dataset <- clean_magpie(dataset, what = "sets")
  sets <- getSets(dataset)
  if (all(isYear(interpolatedYear, with_y = FALSE))) {
    interpolatedYear <- paste("y", interpolatedYear, sep = "")
  } else  {
    if (!any(isYear(interpolatedYear, with_y = TRUE))) {
      stop("year not in the right format")
    }
  }

  if (nyears(dataset) == 1) {
    tmp <- dataset
    dimnames(tmp)[[2]] <- "y0000"
    dataset <- mbind(tmp, dataset)
  }

  interpolatedYearFiltered <- interpolatedYear[!interpolatedYear %in% getYears(dataset)]
  datasetInterpolated       <- array(NA,
    dim = c(dim(dataset)[1], length(interpolatedYearFiltered), dim(dataset)[3]),
    dimnames = list(getCells(dataset), interpolatedYearFiltered, getNames(dataset))
  )
  dataset <- as.array(dataset)


  for (singleInterpolatedYear in interpolatedYearFiltered) {
    sortedYears                <-  sort(c(dimnames(dataset)[[2]], singleInterpolatedYear))
    if (sortedYears[1] == singleInterpolatedYear) {
      yearBefore <- sortedYears[2]
      yearAfter  <- sortedYears[3]
      yearExtrapolate <- ifelse(extrapolation_type == "constant", sortedYears[2], -1)
    } else if (sortedYears[length(sortedYears)] == singleInterpolatedYear) {
      yearBefore <- sortedYears[length(sortedYears) - 2]
      yearAfter  <- sortedYears[length(sortedYears) - 1]
      yearExtrapolate <- ifelse(extrapolation_type == "constant", sortedYears[length(sortedYears) - 1], -1)
    } else {
      yearBefore <- sortedYears[which(sortedYears == singleInterpolatedYear) - 1]
      yearAfter <- sortedYears[which(sortedYears == singleInterpolatedYear) + 1]
      yearExtrapolate <- -1
    }

    interpolatedYearInt       <- as.integer(substring(singleInterpolatedYear, 2))
    yearBeforeInt             <- as.integer(substring(yearBefore, 2))
    yearAfterInt              <- as.integer(substring(yearAfter, 2))

    datasetDifference        <-  dataset[, yearAfter, , drop = FALSE] - dataset[, yearBefore, , drop = FALSE]
    yearBeforeToAfter        <-  yearAfterInt        - yearBeforeInt
    yearBeforeToInterpolated <-  interpolatedYearInt - yearBeforeInt


    if (yearExtrapolate == -1) {
      datasetInterpolated[, singleInterpolatedYear, ] <- dataset[, yearBefore, , drop = FALSE] +
        yearBeforeToInterpolated * datasetDifference /
          yearBeforeToAfter
    } else {
      datasetInterpolated[, singleInterpolatedYear, ] <- dataset[, yearExtrapolate, , drop = FALSE]
    }
  }
  if (integrate_interpolated_years == FALSE) {
    addYears <- setdiff(interpolatedYear, interpolatedYearFiltered)
    if (length(addYears) > 0) {
      dataset <- abind::abind(datasetInterpolated, dataset[, addYears, , drop = FALSE], along = 2)
    } else {
      dataset <- datasetInterpolated
    }
  } else {
    if (any(getYears(dataset) == "y0000")) {
      dataset <- dataset[, -which(getYears(dataset) == "y0000"), , drop = FALSE]
    }
    dataset <- abind::abind(dataset, datasetInterpolated, along = 2)
  }
  dataset <- as.magpie(dataset, spatial = 1, temporal = 2)
  dataset <- dataset[, sort(getYears(dataset)), ]
  getSets(dataset) <- sets
  return(dataset)
}
