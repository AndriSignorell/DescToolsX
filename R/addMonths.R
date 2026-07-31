

#' Add Months to a Date
#'
#' Naively adding months to a date can produce invalid dates, for example
#' when adding one month to 2012-01-30. \code{addMonths()} always returns a
#' valid date: adding one month to \code{as.Date("2013-01-31")} returns
#' \code{"2013-02-28"}. Negative values of \code{n} subtract months.
#'
#' All parameters are recycled if necessary. The result is always of class
#' \code{Date}, also when \code{x} is supplied as \code{POSIXct} or as a
#' character string; time-of-day information is therefore dropped. Note that
#' \code{\link{as.Date}} converts a \code{POSIXct} in UTC by default, which
#' can shift the calendar day - pass \code{tz} through \code{\dots} to
#' control this.
#'
#' Missing values in either argument propagate to \code{NA}.
#'
#' @param x a Date object, or an object coercible to one by
#' \code{\link{as.Date}}(x, ...), to which months are added
#' @param n number of months to add, a whole number. Negative values
#' subtract months.
#' @param \dots further arguments passed to \code{\link{as.Date}}, for example
#' to supply \code{origin} or \code{tz}
#' @return a vector of class \code{Date}, of the length of the longer of
#' \code{x} and \code{n}, containing the transformed dates
#' @note Based on code by Roland Rapold and Antonio, adapted to conform to package standards.
#'
#' @seealso \code{\link{as.ym}}; Date functions: \code{\link{year}},
#' \code{\link{month}}, etc.
#' @references
#' \url{https://stackoverflow.com/questions/14169620/add-a-month-to-a-date}
#'
#' @examples
#'
#' # characters will be coerced to Date
#' addMonths("2013-01-31", 1)
#'
#' # negative n
#' addMonths(as.Date("2013-03-31"), -1)
#'
#' # arguments will be recycled
#' # (with warning if the longer is not a multiple of length of shorter)
#' addMonths(c("2013-01-31", "2013-03-31", "2013-10-31", "2013-12-31"), c(1, -1))
#'
#' # missing values propagate
#' addMonths(c("2013-01-31", NA), 1)
#'
#' # POSIXct input is converted to Date first; supply tz to pin the day down
#' x <- as.POSIXct(c("2015-01-31", "2015-08-31"), tz = "UTC")
#' addMonths(x, c(1, 3), tz = "UTC")
#'
#' # mind the origin if x supplied as numeric ...
#' x <- as.numeric(as.Date(x))
#' addMonths(x, c(1, 3), origin = as.Date("1970-01-01"))
#'
#' @family date.time
#' @concept date-time
#' @export
addMonths <- function(x, n, ...) {
  UseMethod("addMonths")
}


#' @method addMonths default
#' @export
addMonths.default <- function(x, n, ...) {

  # ref: http://stackoverflow.com/questions/14169620/add-a-month-to-a-date
  # Author: Antonio
  .addMonths <- function(x, n) {

    # seq.Date() errors on a missing 'from', so NAs are caught up front
    # rather than aborting the whole call.
    if (is.na(x) || is.na(n))
      return(NA_real_)

    # naive shift - may roll over into the following month,
    # e.g. 2013-01-31 + 1 month -> 2013-03-03
    res <- unclass(seq(x, by = paste(n, "months"), length.out = 2L)[2L])

    # ceiling: the last day of the target month
    day(x) <- 1L
    resC <- unclass(seq(x, by = paste(n + 1L, "months"),
                        length.out = 2L)[2L]) - 1L

    min(res, resC)
  }

  x <- as.Date(x, ...)

  if (!is.numeric(n) || any(n %% 1 != 0, na.rm = TRUE))
    stop("'n' must be a whole number of months")

  if (length(x) == 0L || length(n) == 0L)
    return(x[0L])

  # mapply() supplies the recycling (and the mismatch warning documented
  # above); as.numeric() guards the all-NA case, where mapply() would
  # otherwise hand back a logical vector that must not be classed Date.
  res <- as.numeric(mapply(.addMonths, x, as.integer(n), USE.NAMES = FALSE))

  structure(res, class = "Date")
}
