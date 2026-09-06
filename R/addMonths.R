
#' Add Months to a Date
#'
#' Adds or subtracts whole calendar months while ensuring that the result is
#' always a valid date.Negative values of `n` subtract months.
#'
#' Naively adding months to a date can produce invalid results. For example, adding one
#' month to `as.Date("2013-01-31")` returns `"2013-02-28"` rather
#' than a date in March. 
#'
#' @param x a `Date` object or an object coercible to one with
#'   [as.Date()]
#' @param n numeric vector containing finite whole numbers of months or
#'   missing values
#' @param \dots further arguments passed to [as.Date()], such as
#'   `origin`, `format`, or `tz`
#'
#' @return a vector of class `Date` having the length of the longer of
#'   `x` and `n`
#'
#' @details
#' The arguments `x` and `n` are recycled to their common maximum
#' length using [bedrock::recycle()]. Partial recycling does not produce a
#' warning.
#'
#' If either argument has length zero, an empty `Date` vector is
#' returned. Missing values in either argument produce missing values in the
#' corresponding result.
#'
#' Inputs supplied as `POSIXct` are converted to `Date`, and their
#' time-of-day information is discarded. By default, `as.Date.POSIXct`
#' performs this conversion in UTC. Supply `tz` through `\dots` if
#' another time zone should determine the calendar date.
#'
#' @note Based on code by Roland Rapold and Antonio, adapted to conform to
#'   package standards.
#'
#' @references
#' <https://stackoverflow.com/questions/14169620/add-a-month-to-a-date>
#'
#' @seealso [as.ym()], [year()], [month()]
#'
#' @examples
#' # character input is converted to Date
#' addMonths("2013-01-31", 1)
#'
#' # negative values subtract months
#' addMonths(as.Date("2013-03-31"), -1)
#'
#' # x and n are recycled
#' addMonths(
#'   c("2013-01-31", "2013-03-31", "2013-10-31", "2013-12-31"),
#'   c(1, -1)
#' )
#'
#' # missing values propagate
#' addMonths(c("2013-01-31", NA), 1)
#'
#' # POSIXct input is converted to Date
#' x <- as.POSIXct(c("2015-01-31", "2015-08-31"), tz = "UTC")
#' addMonths(x, c(1, 3), tz = "UTC")
#'
#' # numeric dates require an origin
#' x <- as.numeric(as.Date(c("2015-01-31", "2015-08-31")))
#' addMonths(x, c(1, 3), origin = as.Date("1970-01-01"))
#'
#' @family date.time
#' @concept date-time
#' @export
addMonths <- function(x, n, ...) {
  UseMethod("addMonths")
}




#' @rdname addMonths
#' @method addMonths default
#' @export
addMonths.default <- function(x, n, ...) {
  
  x <- as.Date(x, ...)
  
  if (!is.numeric(n))
    stop("'n' must be a numeric vector")
  
  invalidN <- !is.na(n) &
    (!is.finite(n) | n != trunc(n))
  
  if (any(invalidN))
    stop("'n' must contain finite whole numbers or NA")
  
  # seq.Date() parses the number of months as an integer.
  outsideRange <- !is.na(n) & abs(n) > .Machine$integer.max
  
  if (any(outsideRange))
    stop("'n' is outside the supported range")
  
  invalidX <- !is.na(x) & !is.finite(unclass(x))
  
  if (any(invalidX))
    stop("'x' must contain finite dates or NA")
  
  if (length(x) == 0L || length(n) == 0L)
    return(x[0L])
  
  arg <- recycle(x = x, n = n)
  maxdim <- attr(arg, "maxdim")
  
  res <- vapply(
    seq_len(maxdim),
    function(i) {
      .addMonthsEngine(
        x = arg$x[i],
        n = arg$n[i]
      )
    },
    numeric(1L)
  )
  
  return(structure(res, class = "Date"))
}


#' @rdname addMonths
#' @method addMonths ym
#' @export
addMonths.ym <- function(x, n, ...) {
  
  if (!is.numeric(n) ||
      any(!is.na(n) & (!is.finite(n) | n %% 1 != 0)))
    stop("'n' must contain whole finite numbers or NA")
  
  idx <- unclass(x) %/% 100 * 12 +
    (unclass(x) %% 100 - 1) +
    n
  
  res <- idx %/% 12 * 100 + idx %% 12 + 1
  
  as.ym(res)
}



#' @export
`+.ym` <- function(e1, e2) {
  
  if (missing(e2))
    return(e1)
  
  if (inherits(e1, "ym") && inherits(e2, "ym"))
    stop("two 'ym' objects cannot be added")
  
  if (inherits(e1, "ym"))
    return(addMonths(e1, e2))
  
  if (inherits(e2, "ym"))
    return(addMonths(e2, e1))
  
  stop("one operand must be a 'ym' object")
}


#' @export
`-.ym` <- function(e1, e2) {
  if (missing(e2))
    stop("unary '-' is not defined for 'ym' objects")
  if (inherits(e2, "ym"))
    stop("use difference in months explicitly; '-' expects a number of months")
  addMonths(e1, -e2)
}



## == internal helper functions ================================================

.addMonthsEngine <- function(x, n) {
  
  if (is.na(x) || is.na(n))
    return(NA_real_)
  
  if (n == 0)
    return(unclass(x))
  
  originalDay <- day(x)
  day(x) <- 1L
  
  by <- paste(
    format(n, scientific = FALSE, trim = TRUE),
    "months"
  )
  
  targetMonth <- seq(
    from = x,
    by = by,
    length.out = 2L
  )[2L]
  
  nextMonth <- seq(
    from = targetMonth,
    by = "1 month",
    length.out = 2L
  )[2L]
  
  daysInTargetMonth <- as.integer(nextMonth - targetMonth)
  targetDay <- min(originalDay, daysInTargetMonth)
  
  return(unclass(targetMonth + targetDay - 1L))
}


