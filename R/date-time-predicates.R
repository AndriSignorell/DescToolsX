
#' Date and Time Class Predicates
#'
#' Check whether an object contains a date component, a time component, or
#' both, and determine whether its observed times of day vary.
#'
#' The predicates inspect the class of \code{x}; character strings are not
#' parsed or converted. In addition to the base R classes \code{"Date"},
#' \code{"POSIXct"}, and \code{"POSIXlt"}, the legacy classes
#' \code{"dates"}, \code{"times"}, and \code{"chron"} are supported.
#'
#' \code{isDate} returns \code{TRUE} for objects containing a date component,
#' including date-time objects. \code{isTime} returns \code{TRUE} for objects
#' containing a time component, including date-time objects.
#' \code{isDateTime} identifies classes containing both components, and is
#' by construction equivalent to \code{isDate(x) && isTime(x)}.
#'
#' The \pkg{chron} classes need care, because \code{"dates"} inherits from
#' \code{"times"}: a pure date object therefore carries the \code{"times"}
#' class without having a time component. \code{isTime} accounts for this,
#' so that \code{"dates"} counts as date-only, \code{"times"} as time-only,
#' and \code{"chron"} as both.
#'
#' \code{hasVaryingTime} compares the observed times of day while ignoring
#' differences in the date component. It returns \code{FALSE} for objects
#' without a time component and when fewer than two non-missing times are
#' available.
#'
#' For date-time objects the comparison uses local clock time, which is
#' usually what matters when deciding whether times need to be displayed at
#' all. Where a daylight saving transition falls inside \code{x}, clock time
#' and elapsed time come apart: two instants an hour apart can share the same
#' clock time in the repeated hour of an autumn transition, and are then
#' reported as not varying.
#'
#' @name date-time-predicates
#'
#' @param x an object to examine
#' @param na.rm logical; if \code{TRUE}, missing values are removed before
#' the time components are compared (\code{hasVaryingTime} only)
#'
#' @return
#' for \code{isDate}, \code{isTime}, and \code{isDateTime}, a single logical
#' value. \code{hasVaryingTime} likewise returns a single logical value, or
#' \code{NA} if \code{x} contains missing values and \code{na.rm = FALSE}.
#'
#' @seealso
#' \code{\link[base]{inherits}}, \code{\link[base]{DateTimeClasses}}
#'
#' @family date.time
#' @concept date-time
#' @concept class-predicate
#'
#' @examples
#' d <- as.Date(c("2024-01-01", "2024-01-02"))
#' dt <- as.POSIXct(
#'   c("2024-01-01 08:00:00", "2024-01-02 09:30:00"),
#'   tz = "UTC"
#' )
#'
#' isDate(d)
#' isTime(d)
#' isDateTime(d)
#'
#' isDate(dt)
#' isTime(dt)
#' isDateTime(dt)
#'
#' # character strings are not interpreted as dates
#' isDate("2024-01-01")
#'
#' hasVaryingTime(dt)
#'
#' sameTime <- as.POSIXct(
#'   c("2024-01-01 08:00:00", "2024-01-02 08:00:00"),
#'   tz = "UTC"
#' )
#' hasVaryingTime(sameTime)
#'
#' # missing values propagate unless they are removed
#' withNA <- dt
#' withNA[2] <- NA
#' hasVaryingTime(withNA)
#' hasVaryingTime(withNA, na.rm = TRUE)
#'
#' @rdname date-time-predicates
#' @export
isDate <- function(x) {

  inherits(x, "Date") ||
    inherits(x, "POSIXt") ||
    inherits(x, "dates")

}


#' @rdname date-time-predicates
#' @export
isTime <- function(x) {

  # chron's "dates" inherits from "times", so a pure date would otherwise
  # be reported as carrying a time component. "chron" itself does have
  # both, and is matched before that exclusion applies.
  inherits(x, "POSIXt") ||
    inherits(x, "chron") ||
    (inherits(x, "times") && !inherits(x, "dates"))

}


#' @rdname date-time-predicates
#' @export
isDateTime <- function(x) {

  # Derived rather than restated, so the three predicates cannot drift
  # apart as classes are added.
  isDate(x) && isTime(x)

}


#' @rdname date-time-predicates
#' @export
hasVaryingTime <- function(x, na.rm = FALSE) {

  if(!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("Argument 'na.rm' must be a single non-missing logical value.")

  if(!isTime(x))
    return(FALSE)

  # Seconds since midnight in both branches. For POSIXt this is local
  # clock time, which sidesteps the rounding that format() would impose
  # via getOption("digits.secs").
  #
  # The two representations carry different error: POSIXlt holds seconds
  # directly, whereas chron stores a fractional day that is multiplied by
  # 86400 here, scaling its representation error with it. Each branch
  # therefore brings its own tolerance rather than sharing one that would
  # be too strict for chron or too coarse for POSIXt.
  if(inherits(x, "POSIXt")) {

    z <- as.POSIXlt(x)
    time <- z$hour * 3600 + z$min * 60 + z$sec
    tol <- sqrt(.Machine$double.eps)

  } else {

    time <- (as.numeric(x) %% 1) * 86400

    # A fractional day resolves to roughly 2e-11 s near 1.0, so a
    # microsecond sits several orders of magnitude above the noise while
    # staying far below any difference worth reporting.
    tol <- 1e-6

  }

  if(na.rm) {

    time <- time[!is.na(time)]

  } else if(anyNA(time)) {

    return(NA)

  }

  if(length(time) < 2L)
    return(FALSE)

  diff(range(time)) > tol

}
