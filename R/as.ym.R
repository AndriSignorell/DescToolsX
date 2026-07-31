

#' A Class for Dealing with the Yearmonth Format
#'
#' Representing year and month information as an integer in YYYYMM format is
#' compact and efficient. Calendar arithmetic must nevertheless preserve the
#' date structure: for example, subtracting two months from 201201 should
#' return 201111 rather than 201199. \code{addMonths()} provides this
#' arithmetic for objects of class \code{"ym"}.
#'
#' All parameters are recycled if necessary, following the usual arithmetic
#' recycling rules; a warning is issued when the longer argument is not a
#' multiple of the length of the shorter one.
#'
#' @name as_ym
#' @aliases as.ym as.Date.ym addMonths.ym print.ym
#' @param x a vector of integers, representing the dates in the format YYYYMM,
#' to which a number of months has to be added. YYYY must lie in the range of
#' 1000-3000, MM in 1-12. Values outside that range become \code{NA}.
#' @param d the day to be used for converting a yearmonth to a date. Default is
#' 1. Combinations that do not exist (e.g. 30 February) yield \code{NA}.
#' @param n the number of months to be added. If n is negative the months will
#' be subtracted.
#' @param \dots further arguments; currently unused
#' @return
#' \describe{
#'   \item{\code{as.ym()}}{an integer vector of class \code{"ym"}}
#'   \item{\code{as.Date.ym()}}{a vector of class \code{"Date"}}
#'   \item{\code{addMonths.ym()}}{an integer vector of class \code{"ym"}}
#'   \item{\code{print.ym()}}{invisibly, \code{x}}
#' }
#'
#' @note Based on code by Roland Rapold, adapted to conform to package standards.
#'
#' @seealso \code{\link{addMonths}}; Date functions, like \code{\link{year}},
#' \code{\link{month}}, etc.
#'
#' @examples
#'
#' month(as.ym(202408))
#' year(as.ym(202408))
#'
#' year(as.Date("2024-12-05"))
#' year(as.ym(202412))
#'
#' month(as.Date("2024-12-05"), fmt = "mm")
#' month(as.ym(202412), fmt = "mm")
#'
#' # arithmetic stays in the ym domain, so it can be chained
#' addMonths(as.ym(201511), 5)
#' as.ym(201511) + 5 - 2
#'
#' addMonths(as.ym(c(201511, 201302)), c(5, 15))
#' addMonths(as.ym(c(201511, 201302)), c(5, -4))
#'
#' # out-of-range input is flagged, not silently mangled
#' as.ym(c(201513, 999901))
#'
#' @family date.time
#' @concept date-time
#' @export
as.ym <- function(x) {

  x <- suppressWarnings(as.integer(x))

  y <- x %/% 100L
  m <- x %% 100L

  # is.na() first: a logical index containing NA is an error in
  # subassignment, so the invalid positions must be resolved to FALSE/TRUE.
  bad <- is.na(x) | y < 1000L | y > 3000L | m < 1L | m > 12L
  x[bad] <- NA_integer_

  # "num" was never a real class - the vector is an integer carrying the
  # single class "ym", and print.ym() unclasses it for display.
  structure(x, class = "ym")
}


#' @rdname as_ym
#' @method as.Date ym
#' @export
as.Date.ym <- function(x, d = 1, ...) {

  if (!is.numeric(d) || length(d) != 1L || is.na(d) ||
      d < 1 || d > 31 || d %% 1 != 0)
    stop("'d' must be a single whole number between 1 and 31")

  x <- unclass(x)

  res <- rep(NA_character_, length(x))
  ok  <- !is.na(x)
  res[ok] <- sprintf("%04d-%02d-%02d", x[ok] %/% 100L, x[ok] %% 100L,
                     as.integer(d))

  as.Date(res, format = "%Y-%m-%d")
}


#' @rdname as_ym
#' @method print ym
#' @export
print.ym <- function(x, ...) {
  # do not print the class attributes
  print(unclass(x), ...)
  invisible(x)
}


#' @rdname as_ym
#' @method addMonths ym
#' @export
addMonths.ym <- function(x, n, ...) {

  if (!is.numeric(n) || any(n %% 1 != 0, na.rm = TRUE))
    stop("'n' must be a whole number of months")

  # Straight month arithmetic on a linear month index. This replaces the
  # per-element mapply()/branch construction, which
  #   * left 'res' undefined - and thus errored - for any value outside
  #     the two hard-coded ranges,
  #   * carried a YYYYMMDD branch that as.ym() can never produce, and
  #   * returned a bare numeric, so a ym lost its class after one
  #     operation and the next '+' silently fell through to the Date
  #     method.
  # NA propagates by itself, so no special-casing is needed.
  idx <- unclass(x) %/% 100L * 12L + (unclass(x) %% 100L - 1L) + as.integer(n)

  structure(as.integer(idx %/% 12L * 100L + idx %% 12L + 1L), class = "ym")
}


#' @export
`+.ym` <- function(e1, e2) {
  if (missing(e2))
    return(e1)
  if (inherits(e2, "ym"))
    stop("two 'ym' objects cannot be added")
  addMonths(e1, e2)
}

#' @export
`-.ym` <- function(e1, e2) {
  if (missing(e2))
    stop("unary '-' is not defined for 'ym' objects")
  if (inherits(e2, "ym"))
    stop("use difference in months explicitly; '-' expects a number of months")
  addMonths(e1, -e2)
}
