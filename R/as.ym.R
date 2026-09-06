

#' A Class for Dealing with the Yearmonth Format
#'
#' Representing year and month information as an integer in YYYYMM format is
#' compact and efficient. Calendar arithmetic must nevertheless preserve the
#' date structure: for example, subtracting two months from 201201 should
#' return 201111 rather than 201199. [addMonths()] provides this
#' arithmetic for objects of class `"ym"`.
#'
#' All parameters are recycled if necessary, following the usual arithmetic
#' recycling rules; a warning is issued when the longer argument is not a
#' multiple of the length of the shorter one.
#'
#' @name as_ym
#' @aliases as.ym as.Date.ym print.ym
#' 
#' @param x a vector of integers, representing the dates in the format YYYYMM,
#' to which a number of months has to be added. YYYY must lie in the range of
#' 1000-3000, MM in 1-12. Values outside that range become `NA`.
#' @param d the day to be used for converting a yearmonth to a date. Default is
#' 1. Combinations that do not exist (e.g. 30 February) yield `NA`.
#' @param \dots further arguments; currently unused
#' 
#' @return
#' \describe{
#'   \item{`as.ym()`}{an integer vector of class `"ym"`}
#'   \item{`as.Date.ym()`}{a vector of class `"Date"`}
#'   \item{`addMonths.ym()`}{an integer vector of class `"ym"`}
#'   \item{`print.ym()`}{invisibly, `x`}
#' }
#'
#' @note Based on code by Roland Rapold, adapted to conform to package standards.
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
#' @family date-time
#' @export
as.ym <- function(x) {
  
  nm <- names(x)
  
  if (is.factor(x))
    x <- as.character(x)
  
  z <- suppressWarnings(as.numeric(x))
  
  res <- rep(NA_integer_, length(z))
  
  candidate <- !is.na(z) &
    is.finite(z) &
    z == trunc(z) &
    z >= 100001 &
    z <= 300012
  
  idx <- which(candidate)
  
  if (length(idx)) {
    y <- z[idx] %/% 100
    m <- z[idx] %% 100
    
    valid <- y >= 1000 & y <= 3000 &
      m >= 1 & m <= 12
    
    res[idx[valid]] <- as.integer(z[idx[valid]])
  }
  
  names(res) <- nm
  structure(res, class = "ym")
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

