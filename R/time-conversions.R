
#' Convert h:m:s To/From seconds
#' 
#' `hmsToSec` - Converts a vector of h:m:s to seconds.
#' 
#' `secToHms` - Converts a vector of seconds to h:m:s.
#' 
#' 
#' @name time-conversions
#' @aliases hmsToMinute hmsToSec secToHms
#' 
#' @param x date-time object for `hmsToMinute()`, vector of times in
#' h:m:s format for `hmsToSec()`, or numeric vector of seconds for
#' `secToHms()`
#' @param digits number of decimal places for seconds, an integer from 0
#'   to 15, or `NULL`. The default uses zero places if all non-missing values
#'   have negligible fractional parts, otherwise two. Positive values round
#'   fractional seconds with carry into seconds, minutes and hours. For
#'   compatibility, `digits = 0` discards fractions using [floor()].
#' @return depending on the function:
#' \describe{
#'   \item{`hmsToMinute()`}{numeric vector of times in minutes}
#'   \item{`hmsToSec()`}{numeric vector of times in seconds}
#'   \item{`secToHms()`}{character vector of times in h:m:s format}
#' }
#'
#' Missing seconds produce `NA_character_`; infinite values are rejected.
#' Hours can exceed 23. Negative times retain the existing floor-based
#' decomposition (for example, -1 second is `-1:59:59`).
#'
#' @note Based on code by Tyler Rinker, adapted to conform to package standards. 
#' 
#' @examples
#' 
#' hmsToSec(c("02:00:03", "04:03:01"))
#' hmsToSec(secToHms(c(222, 1234, 55)))
#' secToHms(c(256, 3456, 56565))
#' 
#' 
#' @seealso [chron::times()]
#' 
#' @family date.time  
#' @concept date-time
#'
#'
#' @rdname time-conversions
#' @export
hmsToMinute <- function(x){
  hour(x)*60 + minute(x) + second(x)/60
}


#' @rdname time-conversions
#' @export
hmsToSec <- function(x) {
  
  hms <- as.character(x)
  # t(sapply(...)) always produces an n x 3 matrix, even for a single string.
  # The old sapply-over-data.frame returned a plain vector for length-1 input,
  # causing z[, 1] to fail with "incorrect number of dimensions".
  z <- t(sapply(strsplit(hms, ":"), as.numeric))
  z[, 1] * 3600 + z[, 2] * 60 + z[, 3]
}


#' @rdname time-conversions
#' @export
secToHms <- function(x, digits = NULL) {
  x <- as.numeric(x)
  if (any(!is.na(x) & !is.finite(x)))
    stop("'x' must contain finite seconds or NA")
  if (!is.null(digits) &&
      (!is.numeric(digits) || length(digits) != 1L ||
       !is.finite(digits) || digits < 0 || digits > 15 ||
       digits != floor(digits)))
    stop("'digits' must be NULL or a single whole number from 0 to 15")

  ans <- rep(NA_character_, length(x))
  keep <- !is.na(x)
  if (!any(keep)) return(ans)
  values <- x[keep]
  whole <- floor(values)
  fractions <- values - whole

  if (is.null(digits))
    digits <- if (all(fractions < sqrt(.Machine$double.eps))) 0 else 2

  suffix <- ""
  if (digits > 0) {
    scale <- 10^digits
    ticks <- round(fractions * scale)
    carry <- ticks >= scale
    whole <- whole + as.numeric(carry)
    ticks[carry] <- 0
    suffix <- paste0(".", sprintf("%0*.0f", as.integer(digits), ticks))
  }

  h <- floor(whole / 3600)
  remainder <- whole - h * 3600
  m <- floor(remainder / 60)
  s <- remainder - m * 60
  ans[keep] <- sprintf("%02.0f:%02.0f:%02.0f%s", h, m, s, suffix)
  ans
}
