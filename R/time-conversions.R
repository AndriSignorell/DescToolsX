
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
#' @param digits the number of digits to use for potential fractions of
#' seconds
#' @return depending on the function:
#' \describe{
#'   \item{`hmsToMinute()`}{numeric vector of times in minutes}
#'   \item{`hmsToSec()`}{numeric vector of times in seconds}
#'   \item{`secToHms()`}{character vector of times in h:m:s format}
#' }
#' #' 
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
secToHms <- function(x, digits=NULL) {
  
  x <- as.numeric(x)
  
  h <- floor(x/3600)
  m <- floor((x-h*3600)/60)
  s <- floor(x-(m*60 + h*3600))
  b <- x-(s + m*60 + h*3600)
  
  if(is.null(digits)) digits <- ifelse(all(b < sqrt(.Machine$double.eps)),0, 2)
  if(digits==0) f <- "" else f <- gettextf(paste(".%0", digits, "d", sep=""), round(b*10^digits, 0))
  
  gettextf("%02d:%02d:%02d%s", h, m, s, f)
  
}
