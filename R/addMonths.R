

#' Add a Month to a Date
#' 
#' Clueless adding numbers of months to a date will in some cases lead to
#' invalid dates, think of e.g. 2012-01-30 + 1 month. \cr AddMonths ensures
#' that the result is always a valid date, e.g. \code{as.Date("2013-01-31") + 1
#' month} will be \code{"2013-02-28"}. If number \code{n} is negative, the
#' months will be subtracted.
#' 
#' All parameters will be recyled if necessary.
#' 
#' @param x a Date object (or something which can be coerced by
#' \code{\link{as.Date}}(x, ...) to such an object) to which a number of months
#' has to be added. 
#' @param n the number of months to be added. If n is negative the months will
#' be subtracted. 
#' @param \dots the dots are passed to \code{\link{as.Date}}, e.g. for
#' supplying \code{origin}. 
#' @return a vector of class \code{Date} with the same dimension as \code{x},
#' containing the transformed dates.
#' @author Andri Signorell <andri@@signorell.net>, based on code by Roland
#' Rapold and Antonio
#' @seealso \code{\link{as.ym}}; Date functions: \code{\link{Year}},
#' \code{\link{Month}}, etc. 
#' @references Thanks to Antonio:
#' \url{https://stackoverflow.com/questions/14169620/add-a-month-to-a-date}
#' @keywords chron
#' @examples
#' 
#' # characters will be coerced to Date
#' AddMonths("2013-01-31", 1)
#' 
#' # negative n
#' AddMonths(as.Date("2013-03-31"), -1)
#' 
#' # Arguments will be recycled
#' # (with warning if the longer is not a multiple of length of shorter)
#' AddMonths(c("2013-01-31", "2013-03-31", "2013-10-31", "2013-12-31"), c(1,-1))
#' 
#' 
#' x <- as.POSIXct(c("2015-01-31", "2015-08-31"))
#' n <- c(1, 3)
#' AddMonths(x, n)
#' 
#' # mind the origin if x supplied as numeric ...
#' x <- as.numeric(as.Date(x))
#' AddMonths(x, n, origin=as.Date("1970-01-01"))
#' 



#' @export
AddMonths <- function (x, n, ...) {
  UseMethod("AddMonths")
}


#' @method AddMonths default
#' @export
AddMonths.default <- function (x, n, ...) {
  
  .addMonths <- function (x, n) {
    
    # ref: http://stackoverflow.com/questions/14169620/add-a-month-to-a-date
    # Author: Antonio
    
    # no ceiling
    res <- sapply(x, seq, by = paste(n, "months"), length = 2L)[2L,]
    # sapply kills the Date class, so recreate down the road
    
    # ceiling
    Day(x) <- 1L
    res_c <- sapply(x, seq, by = paste(n + 1L, "months"), length = 2L)[2L,] - 1L
    
    # use ceiling in case of overlapping
    res <- pmin(res, res_c)
    
    return(res)
    
  }
  
  x <- as.Date(x, ...)
  
  res <- mapply(.addMonths, x, n)
  # mapply (as sapply above) kills the Date class, so recreate here
  # and return res in the same class as x
  class(res) <- "Date"
  
  return(res)
  
}


