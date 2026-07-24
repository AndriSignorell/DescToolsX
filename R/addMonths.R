

#' Add Months to a Date
#' 
#' Naively adding months to a date can produce invalid dates, for example
#' when adding one month to 2012-01-30. \code{addMonths()} always returns a
#' valid date: adding one month to \code{as.Date("2013-01-31")} returns
#' \code{"2013-02-28"}. Negative values of \code{n} subtract months.
#' 
#' All parameters are recycled if necessary.
#' 
#' @param x a Date object, or an object coercible to one by
#' \code{\link{as.Date}}(x, ...), to which months are added
#' @param n number of months to add. Negative values subtract months.
#' @param \dots further arguments passed to \code{\link{as.Date}}, for example
#' to supply \code{origin}
#' @return a vector of class \code{Date} with the same dimension as \code{x},
#' containing the transformed dates
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
#' # Arguments will be recycled
#' # (with warning if the longer is not a multiple of length of shorter)
#' addMonths(c("2013-01-31", "2013-03-31", "2013-10-31", "2013-12-31"), c(1,-1))
#' 
#' 
#' x <- as.POSIXct(c("2015-01-31", "2015-08-31"))
#' n <- c(1, 3)
#' addMonths(x, n)
#' 
#' # mind the origin if x supplied as numeric ...
#' x <- as.numeric(as.Date(x))
#' addMonths(x, n, origin=as.Date("1970-01-01"))
#' 




#' @family date.time  
#' @concept date-time
#'
#'
#' @export
addMonths <- function (x, n, ...) {
  UseMethod("addMonths")
}


#' @method addMonths default
#' @export
addMonths.default <- function (x, n, ...) {
  
  .addMonths <- function (x, n) {
    
    # ref: http://stackoverflow.com/questions/14169620/add-a-month-to-a-date
    # Author: Antonio
    
    # no ceiling
    res <- sapply(x, seq, by = paste(n, "months"), length = 2L)[2L,]
    # sapply kills the Date class, so recreate down the road
    
    # ceiling
    day(x) <- 1L
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

