

#' A Class for Dealing with the Yearmonth Format
#' 
#' The representation of year and month information in YYYYYMM format as an
#' integer is often handy and a useful and efficient data structure. Adding a
#' number of months to such a date is not quite catchy, however, since the date
#' structure is to be retained. For example, 201201 - 2 \verb{[months]} is expected to
#' result in 201111 instead of 201199. \code{AddMonthsYM()} does this job.
#' 
#' All parameters will be recyled if necessary. The therefore used function
#' \code{\link{mapply}} will display a warning, if the longer argument is not a
#' multiple of the length of the shorter one.
#' 
#' @name as_ym
#' @aliases as.ym as.Date.ym AddMonths.ym
#' @param x a vector of integers, representing the dates in the format YYYYMM,
#' to which a number of months has to be added. YYYY must lie in the range of
#' 1000-3000, MM in 1-12.
#' @param d the day to be used for converting a yearmonth to a date. Default is
#' 1.  
#' @param n the number of months to be added. If n is negative the months will
#' be subtracted. 
#' @param \dots further arguments (not used here).
#' @return a vector of class \code{integer} with the same dimension as x,
#' containing the transformed dates.
#' @author Andri Signorell <andri@@signorell.net>, originally based on code by
#' Roland Rapold
#' @seealso \code{\link{AddMonths}}; Date functions, like \code{\link{Year}},
#' \code{\link{Month}}, etc. 
#' @keywords chron
#' @examples
#' 
#' 
#' Month(as.ym(202408))
#' Year(as.ym(202408))
#' 
#' Year(as.Date("2024-12-05"))
#' Year(as.ym(202412))
#' 
#' Month(as.Date("2024-12-05"), fmt = "mm")
#' Month(as.ym(202412), fmt="mm")
#' 
#' AddMonths(201511, 5)
#' 
#' AddMonths(c(201511, 201302), c(5, 15))
#' AddMonths(c(201511, 201302), c(5, -4))
#' 



# define a new class ym ("yearmonth")
#' @rdname as_ym
#' @export
as.ym <- function(x){
  
  # expects a YYYYMM format
  res <- structure(as.integer(x), class = c("ym", "num"))
  res[!((y <- round(x/100)) %[]% c(1000, 3000) & 
          (x - y * 100) %[]% c(1, 12))]   <- NA_integer_
  return(res)
}


#' @rdname as_ym
#' @method as.Date ym
as.Date.ym <- function(x, d=1, ...){
  as.Date(gsub("([0-9]{4})([0-9]{2})([0-9]{2})", "\\1-\\2-\\3", 
               x*100 + d))
}


#' @rdname as_ym
#' @method print ym
#' @export
print.ym <- function(x, ...) {
  # do not print the class attributes
  print(unclass(x), ...)
}


#' @rdname as_ym
#' @method AddMonths ym
#' @export
AddMonths.ym <- function (x, n, ...) {
  
  .addMonths <- function (x, n) {
    
    if (x %[]% c(100001L, 999912L)) {
      
      # Author: Roland Rapold
      # YYYYMM
      y <- x %/% 100L
      m <- x - y * 100L
      res <- (y - 10L + ((m + n + 120L - 1L) %/% 12L)) * 100L +
        ((m + n + 120L - 1L) %% 12L) + 1L
      
    } else if (x %[]% c(10000101L, 99991231L)) {
      
      # YYYYMMDD
      res <- AddMonths(x = as.Date(as.character(x), "%Y%m%d"), n = n)
      res <- Year(res)*10000L + DescToolsX::Month(res)*100L + Day(res)
    }
    
    return(res)
    
  }
  
  res <- mapply(.addMonths, x, n)
  
  return(res)
  
}

