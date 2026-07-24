

#' A Class for Dealing with the Yearmonth Format
#' 
#' Representing year and month information as an integer in YYYYMM format is
#' compact and efficient. Calendar arithmetic must nevertheless preserve the
#' date structure: for example, subtracting two months from 201201 should
#' return 201111 rather than 201199. \code{addMonths()} provides this
#' arithmetic for objects of class \code{"ym"}.
#' 
#' All parameters are recycled if necessary. The underlying
#' \code{\link{mapply}} call warns if the longer argument is not a multiple
#' of the length of the shorter one.
#' 
#' @name as_ym
#' @aliases as.ym as.Date.ym addMonths.ym
#' @param x a vector of integers, representing the dates in the format YYYYMM,
#' to which a number of months has to be added. YYYY must lie in the range of
#' 1000-3000, MM in 1-12.
#' @param d the day to be used for converting a yearmonth to a date. Default is
#' 1.  
#' @param n the number of months to be added. If n is negative the months will
#' be subtracted. 
#' @param \dots further arguments; currently unused
#' @return
#' \describe{
#'   \item{\code{as.ym()}}{an integer vector of class \code{"ym"}}
#'   \item{\code{as.Date.ym()}}{a vector of class \code{"Date"}}
#'   \item{\code{addMonths.ym()}}{a numeric vector containing the transformed
#'     YYYYMM or YYYYMMDD values}
#'   \item{\code{print.ym()}}{invisibly, the printed unclassed vector}
#' }
#' 
#' @note Based on code by Roland Rapold, adapted to conform to package standards. 
#' 
#' @seealso \code{\link{addMonths}}; Date functions, like \code{\link{year}},
#' \code{\link{month}}, etc. 
#' @examples
#' 
#' 
#' month(as.ym(202408))
#' year(as.ym(202408))
#' 
#' year(as.Date("2024-12-05"))
#' year(as.ym(202412))
#' 
#' month(as.Date("2024-12-05"), fmt = "mm")
#' month(as.ym(202412), fmt="mm")
#' 
#' addMonths(201511, 5)
#' 
#' addMonths(c(201511, 201302), c(5, 15))
#' addMonths(c(201511, 201302), c(5, -4))
#' 



# define a new class ym ("yearmonth")
#' @rdname as_ym




#' @family date.time  
#' @concept date-time
#'
#'
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
#' @export
as.Date.ym <- function(x, d=1, ...){
  as.Date(gsub("([0-9]{4})([0-9]{2})([0-9]{2})", "\\1-\\2-\\3", 
               as.integer(x)*100 + d))
}


#' @rdname as_ym
#' @method print ym
#' @export
print.ym <- function(x, ...) {
  # do not print the class attributes
  print(unclass(x), ...)
}


#' @rdname as_ym
#' @method addMonths ym
#' @export
addMonths.ym <- function (x, n, ...) {
  
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
      res <- addMonths(x = as.Date(as.character(x), "%Y%m%d"), n = n)
      res <- year(res)*10000L + DescToolsX::month(res)*100L + day(res)
    }
    
    return(res)
    
  }
  
  res <- mapply(.addMonths, x, n)
  
  return(res)
  
}


#' @export
`+.ym` <- function(x, y){
  addMonths(x, y)
}

#' @export
`-.ym` <- function(x, y){
  addMonths(x, -y)
}




