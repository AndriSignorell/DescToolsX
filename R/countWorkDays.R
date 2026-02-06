

#' Count Work Days Between Two Dates
#' 
#' Returns the number of work days between two dates taking into account the
#' provided holiday dates. 
#' 
#' The function is vectorised so that multiple initial and final dates can be
#' supplied. The dates are recycled, if their number are different.
#' 
#' @param from the initial dates 
#' @param to the final dates 
#' @param holiday a vector with dates to be excluded. 
#' @param nonworkdays a character vector containing the abbreviations of the
#' weekdays (as in \code{day.abb}) to be considered non work days. Default is
#' \code{c("Sat","Sun")}. 
#' @return an integer vector 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{weekdays}}, \code{Date Functions}
#' @keywords chron
#' @examples
#' 
#' from <- as.Date("2019-01-01") + rep(0, 10)
#' to <- as.Date("2020-01-15") + seq(0, 9)
#' 
#' CountWorkDays(from, to)
#' 
#' x <- seq(from[1], from[1]+11, "days")
#' x <- to
#' data.frame(
#'   date = x, 
#'   day  = fm(x, fmt="ddd"))
#' 
#' CountWorkDays(from = min(x), to = max(x), holiday = c("2019-01-06", "2019-01-07"))
#' 


#' @rdname date_functions
#' @export
CountWorkDays <- function(from, to, 
                          holiday=NULL, nonworkdays=c("Sat","Sun")) {
  
  
  .workDays <- function(from, to, 
                        holiday=NULL, nonworkdays=c("Sat","Sun")) {
    d <- as.integer(to - from)
    w <- (d %/% 7)
    
    res <- w * (7-length(nonworkdays)) + 
      sum(Weekday(seq(from + w*7,  to, 1), fmt="dd", lang="en") %nin% nonworkdays)
    
    if(!is.null(holiday)){
      # count holidays in period
      h <- holiday[holiday %[]% c(from, to)]
      res <- res - sum(Weekday(h, fmt="dd", lang="en") %nin% nonworkdays)
    }
    
    return(res)
    
  }
  
  
  ll <- recycle(from=from, to=to)  
  
  res <- integer(attr(ll, "maxdim"))
  for(i in 1:attr(ll, "maxdim"))
    res[i] <- .workDays(ll$from[i], ll$to[i], holiday=holiday, nonworkdays=nonworkdays) 
  
  return(res)
  
}

