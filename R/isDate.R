
#' Check If an Object Is of Type Date
#' 
#' Check if the given x is of any known Date type. 
#' 
#' This checks for many known Date and Time classes: "POSIXt", "POSIXct",
#' "dates", "times", "chron", "Date". 
#' 
#' @param x a vector or values to be checked. 
#' @param what can be any value out of "\code{either}" (default), "\code{both}"
#' or "\code{timeVaries}". 
#' @return logical vector of the same dimension as x.
#' @author Frank E Harrell 
#' @seealso \code{\link{Year}}, \code{\link{Month}}, etc. 
#' @keywords chron logic
#' @examples
#' 
#' isDate(as.Date("2013-04-10"))
#' 
#' isDate(31002)
#' 

#' @export
isDate <- function(x, what=c('either','both','timeVaries')) {
  
  what <- match.arg(what)
  cl <- class(x) # was oldClass 22jun03
  if(!length(cl)) return(FALSE)
  
  dc <- c('POSIXt','POSIXct','dates','times','chron','Date')
  dtc <- c('POSIXt','POSIXct','chron')
  switch(what,
         either = any(cl %in% dc),
         both = any(cl %in% dtc),
         timeVaries = {
           # original: if('chron' %in% cl || !.R.) { ### chron or S+ timeDate
           if('chron' %in% cl) { # chron ok, but who cares about S+?
             y <- as.numeric(x)
             length(unique(round(y - floor(y), 13L))) > 1
           } else {
             length(unique(format(x, '%H%M%S'))) > 1
           }
         }
  )
  
}

