
#' Convert h:m:s To/From Seconds
#' 
#' \code{HmsToSec} - Converts a vector of h:m:s to seconds.
#' 
#' \code{SecToHms} - Converts a vector of seconds to h:m:s.
#' 
#' 
#' @name time_conversions
#' @aliases HmsToSec SecToHms
#' @param x A vector of times in h:m:s (for \code{HmsToSec}) or seconds (for
#' \code{SecToHms}).
#' @param digits the number of digits to use for potential fractions of
#' seconds.
#' @return \code{HmsToSec} - Returns a vector of times in seconds.
#' 
#' \code{SecToHms} - Returns a vector of times in h:m:s format.
#' @author Tyler Rinker <tyler.rinker@@gmail.com>
#' @seealso \code{\link[chron]{times}}
#' @keywords chron
#' @examples
#' 
#' HmsToSec(c("02:00:03", "04:03:01"))
#' HmsToSec(SecToHms(c(222, 1234, 55)))
#' SecToHms(c(256, 3456, 56565))
#' 



#' @rdname time_conversions
#' @export
HmsToMinute <- function(x){
  Hour(x)*60 + Minute(x) + Second(x)/60
}


#' @rdname time_conversions
#' @export
HmsToSec <- function(x) {
  
  hms <- as.character(x)
  z <- sapply(data.frame(do.call(rbind, strsplit(hms, ":"))),
              function(x) { as.numeric(as.character(x)) })
  z[,1] * 3600 + z[,2] * 60 + z[,3]
}


#' @rdname time_conversions
#' @export
SecToHms <- function(x, digits=NULL) {
  
  x <- as.numeric(x)
  
  h <- floor(x/3600)
  m <- floor((x-h*3600)/60)
  s <- floor(x-(m*60 + h*3600))
  b <- x-(s + m*60 + h*3600)
  
  if(is.null(digits)) digits <- ifelse(all(b < sqrt(.Machine$double.eps)),0, 2)
  if(digits==0) f <- "" else f <- gettextf(paste(".%0", digits, "d", sep=""), round(b*10^digits, 0))
  
  gettextf("%02d:%02d:%02d%s", h, m, s, f)
  
}


