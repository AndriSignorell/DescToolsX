
#' Fractional Part and Maximal Digits of a Numeric Value 
#' 
#' \code{Frac()} returns the fractional part of a numeric value.
#' \code{MaxDigits()} return the number of digits in \code{x}. \cr
#' \code{Ndec()} returns the number of decimals.\cr \code{Prec()} returns the
#' precision of a number \code{x}.
#' 
#' @name precision
#' @aliases frac maxDigits ndec prec
#' 
#' @param x the numeric value (or a vector of numerics), whose fractional part
#' is to be calculated. 
#' @param dpwr power of 10 for a factor z, the fractional part will be
#' multiplied with. The result will be returned rounded to integer. Defaults to
#' \code{NA} and will then be ignored. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{format.info}}, \code{\link{as.integer}},
#' \code{\link{trunc}}
#' @keywords arith
#' @examples
#' 
#' x <- rnorm(5)*100
#' x
#' frac(x)
#' 
#' # multiply by 10^4
#' frac(x, dpwr=4)
#' 
#' maxDigits(c(1.25, 1.8, 12.0, 1.00000))
#' 
#' x <- c("0.0000", "0", "159.283", "1.45e+10", "1.4599E+10" )
#' nDec(x)
#' prec(as.numeric(x))
#' 


#' @rdname precision
#' @export
nDec <- function(x) {
  
  # the decimal separator
  # .dec_sep <- function() gsub("1", "", format(1.1))
  
  # returns the number of decimal places in a number x
  
  # possible alternative:  format.info 
  #   [1] ... width (in characters) used by format(x), 
  #   [2] ... number of digits after decimal point.
  #   [3] ... exponential representation 
  
  if(!inherits(x, "character"))
    x <- as.character(x)
  
  res <- rep(0, length(x))
  
  # remove evtl. exponents
  x <- gsub(pattern="[eE].+$", replacement="", x=x)
  
  has_sep <- grep(.dec_sep(), x, fixed = TRUE)
  res[has_sep] <- nchar( sub("^.+[.]", "", x) )[has_sep]
  
  return(res)
  
}


#' @rdname precision
#' @export
prec <- function (x) {
  
  # Function to return the most precise
  # digit from a vector of real numbers
  # Keep dividing by powers of 10 (pos and neg from trunc(log(max(x)) down)
  # until the fractional portion is zero, then we have the highest precision
  # digit in terms of a integer power of 10.
  
  # Thanks to Thomas Lumley for help with machine precision
  
  # Note:  Turn this into a standalone function for "regularizing" a
  #        time-activity object with irregular time breaks.
  
  init <- trunc(log10(max(x))) + 1
  zero <- 0
  y <- 1
  while (any(y > zero)) {
    init <- init - 1
    x1 <- x*10^(-init)
    y <- x1 - trunc(x1)
    zero <- max(x1)*.Machine$double.eps
  }
  10^init
  
  # sapply(c(1.235, 125.3, 1245), prec)
  
}

# other idea:
# precision <- function(x) {
#   rng <- range(x, na.rm = TRUE)
#
#   span <- if (zero_range(rng)) rng[1] else diff(rng)
#   10 ^ floor(log10(span))
# }



#' @rdname precision
#' @export
frac <- function(x, dpwr = NA) {  # fractional part
  res <- abs(x) %% 1
  # Alternative: res <- abs(x-trunc(x))
  if (!missing(dpwr)) res <- round(10^dpwr * res)
  res
}


#' @rdname precision
#' @export
maxDigits <- function(x){
  # How to find the significant digits of a number?
  z <- na.omit(unlist(
    lapply(strsplit(as.character(x),
                    split = getOption("OutDec"), fixed = TRUE),
           "[", 2)))
  if(length(z)==0)
    res <- 0
  else
    res <- max(nchar(z))
  
  return(res)
  
  # Alternative: Sys.localeconv()["decimal_point"]
}



