
#' Converts Numbers From Binmode, Octmode or Hexmode to Decimal and Vice Versa
#' 
#' These functions convert numbers from one base to another. There are several
#' solutions for this problem out there, but the naming is quite heterogeneous
#' and so consistent function names might be helpful. 
#' 
#' \code{binToDec()} converts numbers from binary mode into decimal values. 
#' \code{decToBin} does it the other way round.\cr Oct means octal 
#' system and hex hexadecimal.
#' 
#' @name conv_numeric
#' @aliases binToDec decToBin octToDec decToOct hexToDec decToHex
#' 
#' @param x a vector of numbers, resp. alphanumerical representation of numbers
#' (hex), to be converted. 
#' @return A numeric or character vector of the same length as x containing the
#' converted values. Binary, octal and decimal values are numeric, hex-values
#' are returned as class \code{hexmode}. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{strtoi}} 
#' @keywords arith
#' @examples
#' 
#' decToBin(c(17, 25))
#' binToDec(c(101, 11101))
#' 
#' decToOct(c(17, 25))
#' octToDec(c(11, 77))
#' 
#' decToHex(c(17, 25))
#' hexToDec(c("FF", "AA", "ABC"))
#' 


#' @rdname conv_numeric
#' @export
hexToDec <- function(x) 
  # strip potential # from a string x
  strtoi(gsub("^#", "", x), 16L)
# example: strtoi(c("9A", "3B"), 16L)

#' @rdname conv_numeric
#' @export
decToHex <- function(x) as.hexmode(as.numeric(x))

#' @rdname conv_numeric
#' @export
octToDec <- function(x) strtoi(x, 8L)
# example: strtoi(c("12", "24"), 8L)

#' @rdname conv_numeric
#' @export
decToOct <- function(x) as.numeric(as.character(as.octmode(as.numeric(x))))
# Alternative: as.numeric(sprintf(242, fmt="%o"))


#' @rdname conv_numeric
#' @export
binToDec <- function(x) {
  # Alternative:  bin2dec <- function(x) { sum(2^.subset((length(x)-1):0, x)) }
  # example: bin2dec(x=as.numeric(unlist(strsplit("1001", split=NULL)))==1)
  strtoi(x, 2L)
}

#' @rdname conv_numeric
#' @export
decToBin <- function (x) {
  z <- conv_DecToBin(x)
  z[x > 536870911] <- NA
  return(sub("^0+", "", z))
}


#' @rdname conv_numeric
#' @export
romanToInt <- function (x) {
  # opposite to as.roman
  roman <- trimws(toupper(as.character(x)))
  tryIt <- function(x) {
    retval <- try(roman2int_cpp(x)(x), silent = TRUE)
    if (is.numeric(retval))
      retval
    else NA
  }
  retval <- sapply(roman, tryIt)
  retval
  
}

