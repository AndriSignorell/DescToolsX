
#' Find Position of First Occurrence Of a String 
#' 
#' Returns the numeric position of the first occurrence of a substring within a
#' string. If the search string is not found, the result will be \code{NA}.
#' 
#' This is just a wrapper for the function \code{\link{regexpr}}. 
#' 
#' @param x a character vector in which to search for the pattern, or an object
#' which can be coerced by as.character to a character vector. 
#' @param pattern character string (search string) containing the pattern to be
#' matched in the given character vector. This can be a character string or a
#' regular expression.
#' @param pos integer, defining the start position for the search within x. The
#' result will then be relative to the begin of the truncated string. Will be
#' recycled. 
#' @param \dots the dots are passed to the function \code{\link{regexpr}}. 
#' @return a vector of the first position of pattern in x 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{strChop}}, \code{\link{regexpr}} 
#' @keywords character utilities
#' @examples
#' 
#' strPos(x = levels(d.pizza$driver), pattern = "t")
#' 
#' 


#' @export
strPos <- function(x, pattern, pos=1, ... ){
  
  # example:
  #    strPos(x=levels(d.pizza$driver), "t", pos=4)
  
  pos <- rep(pos, length.out=length(x))
  x <- substr(x, start=pos, stop=nchar(x))
  
  i <- as.vector(regexpr(pattern = pattern, text = x, ...))
  i[i<0] <- NA
  return(i)
}

