
#' Split a String into a Number of Sections of Defined Length 
#' 
#' Splitting a string into a number of sections of defined length is needed,
#' when we want to split a table given as a number of lines without separator
#' into columns. The cutting points can either be defined by the lengths of the
#' sections or directly by position. 
#' 
#' If length is going over the end of the string the last part will be
#' returned, so if the rest of the string is needed, it's possible to simply
#' enter a big number as last partlength.
#' 
#' \code{len} and \code{pos} can't be defined simultaneously, only
#' alternatively.
#' 
#' Typical usages are \preformatted{ strChop(x, len) strChop(x, pos) } 
#' 
#' @param x the string to be cut in pieces. 
#' @param len a vector with the lengths of the pieces.
#' @param pos a vector of cutting positions. Will be ignored when \code{len}
#' has been defined.
#' @return a vector with the parts of the string.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{strLeft}},
#' \code{\link{substr}}
#' @keywords character
#' @examples
#' 
#' x <- paste(letters, collapse="")
#' strChop(x=x, len = c(3,5,2))
#' 
#' # and with the rest integrated
#' strChop(x=x, len = c(3, 5, 2, nchar(x)))
#' 
#' # cutpoints at 5th and 10th position
#' strChop(x=x, pos=c(5, 10))
#' 

#' @export
strChop <- function(x, len, pos) {
  
  .chop <- function(x, len, pos) {
    # Splits a string into a number of pieces of fixed length
    # example: strChop(x=paste(letters, collapse=""), len = c(3,5,0))
    if(!missing(len)){
      if(!missing(pos))
        stop("too many arguments")
    } else {
      len <- c(pos[1], diff(pos), nchar(x))
    }
    
    xsplit <- character(0)
    for(i in 1:length(len)){
      xsplit <- append(xsplit, substr(x, 1, len[i]))
      x <- substr(x, len[i]+1, nchar(x))
    }
    return(xsplit)
  }
  
  res <- lapply(x, .chop, len=len, pos=pos)
  
  if(length(x)==1)
    res <- res[[1]]
  
  return(res)
  
}

