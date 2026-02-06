
#' Convert ASCII Codes to Characters and Vice Versa
#' 
#' \code{ascToChar()} returns a character for each ASCII code (integer) supplied.\cr
#' \code{charToAsc()} returns integer codes in \code{0:255} for each (one byte)
#' character in all strings in \code{x}.
#' 
#' Only codes in \code{1:127} make up the ASCII encoding which should be
#' identical for all \R versions, whereas the \emph{\sQuote{upper}} half is
#' often determined from the ISO-8859-1 (aka \dQuote{ISO-Latin 1)} encoding,
#' but may well differ, depending on the locale setting, see also
#' \code{\link{Sys.setlocale}()}.
#' 
#' Note that \code{0} is no longer allowed since, \R does not allow \code{\0}
#' aka \code{nul} characters in a string anymore.
#' 
#' @name char_asc
#' @aliases ascToChar charToAsc
#' 
#' @param i numeric (integer) vector of values in \code{1:255}.
#' @param x vector of strings.
#' @return \code{AscToChar} returns a vector of the same length as i.
#' \code{CharToAsc} returns a list of numeric vectors of character length of
#' each string in x.
#' 
#' @author unknown guy out there, help text partly taken from M. Maechler's
#' \pkg{sfsmisc}.
#' @seealso \code{\link{charToRaw}}
#' @keywords manip
#' @examples
#' 
#' (x <- charToAsc("Silvia"))
#' 
#' # will be pasted together
#' ascToChar(x)
#' 
#' # use strsplit if the single characters are needed
#' strsplit(ascToChar(x), split=NULL)
#' 
#' # this would be an alternative, but the latter would be of class raw
#' decToHex(charToAsc("Silvia"))
#' # same as:
#' charToRaw("Silvia")
#' 


#' @rdname char_asc
#' @export
charToAsc <- function(x) {
  # Original from Henrik Bengtsson R.oo:
  # char2asc <- function (ch, ...) { match(ch, ASCII) - 1 }
  # example:  x.char <- char2asc(x="Andri")
  
  
  if(length(x) == 1)
    strtoi(charToRaw(x), 16L)
  else
    sapply(x, function(x) strtoi(charToRaw(x), 16L))
  
}


#' @rdname char_asc
#' @export
ascToChar <- function(i) {
  # old version:
  # example: AscToChar(x.char)
  #  ASCII <- intToUtf8(1:256, multiple=TRUE)
  
  # new and far more elegant
  # ref: http://datadebrief.blogspot.ch/search/label/R
  rawToChar(as.raw(i))
  
}

