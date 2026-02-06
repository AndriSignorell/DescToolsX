
#' Truncate Strings and Add Ellipses If a String is Truncated.
#' 
#' Truncates one or more strings to a specified length, adding an ellipsis
#' (...)  to those strings that have been truncated. The truncation can also be
#' performed using word boundaries.  Use \code{\link{strAlign}()} to justify
#' the strings if needed.
#' 
#' 
#' @param x a vector of strings.
#' @param maxlen the maximum length of the returned strings (NOT counting the
#' appended ellipsis). \code{maxlen} is recycled.
#' @param ellipsis the string to be appended, if the string is longer than the
#' given maximal length. The default is \code{"..."}.
#' @param wbound logical. Determines if the maximal length should be reduced to
#' the next smaller word boundary and so words are not chopped. Default is
#' \code{FALSE}.
#' @return The string(s) passed as \samp{x} now with a maximum length of
#' \samp{maxlen} + 3 (for the ellipsis).
#' @author Andri Signorell, \cr once following an idea of Jim Lemon in
#' \code{\link[prettyR]{truncString}()}
#' @seealso String functions: \code{\link{nchar}}, \code{\link{match}},
#' \code{\link{grep}}, \code{\link{regexpr}}, \code{\link{substr}},
#' \code{\link{sub}}, \code{\link{gsub}}, \code{\link{strTrim}},
#' \code{\link{strDist}}
#' @keywords character utilities
#' @examples
#' 
#' x <- c("this is short", "and this is a longer text", 
#'        "whereas this is a much longer story, which could not be told shorter")
#' 
#' # simple truncation on 10 characters
#' strTrunc(x, maxlen=10)
#' 
#' # NAs remain NA
#' strTrunc(c(x, NA_character_), maxlen=15, wbound=TRUE)
#' 
#' # using word boundaries
#' for(i in -5:20)
#'   print(strTrunc(x, maxlen=i, wbound=TRUE))
#' 
#' # compare
#' for(i in -5:20)
#'   print(strTrunc(x, maxlen=i, wbound=FALSE))
#' 



#' @export
strTrunc <- function (x, maxlen = 20, ellipsis="...", wbound=FALSE) {
  
  # replace NAs with blanks, and store the indices
  x[!(valid <- !is.na(x))] <- ""
  
  # recycle max length
  maxlen <- rep(maxlen, length.out = length(x))
  
  # correct for word boundaries
  if (wbound) {
    for(i in seq_along(x)){
      
      # only change maxlen for overlong strings
      if(nchar(x[i]) > maxlen[i]){
        # get all word boundaries
        ll <- gregexpr("\\b\\W+\\b", x[i], perl = TRUE)[[1]]
        j <- ll <= maxlen[i]
        
        # use minimum of original maxlen and closest smaller maxlen respecting word boundaries 
        maxlen[i] <- 
          if(all(!j)) {
            # length of first word is > maxlen, so return maxlen 
            maxlen[i]     
          } else {
            max(ll[ll <= maxlen[i]])
          }
      }
    }
  }
  
  res <- paste0(substr(x, 0L, maxlen), ifelse(nchar(x) > maxlen, ellipsis, ""))
  
  # restore NAs
  res[!valid] <- NA_character_
  return(res)
  
}

