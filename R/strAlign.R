
#' String Alignment 
#' 
#' Align a vector of strings to the left, to the right, to the center or to the
#' first occurance of a specified character, e.g. to the decimal separator.
#' Alignment is achieved by padding the strings with empty spaces (which
#' evidently only will have an alignment effect if the text is displayed with a
#' monospaced font). 
#' 
#' Alignment to the left or right leave no room for misinterpretation. The
#' function will determine the maximum string size in the vector, resize all
#' the strings to this size by padding empty spaces either at the beginning or
#' at the end. \preformatted{cbind(strAlign(c("here", "there", "everywhere"),
#' sep = "\r")) [1,] " here" [2,] " there" [3,] "everywhere"}
#' 
#' When it comes to center strings, it's not clear where to place strings with
#' an even length in case the maximum length is odd (or vice versa). We will
#' put the shorter distance of an uneven string to the left (note the second
#' term, that has 2 spaces on the left and 3 spaces on the right).
#' \preformatted{cbind(strAlign(c("here", "there", "everywhere"), sep = "\c"))
#' [1,] " here " [2,] " there " [3,] "everywhere"}
#' 
#' Any specific length of the strings can be created by \code{\link{strPad}} if
#' required.
#' 
#' In case of a given character as separator the strings will be aligned
#' towards this separator. Frequently this might be the decimal separator. If a
#' string does not contain the separator, the affected string will be aligned
#' as if it had a separator as last character. This seems to be a good default,
#' when integer numbers are to be aligned with numerical values. Note that the
#' character length of the resulting strings can excceed the maximum length of
#' the supplied strings. \preformatted{z <- c(" 6.0", "6.00 ", " 45.12 ",
#' "784", NA) cbind(strAlign(z, sep=".")) [,1] [1,] " 6.0 " [2,] " 6.00" [3,] "
#' 45.12" [4,] "784 " [5,] NA }
#' 
#' The character strings will not be pruned of whitespaces, if the requested
#' alignment does not explicitly require it. \code{\link{strTrim}} can be used
#' for that.
#' 
#' @param x a character vector to be aligned. %% ~~Describe \code{x} here~~
#' @param sep the character on whose position the strings will be aligned. Left
#' alignment can be requested by setting \code{sep = "\\l"}, right alignment by
#' \code{"\\r"} and center alignment by \code{"\\c"}. Mind the backslashes, as
#' if they are omitted, strings would be aligned to the \bold{character} l, r
#' or c respectively. Default value is \code{"\\r"}, thus right alignment. 
#' @return a character vector containing the aligned strings 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{strTrim}}, \code{\link{strPad}}, \code{\link{fm}}
#' @keywords character
#' @examples
#' 
#' # align on (the first occuring) B
#' x <- c("ABCDMNB", "CDGHEBK", "BCI")
#' cbind(strAlign(x, sep="B"))
#' 
#' # align to decimal separator (here point)
#' z <- c("    6.0", "6.00  ", " 45.12    ", "784", NA)
#' cbind(strAlign(z, sep="."))
#' 
#' # right align, the width will be the max number of characters in x
#' cbind(strAlign(x, sep="\\r"))
#' # left align
#' cbind(strAlign(x, sep="\\l"))
#' # center
#' cbind(strAlign(x, sep="\\c"))
#' 

#' @export
strAlign <- function(x, sep = "\\r"){
  
  # replace \l by \\^, \r by \\$ and \c means centered
  # check for NA only and combined
  # return x if sep is not found in x
  
  id.na <- is.na(x)
  
  # what should be done, if x does not contain sep??
  # we could return unchanged, but this is often not adaquate
  # we align right to the separator
  if(length(grep("\\", sep, fixed=TRUE)) == 0) {
    idx <- !grepl(x=x, pattern=sep, fixed = TRUE)
    x[idx] <- paste(x[idx], sep, sep="")
  }
  
  # center alignment
  # keep this here, as we may NOT pad x for centered text!!
  # example?? don't see why anymore... check!
  if (sep == "\\c")
    return(strPad(x, width = max(nchar(x), na.rm=TRUE), pad = " ", adj = "center"))
  
  # Pad to same maximal length, for right alignment this is mandatory
  # for left alignment not, but again for any character
  x <- strPad(x, max(nchar(x), na.rm=TRUE))
  
  # left alignment
  if(sep == "\\l")
    return( sub("(^ +)(.+)", "\\2\\1", x) )
  
  # right alignment
  if(sep == "\\r")
    return( sub("(.+?)( +$)", "\\2\\1", x) )
  
  # alignment by a special character
  bef <- substr(x, 1, strPos(x, sep, fix=TRUE))  # use fix = TRUE as otherwise the decimal would be to have entered as \\.
  aft <- substr(x, strPos(x, sep, fix=TRUE) + 1, nchar(x))
  # chop white space on the right
  aft <- substr(aft, 1, max(nchar(strTrim(aft, method="right"))))
  res <- paste(replace(strPad(bef, max(nchar(bef), na.rm=TRUE),
                              " ", adj = "right"), is.na(bef), ""),
               replace(strPad(aft, max(nchar(aft), na.rm=TRUE), " ", adj = "left"), is.na(aft),
                       ""), sep = "")
  
  # restore orignal NAs
  res[id.na] <- NA
  
  # overwrite the separator
  if(length(grep("\\", sep, fixed=TRUE)) == 0)
    res[idx] <- gsub(sep, " ", res[idx], fixed = TRUE)
  
  # return unchanged values not containing sep
  return(res)
  
}
