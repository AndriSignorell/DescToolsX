
#' Remove Leading/Trailing Whitespace From A String 
#' 
#' The function removes whitespace characters as spaces, tabs and newlines from
#' the beginning and end of the supplied string. Whitespace characters
#' occurring in the middle of the string are retained.\cr Trimming with method
#' \code{"left"} deletes only leading whitespaces, \code{"right"} only
#' trailing. Designed for users who were socialized by SQL. 
#' 
#' The functions are defined depending on method as\cr \code{both: gsub(
#' pattern=gettextf("^[\%s]+|[\%s]+$", pattern, pattern), replacement="",
#' x=x)}\cr \code{left: gsub( pattern=gettextf("^[\%s]+",pattern),
#' replacement="", x=x)}\cr \code{right: gsub(
#' pattern=gettextf("[\%s]+$",pattern), replacement="", x=x)} 
#' 
#' @param x the string to be trimmed. 
#' @param pattern the pattern of the whitespaces to be deleted, defaults to
#' space, tab and newline: \code{" \t\n"}. 
#' @param method one out of \code{"both"} (default), \code{"left"},
#' \code{"right"}. Determines on which side the string should be trimmed.
#' @return the string x without whitespaces
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso String functions: \code{trimws}, \code{\link{nchar}},
#' \code{\link{match}}, \code{\link{grep}}, \code{\link{regexpr}},
#' \code{\link{substr}}, \code{\link{sub}}, \code{\link{gsub}},
#' \code{\link{strTrunc}}, \code{\link{strDist}}
#' @keywords manip character
#' @examples
#' 
#' strTrim("  Hello world! ")
#' 
#' strTrim("  Hello world! ", method="left")
#' strTrim("  Hello world! ", method="right")
#' 
#' # user defined pattern
#' strTrim(" ..Hello ... world! ", pattern=" \\.")
#' 

 
#' @export
strTrim <- function(x, pattern=" \t\n", method="both") {
  
  switch(match.arg(arg = method, choices = c("both", "left", "right")),
         both =  { gsub( pattern=gettextf("^[%s]+|[%s]+$", pattern, pattern), replacement="", x=x) },
         left =  { gsub( pattern=gettextf("^[%s]+",pattern), replacement="", x=x)  },
         right = { gsub( pattern=gettextf("[%s]+$",pattern), replacement="", x=x)  }
  )
  
}

