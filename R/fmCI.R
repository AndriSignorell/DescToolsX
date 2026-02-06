
#' Format Confidence Intervals
#' 
#' Format confidence intervals using a flexible template.
#' 
#' 
#' @param x the numerical values, given in the order in which they are used in
#' the template.
#' @param template character string as template for the desired format.  
#' %s are the placeholders for the numerical values. Default 
#' is \verb{\code{"\%s [\%s, \%s]"}} for \verb{<est> [<lci>, <uci>]}.
#' @param \dots the dots are passed on to the \code{\link{fm}()} function.
#' @return a formatted string
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{fm}}
#' @examples
#' 
#' x <- c(est=2.1, lci=1.5, uci=3.8)
#' 
#' # default template
#' fmCI(x)
#' # user defined template (note the double percent code)
#' fmCI(x, template="%s (95%%-CI %s-%s)", digits=1)
#' 
#' # in higher datastructures
#' mci <- tapply(warpbreaks$breaks, warpbreaks$wool, meanCI)
#' sapply(mci, fmCI, digits=1)
#' 


#' @export  
fmCI <- function(x, template=NULL, ...){
  
  x <- fm(x, ...)
  n <- length(x)
  
  if (!n %in% c(2, 3))
    stop("x must have length 2 (lci, uci) or 3 (estimate, lci, uci)")
  
  if (is.null(template)) {
    template <- switch(
      n,
      "%s [%s, %s]",   # n = 3
      "[%s, %s]"       # n = 2
    )
  }
  
  do.call(gettextf, c(list(template), as.list(x)))
  
}


