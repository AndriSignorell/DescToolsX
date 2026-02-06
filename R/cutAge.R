
#' Create a Factor Variable by Cutting an Age Variable
#' 
#' Dividing the range of an age variable \code{x} into intervals is a frequent
#' task in data analysis. The commonly used function \code{\link{cut}} has
#' unfavourable default values for this. \code{cutAge()} is a convenient
#' wrapper for cutting age variables in groups of e.g. 10 years with more
#' suitable defaults.
#' 
#' 
#' @param x continuous variable.
#' @param breaks either a numeric vector of two or more unique cut points or a
#' single number (greater than or equal to 2) giving the number of intervals
#' into which x is to be cut. Default is 10-year intervals from 0 to 90.
#' @param right logical, indicating if the intervals should be closed on the
#' right (and open on the left) or vice versa.  Default is \code{FALSE} -
#' unlike in \code{\link{cut}}!
#' @param ordered_result logical: should the result be an ordered factor?
#' Default is \code{TRUE} - unlike in \code{\link{cut}}!
#' @param full logical, setting to \code{FALSE} will remove empty levels at the
#' edges of the distribution
#' @param labels labels for the levels. When set to \code{TRUE} the age ranges
#' will be 00-09, 10-19, 20-29, etc.
#' @param \dots the dots are passed on to the underlying function
#' \code{\link{cut}()}. Use these for e.g. change the labels.
#' @return A factor is returned, unless labels = FALSE which results in an
#' integer vector of level codes.
#' 
#' Values which fall outside the range of breaks are coded as \code{NA}, as are
#' \code{NaN} and \code{NA} values.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{cut}}, \code{\link{seq}}
#' @examples
#' 
#' Desc(cutAge(sample(100, 100)))


 
#' @export
cutAge <- function(x, breaks=c(seq(from=0, to=90, by=10), Inf), 
                   right=FALSE, ordered_result=TRUE, full=TRUE, 
                   labels=NULL, ...) {
  
  
  if(identical(labels, TRUE)){
    labels <- paste(fm(head(breaks, -1), ldigits=2, digits=0), 
                    c(head(breaks[-1], -1)-1, ".."), sep="-")
  }
  
  res <- cut(x, breaks = breaks, 
             right=right, ordered_result = ordered_result, 
             labels = labels, ...)
  
  if(!full)
    res <- factor(res, 
                  levels=levels(res)[do.call(seq, 
                                             as.list(range(which(Freq(res)$freq != 0))))])			
  
  return(res)  
  
}

