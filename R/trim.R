

#' Trim a Vector 
#' 
#' Clean data by means of trimming, i.e., by omitting outlying observations. 
#' 
#' A symmetrically trimmed vector \code{x} with a fraction of trim observations
#' (resp. the given number) deleted from each end will be returned. If
#' \code{trim} is set to a value >0.5 or to an integer value > n/2 then the
#' result will be \code{NA}.
#' 
#' @param x a numeric vector to be trimmed. 
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each
#' end of x. Values of trim outside that range (and < 1) are taken as the
#' nearest endpoint.  If \code{trim} is set to a value >1 it's interpreted as
#' the number of elements to be cut off at each tail of \code{x}. 
#' @param na.rm a logical value indicating whether \code{NA} values should be
#' stripped before the computation proceeds. 
#' @return The trimmed vector \code{x}. The indices of the trimmed values will
#' be attached as attribute named \code{"trim"}. 
#' 
#' @note This function is basically an excerpt from the base function
#' \code{\link{mean}}, which allows the vector \code{x} to be trimmed before
#' calculating the mean. But what if a trimmed standard deviation is needed?
#' @author R-Core (function mean), Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{Winsorize}} 
#' @keywords univar robust
#' @examples
#' 
#' ## generate data
#' set.seed(1234)     # for reproducibility
#' x <- rnorm(10)     # standard normal
#' x[1] <- x[1] * 10  # introduce outlier
#' 
#' ## Trim data
#' x
#' trim(x, trim=0.1)
#' 
#' ## Trim fixed number, say cut the 3 extreme elements from each end
#' trim(x, trim=3)
#' 
#' ## check function
#' s <- sample(10:20)
#' s.tr <- trim(s, trim = 2)
#' setequal(c(s[attr(s.tr, "trim")], s.tr), s)
#'


#' @export 
trim <- function(x, trim = 0.1, na.rm = FALSE){
  
  if (na.rm) x <- x[!is.na(x)]
  
  if (!is.numeric(trim) || length(trim) != 1L)
    stop("'trim' must be numeric of length one")
  
  n <- length(x)
  
  if (trim > 0 && n) {
    if (is.complex(x))
      stop("trim is not defined for complex data")
    if (anyNA(x))
      return(NA_real_)
    if (trim >= 0.5 && trim < 1)
      return(NA_real_)
    if(trim < 1)
      lo <- floor(n * trim) + 1
    else{
      lo <- trim + 1
      if (trim >= (n/2))
        return(NA_real_)
    }
    hi <- n + 1 - lo
    
    # x <- sort.int(x, partial = unique(c(lo, hi)))[lo:hi]
    res <- sort.int(x, index.return = TRUE)
    trimi <- res[["ix"]][c(1:(lo-1), (hi+1):length(x))]
    
    # x <- res[["x"]][order(res[["ix"]])[lo:hi]]
    x <- res[["x"]][lo:hi][order(res[["ix"]][lo:hi])]
    attr(x, "trim") <- trimi
    
  }
  return(x)
}

