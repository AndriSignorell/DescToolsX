
#' Find the Closest Value
#' 
#' Find the closest value(s) of a number in a vector x. Multiple values will be
#' reported, if the differences are the same or if there are duplicates of the
#' same value.
#' 
#' 
#' @param x the vector to be searched in
#' @param a the reference value
#' @param which a logical value defining if the index position or the value
#' should be returned. By default will the value be returned.
#' @param na.rm a logical value indicating whether \code{NA} values should be
#' stripped before the computation proceeds.
#' @return the value or index in x which is closest to a
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{which}}
#' @keywords logic
#' @examples
#' 
#' 
#' # basic
#' set.seed(8)
#' x <- runif(10) * 10
#' closest(x, 3.1)
#' sort(x)
#' 
#' y <- sample(10, size=10, replace=TRUE)
#' # multiple observations of the same closest value 
#' closest(y, a=6)
#' # get the relevant positions
#' closest(y, a=6, which=TRUE)
#' 
#' # two different values having the same distance
#' closest(c(2, 3, 4, 5), a=3.5)
#' 
#' # vectorize "a"
#' closest(c(2, 3, 4, 5), a=c(3.1, 3.9))
#' 
#' # vectorize "which"
#' closest(c(2, 3, 4, 5), a=3.1, which=c(FALSE, TRUE))
#' 
#' # vectorize both
#' closest(c(2, 3, 4, 5), a=c(3.1, 3.9), which=c(FALSE, TRUE))
#' 

#' @export
closest <- function(x, a, which = FALSE, na.rm = FALSE){
  
  # example: closest(a=67.5, x=d.pizza$temperature, na.rm=TRUE)
  
  FUN <- function(x, a, which = FALSE, na.rm = FALSE){
    
    if(na.rm) x <- x[!is.na(x)]
    
    mdist <- min(abs(x-a))
    if(is.na(mdist))
      res <- NA
    
    else {
      idx <- isZero(abs(x-a) - mdist)    # beware of floating-point-gods
      if(which == TRUE )
        res <- which(idx)
      else
        res <- x[idx]
    }
    
    # Frank's Hmisc solution is faster
    # but does not handle ties satisfactorily
    
    #   res <- .Fortran("wclosest", as.double(a), as.double(x), length(a),
    #            length(x), j = integer(length(a)), PACKAGE = "DescTools")$j
    #   if(!which) res <- x[res]
    
    return(res)
    
  }
  
  # vectorize arguments a and which
  res <- mapply(FUN=FUN, a=a, which=which, 
                MoreArgs = list(x=x, na.rm=na.rm), SIMPLIFY=FALSE)
  
  # simplify: if res is a list with 1 element only, reduce to vector
  if(length(res)==1)
    res <- res[[1]] 
  
  return(res)
  
}
