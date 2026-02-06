#' Check a Vector For Being Numeric, Zero Or a Whole Number 
#' 
#' Test if x contains only integer numbers, or if is numeric or if it is zero.
#' 
#' isWhole is the suggested solution for checking for an integer value, as
#' \code{\link{is.integer}} tests for \code{class(x) == "integer"} and does NOT
#' test whether x (which might be of class "numeric") contains only integer
#' numbers. (Why not simply implement it in \pkg{base}?)
#' 
#' isZero tests float numeric values for being zero.
#' 
#' isNumeric combines a test for numeric and integers. 
#' @name isFunctions
#' @aliases isWhole isNumeric isZero
#' @param x a (non-empty) numeric vector of data values. 
#' @param all logical, specifying if the whole vector should be checked. If set
#' to \code{TRUE} the function will return the result of
#' \code{all(isWhole(x))}.
#' @param tol tolerance to be used 
#' @param length.arg integer, the length of the vector to be checked for.
#' @param integer.valued logical, should x be checked as integer?
#' @param positive logical, is x supposed to be positive?
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to \code{FALSE}. 
#' @return logical vector of the same dimension as x.
#' @author R-Core, Andri Signorell <andri@@signorell.net>, Thomas W. Yee 
#' @seealso \code{\link{is.integer}} 
#' @keywords logic
#' @examples
#' 
#' (x <- seq(1,5, by=0.5))
#' isWhole( x ) #-->  \code{TRUE} \code{FALSE} \code{TRUE} ...
#' 
#' 
#' # ... These are people who live in ignorance of the Floating Point Gods.
#' # These pagans expect ... (Burns, 2011)" the following to be TRUE:
#' (.1 - .3 / 3) == 0
#' 
#' # they might be helped by
#' isZero(.1 - .3 / 3)
#' 


#' @rdname isFunctions
#' @export
isWhole <- function (x, all=FALSE, tol = sqrt(.Machine$double.eps), na.rm=FALSE) {
  
  if (na.rm)
    x <- x[!is.na(x)]
  
  if(all){
    
    if (is.integer(x)) {
      TRUE
      
    } else if (is.numeric(x)) {
      isTRUE(all.equal(x, round(x), tol))
      
    } else if (is.complex(x)) {
      isTRUE(all.equal(Re(x), round(Re(x)), tol)) && isTRUE(all.equal(Im(x), round(Im(x)), tol))
      
    } else FALSE
    
    
  } else {
    if (is.integer(x)) {
      rep(TRUE, length(x))
      
    } else if (is.numeric(x)) {
      abs(x - round(x)) < tol
      
    } else if (is.complex(x)) {
      abs(Re(x) - round(Re(x))) < tol && abs(Im(x) - round(Im(x))) < tol
      
    } else rep(FALSE, length(x))
    
  }
  
}



#' @rdname isFunctions
#' @export
isZero <-function(x, tol = sqrt(.Machine$double.eps), na.rm=FALSE) {
  # Define check if a numeric is 0
  
  if (na.rm)
    x <- x[!is.na(x)]
  
  if(is.numeric(x))
    abs(x) < tol
  else
    FALSE
  
}
 
#' @rdname isFunctions
#' @export
isNumeric <- function (x, length.arg = Inf, integer.valued = FALSE, positive = FALSE, na.rm = FALSE){

  if (na.rm)
    x <- x[!is.na(x)]

  if (all(is.numeric(x)) && all(is.finite(x)) && (if (is.finite(length.arg)) length(x) ==
                                                  length.arg else TRUE) && (if (integer.valued) all(x == round(x)) else TRUE) &&
      (if (positive) all(x > 0) else TRUE)) TRUE else FALSE
}



# IsOdd <- function(x) x %% 2L == 1L
# 
# 
# IsDichotomous <- function(x, strict=FALSE, na.rm=FALSE) {
#   if(na.rm)
#     x <- x[!is.na(x)]
#   
#   if(strict)
#     length(unique(x)) == 2L
#   else
#     length(unique(x)) <= 2L
# }
# 
# 
# IsPrime <- function(x) {
#   if (is.null(x) || length(x) == 0L)
#     stop("Argument 'x' must be a nonempty vector or matrix.")
#   if (!is.numeric(x) || any(x < 0L) || any(x != round(x)))
#     stop("All entries of 'x' must be nonnegative integers.")
#   
#   n <- length(x)
#   X <- x[1L:n]
#   L <- logical(n)
#   p <- DescTools::Primes(ceiling(sqrt(max(x))))
#   for (i in 1L:n) {
#     L[i] <- all(X[i] %% p[p < X[i]] != 0L)
#   }
#   L[X == 1 | X == 0L] <- FALSE
#   dim(L) <- dim(x)
#   return(L)
# }
# 
