
#' Greatest Common Divisor and Least Common Multiple
#' 
#' Calculates the greatest common divisor (GCD) and least common multiple (LCM)
#' of all the values present in its arguments.
#' 
#' The computation is based on the Euclidean algorithm without using the
#' extended version.The greatest common divisor for all numbers in the integer
#' vector \code{x} will be computed (the multiple GCD).
#' 
#' @name gcd_lcm
#' @aliases GCD LCM
#' @param ... integer or logical vectors.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return A numeric (integer) value.
#' @note The following relation is always true:
#' 
#' \code{n * m = GCD(n, m) * LCM(n, m)}
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{factorize}}, \code{\link{primes}},
#' \code{\link{isPrime}}
#' @references Eddelbuettel, D. (2013). Seamless R and C++ Integration with
#' Rcpp. New York, NY: Springer.
#' @examples
#' 
#' GCD(12, 10)
#' GCD(144, 233)    # Fibonacci numbers are relatively prime to each other
#' 
#' LCM(12, 10)
#' LCM(144, 233)    # = 144 * 233
#' 
#' # all elements will be flattened by unlist
#' GCD(2, 3, c(5, 7) * 11)
#' GCD(c(2*3, 3*5, 5*7))
#' LCM(c(2, 3, 5, 7) * 11)
#' LCM(2*3, 3*5, 5*7)
#' 


#' @rdname gcd_lcm
#' @export
GCD <- function(..., na.rm = FALSE) {
  
  x <- unlist(list(...), recursive=TRUE)
  
  if(na.rm) x <- x[!is.na(x)]
  if(anyNA(x)) return(NA)
  
  
  stopifnot(is.numeric(x))
  if (any(floor(x) != ceiling(x)) || length(x) < 2L)
    stop("Argument 'x' must be an integer vector of length >= 2.")
  
  x <- x[x != 0]
  n <- length(x)
  if (n == 0L) {
    g <- 0
  } else if (n == 1L) {
    g <- x
  } else if (n == 2L) {
    g <- compute_GCD( x[1L], x[2L])
  } else {
    g <- compute_GCD( x[1L], x[2L])
    for (i in 3L:n) {
      g <- compute_GCD( g, x[i])
      if (g == 1) break
    }
  }
  return(g)
}


#' @rdname gcd_lcm
#' @export
LCM <- function(..., na.rm = FALSE) {
  
  x <- unlist(list(...), recursive=TRUE)
  
  if(na.rm) x <- x[!is.na(x)]
  if(anyNA(x)) return(NA)
  
  
  stopifnot(is.numeric(x))
  if (any(floor(x) != ceiling(x)) || length(x) < 2L)
    stop("Argument 'x' must be an integer vector of length >= 2.")
  
  x <- x[x != 0]
  n <- length(x)
  if (n == 0L) {
    l <- 0
  } else if (n == 1L) {
    l <- x
  } else if (n == 2L) {
    l <- compute_LCM( x[1], x[2])
  } else {
    l <- compute_LCM( x[1], x[2])
    for (i in 3L:n) {
      l <- compute_LCM( l, x[i])
    }
  }
  return(l)
}



