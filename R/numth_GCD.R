
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
#' @aliases gcd lcm
#' @param ... integer or logical vectors.
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @return A numeric (integer) value.
#' @note The following relation is always true:
#' 
#' \code{n * m = gcd(n, m) * lcm(n, m)}
#' @author Andri Signorell <andri@@signorell.net> 
#' 
#' @family topic.numberTheory
#' @concept number theory
#' 
#' @examples
#' gcd(12, 10)
#' gcd(144, 233)    # Fibonacci numbers are relatively prime to each other
#' 
#' lcm(12, 10)
#' lcm(144, 233)    # = 144 * 233
#' 
#' # all elements will be flattened by unlist
#' gcd(2, 3, c(5, 7) * 11)
#' gcd(c(2*3, 3*5, 5*7))
#' lcm(c(2, 3, 5, 7) * 11)
#' lcm(2*3, 3*5, 5*7)
#' 


#' @rdname gcd_lcm
#' @export
gcd <- function(..., na.rm = FALSE) {
  
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
lcm <- function(..., na.rm = FALSE) {
  
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



