
#' Calculate Divisors 
#' 
#' Calculate divisors of positive natural numbers. 
#' 
#' Divisibility is a mathematical relationship between two integers. An integer
#' is divisible by another integer if there is no remainder in the division.
#' The number 11 has only two divisors: 1 and the number 11 itself, whereas the
#' number 12 has many divisors: 1, 2, 3, 4, 6 and 12.  In elementary number
#' theory, the concept of divisibility is limited to natural numbers.  The
#' number of its divisors can be determined with the function
#' \code{\link{length}()}.
#' 
#' @param x integer number for which the divisors are to be returned 
#' 
#' @return an integer vector containg the divisors 
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{primes}}, \code{\link{isPrime}}, \code{\link{GCD}},
#' \code{\link{lcm}} 
#' @keywords arith math
#' @examples
#' 
#' divisors(c(145, 786))

#' @export 
divisors <- function(x) {
  sapply(x, divs)
}

