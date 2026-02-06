
#' Prime Factorization of Integers
#' 
#' Compute the prime factorization(s) of integer(s) \code{n}.
#' 
#' % ## Purpose: Prime factorization of integer(s) 'n'  
#' 
#' works via \code{\link{primes}}, currently in a cheap way, sub-optimal for
#' large composite \eqn{n}. 
#' 
#' @param n vector of integers to factorize.
#' 
#' @name number_theory
#' @aliases factorize primes isPrime
#' @return A named \code{\link{list}} of the same length as \code{n}, each
#' element a 2-column matrix with column \code{"p"} the prime factors and
#' column~\code{"m"} their respective exponents (or multiplities), i.e., for a
#' prime number \code{n}, the resulting matrix is \code{cbind(p = n, m = 1)}.
#' 
#' @author Andri Signorell <andri@signorell.net>
#' @seealso \code{\link{GCD}}, \code{\link{LCM}}, \code{\link{primes}},
#' \code{\link{isPrime}}, \code{\link{divisors}}
#' 
#' For factorization of moderately or really large numbers, see the \pkg{gmp}
#' package, and its \code{\link[gmp]{factorize}()} (which is ~20x faster!).
#' @keywords math arith
#' @examples
#' 
#' factorize(47)
#' factorize(seq(101, 120, by=2))


#' @rdname number_theory
#' @export
factorize <- function (n) {
  setNamesX(lapply(n, factor_u64), n)
}
 

#' @rdname number_theory
#' @export
primes <- function (n) { 
  setNamesX(lapply(n, primes_upto), n)
}


#' @rdname number_theory
#' @export
isPrime <- function(n) {
  sapply(n, is_prime_u64)
}

