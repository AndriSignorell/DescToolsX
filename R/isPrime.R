
#' Test Whether Numbers Are Prime
#'
#' Determines whether integer values are prime numbers.
#'
#' This function is vectorized and returns a logical vector of the
#' same length as the input.
#'
#' @param n A numeric vector. Values must be finite integers in the
#'   unsigned 64-bit integer range.
#'
#' @details
#' Internally, this function uses a fast deterministic primality test
#' for 64-bit integers.
#'
#' Non-integer or non-finite values will result in \code{FALSE}.
#'
#' @return A logical vector indicating whether each element of \code{n}
#'   is a prime number.
#'   
#' @author Andri Signorell <andri@@signorell.net> 
#'
#' @examples
#' isPrime(2)
#' isPrime(1:10)
#' isPrime(c(17, 18, 19))
#'
#' 
#' @family topic.numberTheory
#' @concept number theory
#' 
#'
#' @export
isPrime <- function(n) {
  sapply(n, is_prime_u64)
}
