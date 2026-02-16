
#' Generate Prime Numbers up to Given Limits
#'
#' Computes all prime numbers less than or equal to each value in \code{n}.
#'
#' The function is vectorized over \code{n} and returns a named list,
#' where each element contains the prime numbers up to the corresponding value.
#'
#' @param n A numeric vector of positive integers.
#'
#' @details
#' For each element of \code{n}, the primes are calculated. 
#' The result is returned as a named list, with names corresponding
#' to the input values.
#'
#' @return A named list. Each element is an integer vector containing
#'   the prime numbers less than or equal to the corresponding value in \code{n}.
#'   
#' @author Andri Signorell <andri@@signorell.net> 
#'
#' @examples
#' primes(10)
#' primes(c(5, 10))
#'
#' @seealso \code{\link{isPrime}}
#'
#' @export
primes <- function (n) { 
  setNamesX(lapply(n, primes_upto), n)
}
