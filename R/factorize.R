
#' Prime Factorization of Integers
#' 
#' Compute the prime factorization(s) of integer(s) \code{n}. 
#' Prime factorization of integer(s) works via \code{\link{primes}}, 
#' currently in a cheap way, sub-optimal for large composite \code{n}. 
#' 
#' @param n vector of integers to factorize.
#' 
#' @name factorize
#' @aliases factorize 
#' @return A named \code{\link{list}} of the same length as \code{n}, each
#' element a 2-column matrix with column \code{"p"} the prime factors and
#' column~\code{"m"} their respective exponents (or multiplities), i.e., for a
#' prime number \code{n}, the resulting matrix is \code{cbind(p = n, m = 1)}.
#' 
#' @author Andri Signorell <andri@signorell.net>
#' 
#' For factorization of moderately or really large numbers, see the \pkg{gmp}
#' package, and its \code{\link[gmp]{factorize}()} (which is ~20x faster!).
#' 
#' @family topic.numberTheory
#' @concept number theory
#' 
#' @examples
#' 
#' factorize(47)
#' factorize(seq(101, 120, by=2))


#' @rdname factorize
#' @export
factorize <- function (n) {
  setNamesX(lapply(n, factor_u64), n)
}
 
