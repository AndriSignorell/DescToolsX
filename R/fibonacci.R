
#' Fibonacci Numbers
#' 
#' Generate Fibonacci numbers. The Fibonacci numbers can also be calculated
#' using the golden ratio \code{phi}, as demonstrated in the examples.
#' 
#' Generates the \code{n}-th Fibonacci number, whereas \code{Fibonacci(0) = 0}.
#' \cr The golden ratio is defined as \code{phi = 0.5*(1+sqrt(5))}.
#' 
#' @aliases fibonacci phi
#' 
#' @param n nonnegative integer or vector of nonnegative integers.
#' @return A single integer, or a vector of integers.
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @references \url{https://en.wikipedia.org/wiki/Fibonacci_number}\cr
#' \url{https://mathworld.wolfram.com/GoldenRatio.html}
#' 
#' @family topic.numberTheory
#' @concept number theory
#' 
#' 
#' @examples
#' 
#' fibonacci(0)                            # 1
#' fibonacci(2)                            # 2
#' fibonacci(0:3)                          # 0 1 1 2
#' fibonacci(0:25)                         # ... 75025 121393
#' 
#' # Golden ratio = Fib(25)/ Fib(24)
#' f25 <- quot(fibonacci(24:25))           # 1.618033989
#' phi <- (sqrt(5) + 1)/2
#' abs(f25 - phi)                          # 7.945178e-11
#' 
#' # Fibonacci numbers without iteration
#' fibo <- function(n) {
#'   phi <- (sqrt(5) + 1)/2
#'   fib <- (phi^(n+1) - (1-phi)^(n+1)) / (2*phi - 1)
#'   round(fib)
#' }
#'   
#' fibo(30:33)                             # 1346269 2178309 3524578 5702887
#' 
 
#' @export
fibonacci <- function(n) {
  
  # if (!is.numeric(n) || !IsWhole(n) || n < 0)
  if(any(sapply(n, function(i) !is.numeric(i) || !isWholeLike(i) || i < 0L)))
    stop("Argument 'n' must be an integer >= 0.")
  
  maxn <- max(n)
  if (maxn == 0L) return(0L)
  if (maxn == 1L) return(c(0L, 1)[n+1L])
  if (maxn == 2L) return(c(0L, 1L, 1L)[n+1L])
  z <- c(0L, 1L, 1L, rep(NA, maxn - 3L))
  for (i in 4L:(maxn + 1L)) {
    z[i] <- z[i-1L] + z[i-2L]
  }
  
  z[n+1L]
  
}
