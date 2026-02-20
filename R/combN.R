
#' Number of Combinations of a Set 
#' 
#' Return the number of combinations with and without replacement and order. 
#' 
#' @param n number of elements from which to choose.
#' @param m number of elements to choose. For \code{combSet} can \code{m} be a
#' numeric vector too.
#' @param repl logical. Should repetition of the same element be allowed?
#' Defaults to FALSE
#' @param ord logical. Does the order matter? Default is \code{FALSE}.
#' 
#' @return an integer value
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso \code{\link[DescToolsViz]{combPairs}}, 
#' \code{\link{combn}}, \code{\link{choose}}, \code{\link{factorial}},
#' \cr \code{vignette("Combinatorics")} 
#' 
#' @family topic. combinatorics
#' @concept combinatorics
#'  
#' @examples
#' n <- 5; m <- 2
#' combN(n, m, repl=TRUE, ord=FALSE)
#' combN(n, m, repl=TRUE, ord=TRUE)
#' combN(n, m, repl=FALSE, ord=TRUE)
#' combN(n, m, repl=FALSE, ord=FALSE)
#' 


#' @export
combN <- function(n, m, repl=FALSE, ord=FALSE){
  # return the number for the 4 combinatoric cases
  # n <- length(x)
  if(repl){
    res <- n^m
    if(!ord){
      res <- choose(n+m-1, m)
    }
  } else {
    if(ord){
      # res <- choose(n, m) * factorial(m)
      # res <- gamma(n+1) / gamma(m+1)
      # avoid numeric overflow
      res <- exp(lgamma(n + 1L) - lgamma(n - m + 1L))
    } else {
      res <- choose(n, m)
    }
  }
  
  return(res)
  
}

