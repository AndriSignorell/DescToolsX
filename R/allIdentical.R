
#' Test Multiple Objects for Exact Equality 
#' 
#' The function \code{\link{identical}()} is the safe and reliable way to test
#' two objects for being exactly equal. But it is restricted to the comparison
#' of two objects. \code{allIdentical()} allows the input of multiple objects
#' and returns \code{TRUE} in the case that all of them are exactly equal,
#' \code{FALSE} in every other case. 
#' 
#' The function checks the first object against all others, so if the first
#' object is identical to the second and to the third, then also the second and
#' the third are identical. (If A=B and A=C then is B=C)
#' 
#' @param \dots any \code{R} objects
#' @author Andri Signorell <andri@@signorell.net> 
#' 
#' @seealso \code{\link{identical}()} 
#' @keywords logic programming IO iteration
#' @examples
#' 
#' A <- LETTERS[1:5]
#' B <- LETTERS[1:5]
#' C <- LETTERS[1:5]
#' D <- LETTERS[1:5]
#' E <- factor(LETTERS[1:5])
#' 
#' # all ok
#' allIdentical(A, B, C, D)
#' 
#' # at least one odd man
#' allIdentical(A, B, C, D, E)
#' 


#' @export
allIdentical <- function(...){
  lst <- list(...)
  # identical ought to be transitive, so if A is identical to C and to D, 
  # then C should be identical to D
  
  # all(sapply(lst[-1], identical, lst[[1]]))
  
  # we might not need to compare all elements
  for(i in seq_along(lst)[-1]){
    
    if(!identical(lst[[i]], lst[[1]])){
      # we can stop after the first inequality
      return(FALSE)
    }
  }
  return(TRUE)
  
  # 3 times faster than original
  
  # library(microbenchmark)
  # microbenchmark(
  #   orig = allIdentical(A, B, C, D, E),
  #   A = allIdenticalA(A, B, C, D, E), 
  #   times  = 2000L
  # )
  
  
}



