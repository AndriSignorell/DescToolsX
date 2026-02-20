
#' Number and Samples for Permutations or Combinations of a Set 
#' 
#' Return the set of permutations for a given set of values. The values can be
#' numeric values, characters or factors. \code{combN} computes the number of
#' combinations with and without replacement and order, whereas \code{combSet}
#' returns the value sets.
#' 
#' @param x a vector of numeric values or characters. Characters need not be
#' unique. 
#' @param sort logical, defining if the result set should be sorted. Default is
#' FALSE. 
#' @return a matrix with all possible permutations of the
#' values in x
#' 
#' @author Friederich Leisch 
#' 
#' @seealso [base::choose], [utils::combn()], [base::factorial()],
#' [DescToolsViz::combPairs]
#' \cr \code{vignette("Combinatorics")} 
#' 

#' @family combinatorics
#' @concept combinatorics
#'  
#' @examples
#' 
#' permn(letters[2:5])
#' permn(2:5)
#' 
#' # containing the same element more than once
#' permn(c("a", "b", "c", "a"))
#' 
#' 


#' @export
permn <- function(x, sort = FALSE) {
  
  # by F. Leisch
  
  n <- length(x)
  
  if (n == 1L)
    return(matrix(x))
  # Andri: why should we need that??? ...
  #   else if (n < 2)
  #     stop("n must be a positive integer")
  
  z <- matrix(1L)
  for (i in 2L:n) {
    y <- cbind(z, i)
    a <- c(1L:i, 1:(i - 1L))
    z <- matrix(0L, ncol = ncol(y), nrow = i * nrow(y))
    z[1L:nrow(y), ] <- y
    for (j in 2L:i - 1L) {
      z[j * nrow(y) + 1L:nrow(y), ] <- y[, a[1L:i + j]]
    }
  }
  dimnames(z) <- NULL
  
  m <- apply(z, 2L, function(i) x[i])
  
  if(any(duplicated(x)))
    m <- unique(m)
  
  if(sort) m <- sortX(m)
  return(m)
  
}

