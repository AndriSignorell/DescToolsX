
#' Random Samples and Permutations 
#' 
#' \code{Sample} takes a sample of the specified size from the elements of x
#' using either with or without replacement. The function does the same as the
#' base::sample() and offers additionally an interface for data frames. 
#' 
#' @name sampleX
#' @param x either a vector of one or more elements from which to choose, or a
#' positive integer.
#' @param size a positive number, the number of items to choose from. 
#' @param replace a non-negative integer giving the number of items to choose.
#' @param prob should sampling be with replacement? 
#' @return sampled elements in the same structure as x 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{sample}}
#' @keywords IO distribution
#' @examples
#' 
#' sampleX(d.pizza, size=5)
#' 


# sample interface for data.frames

#' @rdname sampleX
#' @export
sampleX <-  function (x, size, replace = FALSE, prob = NULL) {
  UseMethod("sampleX")
}


#' @rdname sampleX
#' @export
sampleX.data.frame <- function (x, size, replace = FALSE, prob = NULL) {
  
  x[sample(nrow(x), size, replace = replace, prob=prob), ]
  
}


#' @rdname sampleX
#' @export
sampleX.default <- function (x, size, replace = FALSE, prob = NULL)
  base::sample(x, size, replace, prob)


