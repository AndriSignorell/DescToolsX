
#' Number and Samples for Permutations or Combinations of a Set 
#' 
#' Return the set of permutations for a given set of values. The values can be
#' numeric values, characters or factors. \code{combN} computes the number of
#' combinations with and without replacement and order, whereas \code{combSet}
#' returns the value sets.
#' 
#' \code{combPairs} Returns all combinations of 2 out of the elements in x or x and y (if
#' defined). Combinations of the same elements will be dropped (no replacing).
#' The vector x need not contain unique values. The permutations will
#' automatically be filtered for unique sets, if the same element is given
#' twice or more.
#' 
#' If y = \code{NULL} then all combination of 2 out of x are returned. \cr If y
#' is defined then all combinations of x and y are calculated. 
#' 
#' @name combinatoric
#' @aliases permn combSet combN
#' @param x a vector of numeric values or characters. Characters need not be
#' unique. 
#' @param n number of elements from which to choose.
#' @param m number of elements to choose. For \code{combSet} can \code{m} be a
#' numeric vector too.
#' @param repl logical. Should repetition of the same element be allowed?
#' Defaults to FALSE
#' @param ord logical. Does the order matter? Default is FALSE.
#' @param sort logical, defining if the result set should be sorted. Default is
#' FALSE. 
#' @param as.list logical, defining if the results should be returned in a flat
#' list, say every sample is a single element of the resulting list. Default is
#' FALSE.
#' @param y a vector of elements, need not be same dimension as x.  If y is not
#' \code{NULL} then all combination x and y are returned. 
#' 
#' @return a matrix with all possible permutations or combinations of the
#' values in x for \code{permn} and \code{combSet}\cr if m contains more than
#' one element the result will be a list of matrices or a flat list if
#' \code{as.list} is set to \code{TRUE} \cr an integer value for \code{combN}
#' 
#' @author Friederich Leisch <Friedrich.Leisch@@boku.ac.at>\cr Andri Signorell
#' <andri@@signorell.net> (combSet, combN) 
#' @seealso \code{\link{combn}}, \code{\link{choose}}, \code{\link{factorial}},
#' \cr \code{vignette("Combinatorics")} 
#' @keywords math
#' @examples
#' 
#' permn(letters[2:5])
#' permn(2:5)
#' 
#' # containing the same element more than once
#' permn(c("a", "b", "c", "a"))
#' 
#' 
#' # only combinations of 2, but in every possible order
#' x <- letters[1:4]
#' n <- length(x)
#' m <- 2
#' 
#' # the samples
#' combSet(x, m, repl=TRUE, ord=FALSE)
#' combSet(x, m, repl=TRUE, ord=TRUE)
#' combSet(x, m, repl=FALSE, ord=TRUE)
#' combSet(x, m, repl=FALSE, ord=FALSE)
#' 
#' # the number of the samples
#' combN(n, m, repl=TRUE, ord=FALSE)
#' combN(n, m, repl=TRUE, ord=TRUE)
#' combN(n, m, repl=FALSE, ord=TRUE)
#' combN(n, m, repl=FALSE, ord=FALSE)
#' 
#' # build all subsets of length 1, 3 and 5 and return a flat list
#' x <- letters[1:5]
#' combSet(x=x, m=c(1, 3, 5), as.list=TRUE)
#' 
#' combPairs(letters[1:4])
#' combPairs(x = letters[1:4], y = LETTERS[1:2])
#' 
#' # get all pairs of combinations between factors and numerics out of a data.frame
#' combPairs(which(sapply(d.pizza, isNumeric)), which(sapply(d.pizza, is.factor)))
#' 
#' 



#' @rdname combinatoric
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




#' @rdname combinatoric
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



#' @rdname combinatoric
#' @export
combSet <- function(x, m, repl=FALSE, ord=FALSE, as.list=FALSE) {
  
  if(length(m)>1){
    res <- lapply(m, function(i) combSet(x=x, m=i, repl=repl, ord=ord))
    
  } else {
    # generate the samples for the 4 combinatoric cases
    if(repl){
      res <- as.matrix(do.call(expand.grid, as.list(as.data.frame(replicate(m, x)))))
      dimnames(res) <- NULL
      if(!ord){
        res <- unique(t(apply(res, 1L, sort)))
      }
    } else {
      if(ord){
        res <- do.call(rbind, combn(x, m=m, FUN=permn, simplify = FALSE))
      } else {
        res <- t(combn(x, m))
      }
    }
  }
  
  if(as.list){
    
    # Alternative: we could flatten the whole list
    # and now flatten the list of lists into one list
    # lst <- split(unlist(lst), rep(1:length(idx <- rapply(lst, length)), idx))
    
    if(is.list(res)){
      res <- do.call(c, lapply(res,
                               function(x){ as.list(as.data.frame(t(x), stringsAsFactors = FALSE))}))
    } else {
      res <- as.list(as.data.frame(t(res), stringsAsFactors = FALSE))
    }
    names(res) <- NULL
  }
  return(res)
  
}



#' @rdname combinatoric
#' @export
combPairs <- function(x, y = NULL) {
  # returns a data.frame with all pairwise combinations of two variables
  if( missing(y)) {  # kein y vorhanden, use x only
    data.frame( t(combn(x, 2L)), stringsAsFactors=FALSE )
    
  } else {
    # if y is defined, all.x to all.y will be returned  
    expand.grid(x, y, stringsAsFactors=FALSE )
  }
}



