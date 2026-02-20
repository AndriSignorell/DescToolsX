
#' Samples for Combinations of a Set 
#' 
#' Return the value sets of combinations.
#' 
#' @param x a vector of numeric values or characters. Characters need not be
#' unique. 
#' @param m number of elements to choose. For \code{combSet} can \code{m} be a
#' numeric vector too.
#' @param repl logical. Should repetition of the same element be allowed?
#' Defaults to FALSE
#' @param ord logical. Does the order matter? Default is FALSE.
#' @param as.list logical, defining if the results should be returned in a flat
#' list, say every sample is a single element of the resulting list. Default is
#' FALSE.
#' 
#' @return a matrix with all possible combinations of the
#' values in x\cr if m contains more than
#' one element the result will be a list of matrices or a flat list if
#' \code{as.list} is set to \code{TRUE}
#' 
#' @author Andri Signorell <andri@@signorell.net> (combSet, combN) 
#' 
#' @seealso \code{\link[DescToolsViz]{combPairs}}, 
#' \code{\link{combn}}, \code{\link{choose}}, \code{\link{factorial}},
#' \cr \code{vignette("Combinatorics")} 
#' 
#' @family topic.combinatorics
#' @concept combinatorics
#'  
#' @examples
#' x <- letters[1:4]
#' m <- 2
#' 
#' # the samples
#' combSet(x, m, repl=TRUE, ord=FALSE)
#' combSet(x, m, repl=TRUE, ord=TRUE)
#' combSet(x, m, repl=FALSE, ord=TRUE)
#' combSet(x, m, repl=FALSE, ord=FALSE)
#' 
#' # build all subsets of length 1, 3 and 5 and return a flat list
#' x <- letters[1:5]
#' combSet(x=x, m=c(1, 3, 5), as.list=TRUE)
#' 



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



