
#' Inverse Which 
#' 
#' The inverse function to \code{\link{which}} creates a logical vector/matrix
#' from indices. 
#' 
#' 
#' @param idx the indices as returned by \code{\link{which}}. 
#' @param n integer, the length of the original vector. This must not be less
#' than \code{max(idx)}, which is also the default. 
#' @param useNames logical, determining if the names of the indices should be
#' preserved. 
#' @return a logical vector of the length n, with \code{TRUE} on the positions
#' \code{i}. 
#' \item{comp1 }{Description of 'comp1'} 
#' @author Nick Sabbe 
#' @seealso \code{\link{which}} 
#' @references
#' \url{https://stackoverflow.com/questions/7659833/inverse-of-which} 
#' @keywords attribute logic
#' @examples
#' 
#' ll <- c(TRUE, FALSE, TRUE, NA, FALSE, FALSE, TRUE)
#' names(ll) <- letters[seq(ll)]
#' i <- which(ll)
#' # back again (loosing the names of the FALSEs)
#' unwhich(i, length(ll))
#' 
#' 


#' @export
unwhich <- function(idx, n = max(idx), useNames=TRUE){
  
  # Author: Nick Sabbe
  # http://stackoverflow.com/questions/7659833/inverse-of-which
  
  # less performant, but oneliner:
  #   is.element(seq_len(n), i)
  
  if(n < max(idx)){
    warning(gettextf("n=%s must not be less than max(idx)=%s, which currently is the case", n, max(idx)))
    return(NA)
  }
  
  res <- logical(n)
  
  if(length(idx) > 0L) {
    res[idx] <- TRUE
    if(useNames) names(res)[idx] <- names(idx)
  }
  
  return(res)
  
}


