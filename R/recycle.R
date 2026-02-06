
#' Recyle a List of Elements 
#' 
#' This function recycles all supplied elments to the maximal dimension. 
#' 
#' 
#' @param \dots a number of vectors of elements. 
#' 
#' @return a list of the supplied elements\cr \code{attr(,"maxdim")} contains
#' the maximal dimension of the recyled list 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{rep}}, \code{\link{replicate}} 
#' 
#' @keywords utilities
#' @examples
#' 
#' recycle(x=1:5, y=1, s=letters[1:2])
#' 
#' z <- recycle(x=letters[1:5], n=2:3, sep=c("-"," "))
#' sapply(1:attr(z, "maxdim"), function(i) paste(rep(z$x[i], times=z$n[i]), 
#'                                         collapse=z$sep[i]))
#' 

#' @export 
recycle <- function(...){
  lst <- list(...)
  
  # optimization suggestion by moodymudskipper 20.11.2019  
  maxdim <- max(lengths(lst)) # instead of max(unlist(lapply(lst, length)))
  # recycle all params to maxdim
  # res <- lapply(lst, rep_len, length.out=maxdim)
  
  # rep_len would not work for Dates
  res <- lapply(lst, rep, length.out=maxdim)
  
  attr(res, "maxdim") <- maxdim
  
  return(res)
}

