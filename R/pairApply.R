
#' Pairwise Calculations
#' 
#' Implements a logic to run pairwise calculations on the columns of a
#' data.frame or a matrix. 
#' 
#' This code is based on the logic of \code{cor()} and extended for asymmetric
#' functions. 
#' 
#' @param x a list, a data.frame or a matrix with columns to be processed
#' pairwise. 
#' @param FUN a function to be calculated. It is assumed, that the first 2
#' arguments denominate x and y. 
#' @param \dots the dots are passed to FUN. 
#' @param symmetric logical. Does the function yield the same result for FUN(x,
#' y) and FUN(y, x)? \cr If \code{TRUE} just the lower triangular matrix is
#' calculated and transposed. Default is FALSE. 
#' 
#' @return a matrix with the results of FUN. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{outer}}, \code{\link{combPairs}},
#' \code{\link{pairwise.table}}
#' @keywords manip
#' @examples
#' 
#' # build a pizza subset
#' d.sub <- d.pizza[,c("area","driver","operator","quality")]
#' 
#' pairApply(d.sub, FUN = cramerV, symmetric=TRUE)
#' 
#' # user defined functions are ok as well
#' pairApply(d.sub, 
#'   FUN = function(x,y) 
#'     wilcox.test(as.numeric(x), as.numeric(y))$p.value, symmetric=TRUE)
#' 
#' # asymmetric measure
#' pairApply(d.sub, FUN = lambda, direction = "row")
#' 
#' # compare to:
#' lambda(x=d.pizza$driver, y=d.pizza$operator, direction="row")  
#' lambda(x=d.pizza$driver, y=d.pizza$operator, direction="column")  
#' 
#' 
#' # the data.frame
#' pairApply(d.sub, FUN = cramerV, symmetric=TRUE)
#' 
#' # the same as matrix (columnwise)
#' m <- as.matrix(d.sub)
#' pairApply(m, FUN = cramerV, symmetric=TRUE)
#' 
#' # .. and the list interface
#' lst <- as.list(d.sub)
#' pairApply(lst, FUN = cramerV, symmetric=TRUE)
#' 
#' 



#' @export
pairApply <- function(x, FUN = NULL, ..., symmetric = FALSE){
  
  # Alternative names: pairApply, PwApply, pwapply, papply, ...
  
  if(is.function(FUN)) {
    # if FUN is a function, then save it under new name and
    # overwrite function name in FUN, which has to be character
    fct <- FUN
    FUN <- "fct"
  }
  
  if(is.matrix(x)) x <- as.data.frame(x)
  x <- as.list(x)
  
  ix <- 1:length(x)
  # pairwise logic from pairwise.table
  pp <- outer(ix, ix, function(ivec, jvec) sapply(seq_along(ivec),
                                                  function(k) {
                                                    i <- ivec[[k]]
                                                    j <- jvec[[k]]
                                                    if (i >= j)
                                                      eval(parse(text = gettextf("%s(x[[i]], x[[j]], ...)", FUN)))
                                                    else NA
                                                  }))
  # why did we need that? in any case it's wrong, if no symmetric calcs are done
  # diag(pp) <- 1
  if(symmetric){
    pp[upper.tri(pp)] <- t(pp)[upper.tri(t(pp))]
  } else {
    pp.upr <- outer(ix, ix, function(ivec, jvec) sapply(seq_along(ivec),
                                                        function(k) {
                                                          i <- ivec[[k]]
                                                          j <- jvec[[k]]
                                                          if (i >= j)
                                                            eval(parse(text = gettextf("%s(x[[j]], x[[i]], ...)", FUN)))
                                                          else NA
                                                        }))
    pp[upper.tri(pp)] <- t(pp.upr)[upper.tri(pp.upr)]
    
  }
  
  dimnames(pp) <- list(names(x),names(x))
  
  return(pp)
}

