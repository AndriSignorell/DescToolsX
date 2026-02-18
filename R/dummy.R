
#' Generate dummy Codes for a Factor %% ~~function to do ... ~~
#' 
#' Generate a matrix of dummy codes (class indicators) for a given factor.  %%
#' ~~ A concise (1-5 lines) description of what the function does. ~~
#' 
#' For reverting dummy codes see the approach in the examples below.
#' 
#' @param x factor or vector of classes for cases. %% ~~Describe \code{x}
#' here~~
#' @param method defines the method of the contrasts being formed. Can be one
#' out of \code{"treatment"}, \code{"sum"}, \code{"helmert"}, \code{"poly"},
#' \code{"full"}, whereas \code{"treatment"} is the default one. Abbreviations
#' are accepted.\cr The option \code{"full"} returns a full set of class
#' indicators, say a dummy factor for \bold{each} level of x.  Note that this
#' would be redundant for \code{\link{lm}()} and friends! %% ~~Describe
#' \code{method} here~~
#' @param base an integer specifying which group is considered the baseline
#' group.
#' @param levels an optional vector of the values (as character strings) that
#' \code{x} might have taken.  The default is the unique set of values taken by
#' as.character(x), sorted into increasing order of x.\cr This is directly
#' passed on to \code{\link{factor}}.
#' @return a matrix with the dummy codes. The number of rows correspond to the
#' number of elements in \code{x} and the number of columns to the number of
#' its levels - 1, respectively to the number of levels given as argument -1.
#' 
#' When \code{method = "full"} is chosen the number of columns will correspond
#' to the number of levels.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{model.frame}}, \code{\link{contrasts}},
#' \code{\link[nnet]{class.ind}} in the package \pkg{nnet}
#' @references Venables, W N and Ripley, B D (2002): \emph{Modern Applied
#' Statistics with S}. Fourth edition. Springer.
#' @keywords math
#' @examples
#' 
#' x <- c("red","blue","green","blue","green","red","red","blue")
#' dummy(x)
#' dummy(x, base=2)
#' 
#' dummy(x, method="sum")
#' 
#' 
#' y <- c("Max","Max","Max","Max","Max","Bill","Bill","Bill")
#' 
#' dummy(y)
#' dummy(y, base="Max")
#' 
#' dummy(y, base="Max", method="full")
#' 
#' 
#' # "Undummy" (revert the dummy coding)
#' m <- dummy(y, method="full")
#' m
#' z <- apply(m, 1, function(x) colnames(m)[x==1])
#' z
#' identical(y, as.vector(z))
#' 
#' m <- dummy(y)
#' m
#' z <- apply(m, 1, function(x) ifelse(sum(x)==0, attr(m,"base"), colnames(m)[x==1]))
#' z
#' 

#' @export
dummy <- function (x, method = c("treatment", "sum", "helmert", "poly", "full"),  base = 1, levels=NULL) {
  
  # Alternatives:
  # options(contrasts = c("contr.sum", "contr.poly"))
  # model.matrix(~x.)[, -1]               ### und die dummy-codes
  # or Ripley's brilliant shorty-function:
  #   diag(nlevels(x))[x,]
  
  if(is.null(levels))
    x <- factor(x)
  else
    x <- factor(x, levels=levels)
  
  if(!is.numeric(base)) base <- match(base, levels(x))
  
  method <- match.arg( arg = method, choices = c("treatment", "sum", "helmert", "poly", "full") )
  
  switch( method
          , "treatment" = { res <- contr.treatment(n = nlevels(x), base = base)[x,, drop=FALSE] }
          , "sum" = { res <- contr.sum(n = nlevels(x))[x,, drop=FALSE] }
          , "helmert" = { res <- contr.helmert(n = nlevels(x))[x,, drop=FALSE] }
          , "poly" = { res <- contr.poly(n = nlevels(x))[x,, drop=FALSE] }
          , "full" = { res <- diag(nlevels(x))[x,, drop=FALSE] }
  )
  res <- as.matrix(res) # force res to be matrix, avoiding res being a vector if nlevels(x) = 2
  
  if(method=="full") {
    dimnames(res) <- list(if(is.null(names(x))) 1L:length(x) else names(x), levels(x))
    attr(res, "base") <- NA
  } else {
    dimnames(res) <- list(if(is.null(names(x))) 1L:length(x) else names(x), levels(x)[-base])
    attr(res, "base") <- levels(x)[base]
  }
  return(res)
}
