

## utils: manipulation, utilities ====


inDots <- function(..., arg, default){
  
  # was arg in the dots-args? parse dots.arguments
  arg <- unlist(match.call(expand.dots=FALSE)$...[arg])
  
  # if arg was not in ... then return default
  if(is.null(arg)) arg <- default
  
  return(arg)
  
}



stripAttr <- function(x, attr_names=NULL) {
  
  if(is.null(attr_names))
    attributes(x) <- NULL
  else
    for(a in attr_names) 
      attr(x, which = a) <- NULL
    
    return(x)
}



# internal function, no use to export it.. (?)
.NormWeights <- function(x, weights, na.rm=FALSE, zero.rm=FALSE, normwt=FALSE) {
  
  # Idea Henrik Bengtsson
  # we remove values with zero (and negative) weight. 
  # This would:
  #  1) take care of the case when all weights are zero,
  #  2) it will most likely speed up the sorting.
  
  if (na.rm){
    
    keep <- !is.na(x) & !is.na(weights)
    
    if(zero.rm)
      # remove values with zero weights
      keep <- keep & (weights > 0)
    
    x <- x[keep]
    weights <- weights[keep]
  } 
  
  if(any(is.na(x)) | (!is.null(weights) & any(is.na(weights))))
    return(NA_real_)
  
  n <- length(x)
  
  if (length(weights) != n) 
    stop("length of 'weights' must equal the number of rows in 'x'")
  
  # x and weights have length=0
  if(length(x)==0)
    return(list(x = x, weights = x, wsum = NaN))
  
  if (any(weights< 0) || (s <- sum(weights)) == 0) 
    stop("weights must be non-negative and not all zero")
  
  
  # we could normalize the weights to sum up to 1
  if (normwt) 
    weights <- weights * n/s
  
  return(list(x=x, weights=as.double(weights), wsum=s))
  
}





