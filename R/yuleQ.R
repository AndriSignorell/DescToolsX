

#' Yule Q and Yule Y measures
#' 
#' Calculate Yule Q and Yule Y, two measures of association for ordinal
#' factors in a two-way table.\cr The function has interfaces for a table
#' (matrix) and for single vectors. 
#' @inheritParams Association
#' @name Yule

#' @rdname Yule
#' @export
yuleQ <- function(x, y = NULL, ...){
  
  if(!is.null(y)) x <- table(x, y, ...)
  
  # allow only 2x2 tables
  stopifnot(prod(dim(x)) == 4 || length(x) == 4)
  
  a <- x[1,1]
  b <- x[1,2]
  c <- x[2,1]
  d <- x[2,2]
  return((a*d- b*c)/(a*d + b*c))  #Yule Q
  
}


#' @rdname Yule
#' @export
yuleY <- function(x, y = NULL, ...){
  
  if(!is.null(y)) x <- table(x, y, ...)
  
  # allow only 2x2 tables
  stopifnot(prod(dim(x)) == 4 || length(x) == 4)
  
  a <- x[1,1]
  b <- x[1,2]
  c <- x[2,1]
  d <- x[2,2]
  return((sqrt(a*d) - sqrt(b*c))/(sqrt(a*d)+sqrt(b*c))) # YuleY
  
}
