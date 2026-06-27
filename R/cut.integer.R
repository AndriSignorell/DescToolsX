


#' @export
cut.integer <- function(x, breaks, labels = NULL, include.lowest = FALSE, 
                        right = TRUE, ordered_result = FALSE, ...) {
  
  # labels are constructed using "(a,b]" interval notation in cut.default,
  # which is perfectly fine for numeric variables, but not well suited for
  # integers, for which an explicit formulation is more appropriate

  .FmInf <- function(x) {
    x[!is.finite(x)] <- ".."
    x
  }
  
  # Only construct integer-style labels when breaks is already a vector —
  # if breaks is a scalar, delegate entirely to cut.default which computes
  # the break points first
  if (is.null(labels) && length(breaks) > 1L) {
    from <- head(breaks, -1)
    to   <- breaks[-1]
    labels <- if (right)
      paste(.FmInf(from + 1), .FmInf(to), sep = "-")
    else
      paste(.FmInf(from), .FmInf(to - 1), sep = "-")
  }
  
  cut.default(x = x, breaks = breaks, labels = labels,
              include.lowest = include.lowest, right = right,
              ordered_result = ordered_result, ...)
}



#' #' @export
#' cut.integer <- function(x, breaks, labels = NULL, include.lowest = FALSE, right = TRUE, 
#'                         ordered_result = FALSE, ...){
#'   
#'   # labels are constructed using "(a,b]" interval notation in cut.default, 
#'   # which is perfectly fine for numeric variables, but not well suited for 
#'   # integers, for which an explicit formulation is more appropriate
#'   
#'   
#'   .FmInf <- function(x){
#'     x[!is.finite(x)] <- ".."
#'     return(x)
#'   }
#'   
#'   if(is.null(labels)){
#'     from <- head(breaks, -1)
#'     to <- breaks[-1]
#'     
#'     if(right)
#'       labels <- paste(.FmInf(from + 1), .FmInf(to), sep="-")
#'     else
#'       labels <- paste(.FmInf(from), .FmInf(to - 1), sep="-")
#'     
#'   }
#'   
#'   res <- cut.default(x=x, breaks=breaks, labels=labels, include.lowest=include.lowest,
#'                      right=right, ordered_result=ordered_result, ...)
#'   
#'   return(res)  
#'   
#' } 
#' 
#' 
#' 
#' 
