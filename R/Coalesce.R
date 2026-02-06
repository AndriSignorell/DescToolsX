
#' Return the First Element Not Being NA
#' 
#' Return the first element of a vector, not being NA.
#' 
#' If several vectors are supplied, the evaluation will be elementwise, resp.
#' rowwise if x is a data.frame or a matrix. The first element of the result is
#' the first non \code{NA} element of the first elements of all the arguments,
#' the second element of the result is the one of the second elements of all
#' the arguments and so on. \cr Shorter inputs (of non-zero length) are NOT
#' recycled. The function will bark, if multiple vectors do not all have the
#' same dimension.\cr The idea is borrowed from SQL. Might sometimes be useful
#' when preparing data in R instead of in SQL.
#' 
#' @param \dots the elements to be evaluated. This can either be a single
#' vector, several vectors of same length, a matrix, a data.frame or a list of
#' vectors (of same length). See examples.
#' @param method one out of \code{"is.na"} (default), \code{"is.null"} or
#' \code{"is.finite"}. The \code{"is.na"} option allows \code{Inf} values to be
#' in the result, the second one eliminates them.
#' @param flatten logical, defines whether lists are going to be flattened
#' (default \code{TRUE}).
#' @return return a single vector of the first non \code{NA} element(s) of the
#' given data structure.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{is.na}}, \code{\link{is.finite}}
#' @keywords manip
#' @examples
#' 
#' Coalesce(c(NA, NA, NA, 5, 3))
#' Coalesce(c(NA, NULL, "a"))
#' Coalesce(NULL, 5, 3)
#' 
#' d.frm <- data.frame(matrix(c(
#'   1, 2, NA, 4,
#'   NA, NA, 3, 1,
#'   NaN, 2, 3, 1,
#'   NA, Inf, 1, 1), nrow=4, byrow=TRUE)
#' )
#' 
#' Coalesce(d.frm)
#' Coalesce(as.matrix(d.frm))
#' Coalesce(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4)
#' Coalesce(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4, method="is.finite")
#' Coalesce(list(d.frm[,1], d.frm[,2]))
#' 
#' # returns the first finite element
#' Coalesce(d.frm, method="is.finite")
#' 
#' # with characters (take care, factors won't work!)
#' # is.finite does not make sense here...
#' d.frm <- data.frame(matrix(c(
#'   "a", "b", NA, "4",
#'   NA, NA, "g", "m",
#'   NA_character_,"hfdg", "rr", "m",
#'   NA, Inf, 1, 1), nrow=4, byrow=TRUE)
#' , stringsAsFactors = FALSE)
#' 
#' Coalesce(d.frm$X1, d.frm$X2, d.frm$X3, d.frm$X4)
#' Coalesce(d.frm)
#' Coalesce(as.list(d.frm))
#' 

# would not return characters correctly

#' @export
Coalesce <- function(..., method = c("is.na", "is.null","is.finite"), flatten=TRUE) {
  # Returns the first element in x which is not NA
  
  # problem: if we want the first list element of ... which is not NULL
  # the function fails and returns the first element of this list element
  # by using unlist().
  # An alternative would be: Filter(Negate(is.null), list(...))
  
  if(...length() > 1L) {
    if(all(lapply(list(...), length) > 1L)){
      lst <- data.frame(..., stringsAsFactors = FALSE)
    } else {
      lst <- list(...)
      if(flatten) lst <- unlist(lst)
    }
  } else {
    if(is.matrix(...)) {
      lst <- data.frame(..., stringsAsFactors = FALSE)
    } else {
      lst <- (...)
    }
  }
  
  switch(match.arg(method, choices=c("is.na", "is.null", "is.finite")),
         
         # "is.na"     = res <- 
         #           Reduce(function (x,y) ifelse(!is.na(x), x, y), x),
         # "is.finite" = res <- 
         #   Reduce(function (x,y) ifelse(is.finite(x), x, y), lst)
         
         "is.na"     = res <- 
           Reduce(function (x, y){ 
             i <- which(is.na(x))
             x[i] <- y[i]
             return(x)
           }, lst) ,
         
         "is.null"     = res <- 
           Reduce(function (x, y){ 
             i <- which(is.null(x))
             x[i] <- y[i]
             return(x)
           }, lst) ,
         
         "is.finite"     = res <- 
           Reduce(function (x, y){ 
             i <- which(is.finite(x))
             x[i] <- y[i]
             return(x)
           }, lst) 
  )
  
  return(res)
}

