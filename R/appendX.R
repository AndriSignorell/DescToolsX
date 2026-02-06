
#' Append Elements to Objects 
#' 
#' Append elements to a number of various objects as vectors, matrices,
#' data.frames and lists. In a matrix either rows or columns can be inserted at
#' any position. In data frames any vectors can be inserted. \code{values} will
#' be recycled to the necessary length. 
#' 
#' The vector \code{x} will be recycled to a length of the next multiple of the
#' number of rows (or columns) of the matrix \code{m} and will be inserted such
#' that the first inserted row (column) has the index \code{i}. If the dimnames
#' are given, they will be used no matter if the matrix m has already dimnames
#' defined or not. 
#' 
#' @name appendX
#' @aliases appendX appendX.matrix appendX.data.frame appendX.default
#' @param x object for the elements to be inserted 
#' @param values the elements to be inserted 
#' @param after a subscript, after which the values are to be appended. If it's
#' missing the values will be appended after the last element (or column/row).
#' @param rows logical, defining if vector should be added as row or as column.
#' Default is column (\code{rows}=\code{FALSE}).
#' @param names the dimension names for the inserted elements(s) 
#' @param \dots further arguments (not used here)
#' @return An object containing the values in x with the elements of values
#' appended after the specified element of x. 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{rbind}}, \code{\link{cbind}}, \code{\link{append}}
#' @keywords manip
#' @examples
#' 
#' 
#' appendX(1:5, 0:1, after = 3)    # the same as append
#' 
#' # Insert columns and rows
#' x <- matrix(runif(25), 5)
#' 
#' appendX(x, values=1:10, after=2, names = c("X","Y"))
#' appendX(x, values=1:10, after=2)
#' 
#' appendX(x, values=1:10, after=2, names = c("X","Y"))
#' appendX(x, values=1:10, after=2)
#' 
#' # append to a data.frame
#' d.frm <- data.frame("id"   = c(1,2,3),
#'                     "code" = c("AAA", "BBB", "CCC"),
#'                     "val"  = c(111, 222, 333))
#' z <- c(10, 20, 30)
#' 
#' appendX(d.frm, z, after=2, names="ZZZ")
#' 


#' @rdname appendX
#' @export
appendX <- function(x, values, after = NULL, ... ){
  UseMethod("appendX")
}


#' @rdname appendX
#' @export
appendX.default <- function(x, values, after = NULL, ...){
  if(is.null(after))
    after <- length(x)
  append(x, values, after)
}


#' @rdname appendX
#' @export
appendX.matrix <- function(x, values, after = NULL, rows=FALSE, names=NULL, ...){
  
  if(rows){
    nr <- dim(x)[1]
    if(missing(after) | is.null(after)) after <- nr
    
    values <- matrix(values, ncol=ncol(x))
    if(!is.null(names)){
      err <- try(row.names(x) <- names, silent = TRUE)
      if(inherits(err, "try-error"))
        warning("Could not set rownames.")
    }
    
    if(!after)
      res <- rbind(values, x)
    
    else if(after >= nr)
      res <- rbind(x, values)
    
    else
      res <- rbind(x[1L:after,, drop=FALSE], values, x[(after+1L):nr,, drop=FALSE])
    
    colnames(res) <- colnames(x)
    
  } else {
    
    nc <- dim(x)[2]
    if(missing(after) | is.null(after)) after <- nc
    
    values <- matrix(values, nrow=nrow(x))
    
    if(!is.null(names))
      colnames(values) <- names
    
    if(!after)
      res <- cbind(values, x)
    
    else if(after >= nc)
      res <- cbind(x, values)
    
    else
      res <- cbind(x[, 1L:after, drop=FALSE], values, x[, (after+1L):nc, drop=FALSE])
    
    rownames(res) <- rownames(x)
    
  }
  
  return(res)
  
}


#' @rdname appendX
#' @export
appendX.data.frame <- function(x, values, after = NULL, rows=FALSE, names=NULL, ...){
  
  # appending to a data.frame is by nature append columns, as it is 
  # intrinsically a list. 
  # Inserting rows is however clumsy by hand and so we offer an argument to
  # do that as well
  
  .InsertRow <- function(x, val, after=nrow(x)) {
    
    # insert a row in a data.frame
    # note: we should not use rbind here, as it is not general enough in cases,
    # when not only numeric values are present in the data.frame
    
    x[seq(after+1, nrow(x)+1), ] <- x[seq(after, nrow(x)), ]
    x[after, ] <- val
    
    x
    
  }
  
  if(rows)
    .InsertRow(x, values, after=after)
  
  else 
    as.data.frame(append(x, setNamesX(list(values), names=names), after = after))
  
}


#' @rdname appendX
#' @export
appendX.TOne <-  function(x, values, after = NULL, rows=TRUE, names=NULL, ...) {
  
  # appending to a TOne object means appending to a matrix while preserving the class
  # (which is lost, when using rbind)
  
  res <- appendX.matrix(x, values, after=after, rows=rows, names=names, ...)
  attr(res, "legend") <- attr(x, "legend")
  class(res) <- "TOne"
  
  return(res)
  
}
