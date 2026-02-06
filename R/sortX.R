
#' Sort a Vector, a Matrix, a Table or a Data.frame 
#' 
#' Sort a vector, a matrix, a table or a data.frame. The base sort function
#' does not have an interface for classes other than vectors and coerces the
#' whole world to a vector. This means you get a sorted vector as result while
#' passing a matrix to \code{sort}.\cr \code{sortX} wraps the base sort function
#' and adds an interface for sorting the rows of the named 2-dimensional data
#' structures by the order of one or more of its columns. 
#' 
#' The sort order for factors is the order of their levels (which is
#' particularly appropriate for ordered factors), and usually confusing for
#' unordered factors, whose levels may be defined in the sequence in which they
#' appear in the data (which normally is unordered).
#' 
#' @name sortX
#' @aliases sortX sortX.default sortX.data.frame sortX.matrix sortX.table
#' @param x a numeric, complex. character or logical vector, a factor, a table
#' or a data.frame to be sorted.
#' @param decreasing logical. Should the sort be increasing or decreasing?
#' @param factorsAsCharacter logical. Should factors be sorted by the
#' alphabetic order of their labels or by the order or their levels.  Default
#' is \code{TRUE} (by labels).
#' @param ord vector of integers or columnames. Defines the columns in a table,
#' in a matrix or in a data.frame to be sorted for. \cr 0 means row.names, 1:n
#' the columns and n+1 the marginal sum. See examples.
#' @param na.last for controlling the treatment of \code{NAs}. If \code{TRUE},
#' missing values in the data are put last; if \code{FALSE}, they are put
#' first; if \code{NA}, they are removed (see \code{\link{order}}.)
#' @param \dots further arguments to be passed to or from methods.
#' @return the sorted object.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{sort}}, \code{\link{order}}
#' @keywords manip
#' @examples
#' 
#' d.frm <- d.pizza[1:10, c("driver","temperature","delivery_min")]
#' 
#' sortX(d.frm[,1])
#' # sortX follows the levels by default
#' levels(d.frm[,1])
#' 
#' sortX(x=d.frm, ord="driver", decreasing=FALSE)
#' # set factorsAsCharacter = TRUE, if alphabetical order is required
#' sortX(x=d.frm, ord="driver", decreasing=FALSE, factorsAsCharacter=TRUE)
#' 
#' sortX(x=d.frm, ord=c("driver","delivery_min"), factorsAsCharacter = TRUE)
#' sortX(x=d.frm, ord=c("driver","delivery_min"), factorsAsCharacter = FALSE)
#' 
#' sortX(x=d.frm, ord=c("driver","delivery_min"), decreasing=c(FALSE, TRUE),
#'   factorsAsCharacter = FALSE)
#' 
#' # Sorting tables
#' tab <- table(d.pizza$driver, d.pizza$area)
#' 
#' sortX(x=tab, ord=c(0,2), decreasing=c(TRUE, FALSE))
#' sortX(x=tab, ord=2, decreasing=TRUE)
#' 
#' # partial matching ok:
#' sortX(tab, o=1, d=TRUE)
#' 

#' @rdname sortX
#' @export
sortX <- function(x, ...) {
  UseMethod("sortX")
}


#' @rdname sortX
#' @export
sortX.default <- function(x, ...) {
  sort(x = x, ...)
}

#' @rdname sortX
#' @export
sortX.data.frame <- function(x, ord = NULL, decreasing = FALSE, factorsAsCharacter = TRUE,
                            na.last = TRUE, ...) {
  
  # why not using ord argument as in matrix and table instead of ord?
  
  if(is.null(ord)) { ord <- 1:ncol(x) }
  
  if(is.character(ord)) {
    ord <- match(ord, c("row.names", names(x)))
  } else if(is.numeric(ord)) {
    ord <- as.integer(ord) + 1
  }
  
  # recycle decreasing and by
  lgp <- list(decreasing = decreasing, ord = ord)
  # recycle all params to maxdim = max(unlist(lapply(lgp, length)))
  lgp <- lapply(lgp, rep, length.out = max(unlist(lapply(lgp, length))))
  # decreasing is not recycled in order, so we use rev to change the sorting direction
  # old: d.ord <- x[,lgp$ord, drop=FALSE]  # preserve data.frame with drop = FALSE
  d.ord <- data.frame(rn=rownames(x),x)[, lgp$ord, drop = FALSE] # preserve data.frame with drop = FALSE
  if(factorsAsCharacter){
    for( xn in which(sapply(d.ord, is.factor)) ){ d.ord[,xn] <- factor(d.ord[,xn], levels=sort(levels(d.ord[,xn]))) }
  }
  
  d.ord[, which(sapply(d.ord, is.character))] <- lapply(d.ord[,which(sapply(d.ord, is.character)), drop=FALSE], factor)
  d.ord <- data.frame(lapply(d.ord, as.numeric))
  d.ord[lgp$decreasing] <- lapply(d.ord[lgp$decreasing], "-")
  
  x[ do.call("order", c(as.list(d.ord), na.last=na.last)), , drop = FALSE]
}



#' @rdname sortX
#' @export
sortX.matrix <- function (x, ord = NULL, decreasing = FALSE, na.last = TRUE, ...) {
  
  if (length(dim(x)) == 1 ){
    # do not specially handle 1-dimensional matrices
    res <- sort(x=x, decreasing=decreasing)
    
  } else {
    if (is.null(ord)) {
      # default order by sequence of columns
      ord <- 1:ncol(x)
    }
    
    # replace keyword by code
    ord[ord=="row_names"] <- 0
    # we have to coerce, as ord will be character if row_names is used
    ord <- as.numeric(ord)
    
    lgp <- list(decreasing = decreasing, ord = ord)
    lgp <- lapply(lgp, rep, length.out = max(unlist(lapply(lgp, length))))
    
    if( is.null(row.names(x))) {
      d.x <- data.frame(cbind(rownr=1:nrow(x)), x)
    } else {
      d.x <- data.frame(cbind( rownr=as.numeric(factor(row.names(x))), x))
    }
    d.ord <- d.x[, lgp$ord + 1, drop = FALSE]
    d.ord[lgp$decreasing] <- lapply(d.ord[lgp$decreasing], "-")
    
    res <- x[do.call("order", c(as.list(d.ord), na.last=na.last)), , drop=FALSE]
    # old version cannot be used for [n,1]-matrices, we switch to reset dim
    # class(res) <- "matrix"
    # 19.9.2013: dim kills rownames, so stick to drop = FALSE
    # dim(res) <- dim(x)
  }
  
  return(res)
  
}


#' @rdname sortX
#' @export
sortX.table <- function (x, ord = NULL, decreasing = FALSE, na.last = TRUE, ...) {
  
  if (length(dim(x)) == 1 ){
    # do not specially handle 1-dimensional tables
    res <- sort(x=x, decreasing=decreasing)
    
  } else {
    if (is.null(ord)) {
      ord <- 1:ncol(x)
    }
    lgp <- list(decreasing = decreasing, ord = ord)
    lgp <- lapply(lgp, rep, length.out = max(unlist(lapply(lgp, length))))
    
    d.x <- data.frame(cbind( rownr=as.numeric(factor(row.names(x))), x, mar=apply(x, 1, sum)))
    d.ord <- d.x[, lgp$ord + 1, drop = FALSE]
    d.ord[lgp$decreasing] <- lapply(d.ord[lgp$decreasing], "-")
    
    res <- x[do.call("order", c(as.list(d.ord), na.last=na.last)), , drop=FALSE]
    class(res) <- "table"
  }
  
  return(res)
  
}

