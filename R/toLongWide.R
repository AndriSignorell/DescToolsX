
#' Reshape a Vector From Long to Wide Shape Or Vice Versa 
#' 
#' Simple reshaping a vector from long to wide or from wide to long shape by
#' means of a single factor. 
#' 
#' \code{toLong} expects x as a matrix or a data.frame and reshapes it to a
#' (long) factor representation. \code{toWide} expects the vectors x, g, by,
#' wheras x being the variable, g the splitting factor and by a vector for
#' rowwise merging. 
#' 
#' @name tolong_towide
#' @aliases toWide toLong
#' @param x the vector to be reshaped 
#' @param g the grouping vector to be used for the new columns. The resulting
#' \code{data.frame} will return one column per grouplevel. 
#' @param by a vector to be used to merge the pieces of \code{x}. If this is
#' left to \code{NULL} the pieces will be merged by rownames in the order they
#' are supplied. 
#' @param varnames the variable names if not the grouping levels should be
#' used.
#' @param incl.rownames logical (default \code{FALSE}), if set to \code{TRUE} a
#' column containing the rownames will be appended at the end of the
#' data.frame.
#' @return the reshaped object of \code{data.frame} class
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{reshape}} 
#' @keywords manip
#' @examples
#' 
#' d.x <- read.table(header=TRUE, text="
#' AA BB CC DD EE FF GG
#' 7.9 18.1 13.3 6.2 9.3 8.3 10.6
#' 9.8 14.0 13.6 7.9 2.9 9.1 13.0
#' 6.4 17.4 16.0 10.9 8.6 11.7 17.5
#' ")
#' 
#' toLong(d.x)
#' 
#' # toWide by row numbers (by = NULL)
#' toWide(PlantGrowth$weight, PlantGrowth$group)
#' 
#' # To wide aligned by key
#' set.seed(41)
#' PlantGrowth$nr <- c(sample(12, 10), sample(12, 10), sample(12, 10))
#' head(PlantGrowth)
#' 
#' toWide(PlantGrowth$weight, PlantGrowth$group, by=PlantGrowth$nr)
#' 


# *********************************** 12.12.2014
# stack/unstack does exactly that

#' @rdname tolong_towide
#' @export
toLong <- function (x, varnames = NULL, incl.rownames=FALSE) {
  
  if(!is.list(x)) {
    if(is.matrix(x) || is.table(x))
      x <- as.data.frame(x)
    lst <- as.list(x)
  } else {
    lst <- x
  }
  grpnames <- names(lst)
  if(is.null(grpnames)) grpnames <- paste("X", 1:length(lst), sep="")
  res <- data.frame(rep(grpnames, lapply(lst, length)), unlist(lst))
  rownames(res) <- NULL
  if(!is.null(rownames(x)))
    rownames(res) <- do.call(paste, c(expand.grid(rownames(x), grpnames), sep="."))
  
  if(incl.rownames)
    res <- appendX(res, rep(rownames(x), times=ncol(x)), 
                  after = 2)
  
  if (is.null(varnames))
    varnames <- c("grp", "x", "rowname")
  
  colnames(res) <- varnames[seq(ncol(res))]
  
  return(res)
  
}



#' @rdname tolong_towide
#' @export
toWide <- function(x, g, by=NULL, varnames=NULL){
  
  if(is.null(varnames))
    varnames <- levels(g)
  
  if(is.null(by)){
    by <- "row.names"
    
  }  else {
    x <- data.frame(x, idx=by)
    by <- "idx"
    varnames <- c("by", varnames)
  }
  
  g <- factor(g)
  s <- split(x, g)
  
  if(by != "row.names"){
    # set the columnname for the value according to the group level
    # in order to avoid duplicate names in Reduce() down the road ...
    for(i in seq(s)){
      colnames(s[[i]])[1] <- names(s)[i]
    }
  }
  
  res <- Reduce(function(x, y) {
    
    # overwrite the last column name, in order to avoid:
    # Warning message:
    #   In merge.data.frame(x, y, by = by, all.x = TRUE, all.y = TRUE) :
    #   column names 'y.x', 'y.y' are duplicated in the result
    
    if(inherits(x, "data.frame"))
      if(colnames(x)[ncol(x)] != "idx")
        colnames(x)[ncol(x)] <- paste0(colnames(x)[ncol(x)-1], "y")
    
    z <- merge(x, y, by=by, all.x=TRUE, all.y=TRUE)
    # kill the rownames
    if(by=="row.names") z <- z[, -grep("Row.names", names(z))]
    return(z)
  }, s)
  
  colnames(res) <- varnames
  return(res)
  
}
