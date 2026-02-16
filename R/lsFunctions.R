
#' List Objects, Functions Or Data in a Package 
#' 
#' List all the objects, functions or data in a package. 
#' 
#' This is just a wrapper for \code{\link{ls}}, \code{\link{ls.str}} and
#' \code{\link{lsf.str}} with the appropriate arguments (as I always forgot how
#' to do the trick). \code{LsObj()} lists all objects, \code{LsFct()} just the
#' functions in a package. 
#' 
#' @name FunctionUtils
#' @aliases lsFunctions funArgs funCalls
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{lsf.str}} 
#' 
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}. Wadsworth & Brooks/Cole. 
#' 
#' @keywords utilities
#' @examples
#' 
#' lsFunctions("DescToolsX")
#' 

#' @param package the name of the package 
#' @param exported logical (default \code{TRUE}) should only exported functions be listed?
#' @rdname FunctionUtils
#' @export
lsFunctions <- function(package, exported=TRUE) {
  as.vector(unclass(lsf.str(pos = gettextf("package:%s", package) )))
}


#' @param name the name of the function
#' @param sort logical (default \code{FALSE}) should the arguments be alphabetically sorted?
#' @rdname FunctionUtils
#' @export
funArgs <- function(name, sort=FALSE) {
  
  
  # got that somewhere, but don't know from where...
  
  if(is.function(name)) name <- as.character(substitute(name))
  a <- formals(get(name, pos=1))
  if(is.null(a))
    return(NULL)
  arg.labels <- names(a)
  arg.values <- as.character(a)
  char <- sapply(a, is.character)
  arg.values[char] <- paste("\"", arg.values[char], "\"", sep="")
  
  if(sort)
  {
    ord <- order(arg.labels)
    if(any(arg.labels == "..."))
      ord <- c(ord[-which(arg.labels[ord]=="...")],
               which(arg.labels=="..."))
    arg.labels <- arg.labels[ord]
    arg.values <- arg.values[ord]
  }

  output <- data.frame(value=I(arg.values), row.names=arg.labels)
  class(output) <- "FunArgs"
  
  return(list(output, 
              string=paste(gettextf("%s = %s", arg.labels, arg.values),
                           collapse = ", ")))
  
}


#' #' @rdname FunctionUtils
#' #' @export
#' print.FunArgs <- function(x, ...) {
#'   print(
#'     paste(gettextf("%s = %s", rownames(x), x),
#'           collapse = ", ")
#'   )
#' }




#' @rdname FunctionUtils
#' @export
funCalls <- function (name, package=NULL, sort=FALSE) {
  
  tmp <- utils::getParseData(parse(text = getAnywhere(name), keep.source = TRUE))
  nms <- tmp$text[which(tmp$token == "SYMBOL_FUNCTION_CALL")]
  funs <- unique(if (sort) {
    sort(nms)
  } else {
    nms
  })
  
  src <- paste(as.vector(sapply(funs, find)))
  outlist <- tapply(funs, factor(src), c)
  
  if(!is.null(package))
    outlist <- outlist[grep(package, names(outlist))]
  return(outlist)
}

