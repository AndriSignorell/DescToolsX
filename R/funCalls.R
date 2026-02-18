
#' List Calls Used in Function
#' 
#' List all the calls to other functions in a function definition.
#' 
#' 
#' @name funCalls
#' 
#' @param name the name of the function
#' @param package the name of the package 
#' @param sort logical (default \code{FALSE}) should the arguments be alphabetically sorted?
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{ls}}, \code{\link{ls.str}}, \code{\link{lsf.str}} 
#' 
#' @references Becker, R. A., Chambers, J. M. and Wilks, A. R. (1988) \emph{The
#' New S Language}. Wadsworth & Brooks/Cole. 
#' 
#' @family topic.SystemTools
#' @concept Infrastructure
#' @concept Programming Utilities 
#' 
#' @examples
#' 
#' funCalls("meanX", package="DescToolsX")
#' 

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

