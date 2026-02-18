
#' List Objects, Functions Or Data in a Package 
#' 
#' List all the objects, functions or data in a package. 
#' 
#' This is just a wrapper for \code{\link{ls}}, \code{\link{ls.str}} and
#' \code{\link{lsf.str}} with the appropriate arguments (as I always forgot how
#' to do the trick). \code{LsObj()} lists all objects, \code{LsFct()} just the
#' functions in a package. 
#' 
#' @param package the name of the package 
#' @param exported logical (default \code{TRUE}) should only exported functions be listed?
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
#' lsFunctions("DescToolsX")
#' 

#' @export
lsFunctions <- function(package, exported=TRUE) {
  as.vector(unclass(lsf.str(pos = gettextf("package:%s", package) )))
}

