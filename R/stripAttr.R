
#' Remove Attributes from an Object 
#' 
#' For convenience we sometimes want to strip some or all attributes in a
#' oneliner.  
#' 
#' @name set_strip_attr
#' @aliases stripAttr SetAttr
#' @param x the object whose attributes should be removed or to which an
#' attribute should be added. 
#' @param attr name of a new attribute
#' @param attr_val value for the new attribute \code{attr}
#' @param attr_names a vector with attribute names, which will be removed.
#' Leaving the default to \code{NULL} will cause all the attributes to be
#' deleted. 
#' @return the object \code{x} without the attributes contained in attr_names
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{setNamesX}}, \code{\link{unname}}
#' @examples
#' 
#' x <- runif(10)
#' x <- setAttr(x, 
#'              attr=c("some_attr", "other_attr"),
#'              attr_val=c("First attribute", "Second attribute"))
#' 
#' # strip only single
#' stripAttr(x, "other_attr")
#' 
#' # strip all attributes
#' stripAttr(x)




#' @rdname set_strip_attr
#' @export
setAttr <- function(x, attr, attr_val){
  for(i in seq_along(attr))
    attr(x, which = attr[i]) <- attr_val[i]
  return(x)
}


#' @rdname set_strip_attr
#' @export
stripAttr <- function(x, attr_names=NULL) {
  
  if(is.null(attr_names))
    attributes(x) <- NULL
  else
    for(a in attr_names) 
      attr(x, which = a) <- NULL
    
    return(x)
}

