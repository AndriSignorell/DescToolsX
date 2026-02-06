
#' Label, Unit Attribute of an Object 
#' 
#' Set and retrieve the \code{label}, resp. \code{unit} attribute of \code{x}.
#' This can be helpful for documenting the specific meaning of a variable, of
#' an entire data.frame or any other object. For single vectors it can be
#' useful to store the unit. 
#' 
#' The label should consist of a single text (length of 1). The text may
#' contain line feeds. It can be deleted by setting the label to \code{NULL}.
#' 
#' \code{Labels()} can be used to retrieve and assign vectorized labels to
#' data.frames or lists.
#' 
#' @name Label
#' @aliases Label Label<- Labels Labels<- Unit Unit<-
#' @param x any object 
#' @param value a single string describing the object 
#' @return \code{Label} and \code{Unit} return the label attribute of x, if
#' any; otherwise, NULL.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso A more elaborated label version can be found in package \pkg{Hmisc}
#' \code{\link[Hmisc]{label}()}. 
#' @keywords misc utilities
#' @examples
#' 
#' # add a descriptive label to a variable
#' Label(d.pizza$driver) <- "the driver who delivered the pizza"
#' 
#' # technically just appending the text as attribute to the variable
#' attributes(d.pizza$driver)
#' 
#' # label is supported while describing data
#' # Desc(d.pizza$driver)
#' 
#' # The label can be deleted by setting it to NULL
#' Label(d.pizza$driver) <- NULL
#' 
#' # Labelling the columns of a data.frame is best done with a loop
#' # (all so far seen *apply aproaches lead to more complicated code...)
#' lbl <- gettextf("this is the label for %s", colnames(d.pizza))
#' for(i in seq_along(lbl))
#'   Label(d.pizza[, i]) <- lbl[i]
#' 
#' # Str(d.pizza)
#' 



#' @rdname Label
#' @export
Label <- function(x) {
  attributes(x)$label
}


#' @rdname Label
#' @export
"Label<-" <- function(x, value) {
  if(is.list(value))  stop("cannot assign a list to be an object label")
  if((length(value) != 1L) & !is.null(value)) stop("value must be character vector of length 1")
  
  attr(x, "label") <- value
  return(x)
}



#' @rdname Label
#' @export
`Labels<-` <- function(x, value) {
  if(is.list(value))  stop("cannot assign a list to be an object label")
  # if((length(value) != 1L) & !is.null(value)) stop("value must be character vector of length 1")
  
  
  if(is.atomic(x)) {
    Label(x) <- value
    
  } else {
    
    value <- rep(value, times=length(x))
    
    for(i in seq(x))
      Label(x[, i]) <- value[i]
  }
  
  return(x)
  
}

#' @rdname Label
#' @export
Labels <- function(x) {
  if(is.atomic(x))
    Label(x)
  else 
    sapply(x, Label)
}

