
#' Back to Basics with Tibbles
#' 
#' Sometimes we might wish for the old days be back and want to work with
#' familiar objects. This function helps to convert \code{tibbles} to
#' \code{data.frames} as smoothly as possible.
#' 
#' 
#' @param x the object to be converted.
#' @param \dots arguments passed on.
#' @return converted object
#' @author Andri Signorell <andri@@signorell.net>
#' @examples
#' 
#' \dontrun{
#' # read a Stata file
#' url <- "http://www.stata.com/videos13/data/webclass.dta"
#' d.webclass <- toBaseR(haven::read_dta(url))
#' 
#' # read a SPSS file
#' url <- "https://stats.idre.ucla.edu/wp-content/uploads/2020/10/missing.sav"
#' d.miss <- toBaseR(haven::read_sav(url))
#' }


#' @export
toBaseR <- function(x, ...){
  UseMethod("toBaseR")
}


#' @export
toBaseR.tbl_df <- function(x, ...){
  # rollback a tibble to data.frame, with usual factors etc.
  res <- as.data.frame(x)
  
  # get rid of unimportant SPSS specific attributes
  res <- as.data.frame(
    lapply(res, 
           DescToolsX::stripAttr, 
           attr=c("format.spss", "display_width", "format.stata"))) 
  
  for(i in which(sapply(x, inherits, "haven_labelled") )){
    res[i] <- toBaseR.haven_labelled(x[i])
  }
  
  return(res)
}


#' @export
toBaseR.haven_labelled <- function(x, ...){
  haven::as_factor(x, ...)
}


#' @export
toBaseR.default <- function(x, ...){
  warning(gettextf('Not implemented for class(es) "%s"', paste(class(x), collapse=", ")))
}
