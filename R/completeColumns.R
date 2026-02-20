
#' Find Complete Columns 
#' 
#' Return either the columnnames or a logical vector indicating which columns
#' are complete, i.e., have no missing values. 
#' 
#' 
#' @param x a data.frame containing the data 
#' @param which logical, determining if the names of the variables should be
#' returned or a if a logical vector indicating which columns are complete
#' should be returned. 
#' 
#' @return A logical vector specifying which columns have no missing values
#' across the entire sequence. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{is.na}}, \code{\link{na.omit}},
#' \code{\link{complete.cases}} 
#' 
#' @keywords logic
#' @examples
#' 
#' completeColumns(d.pizza)
#' completeColumns(d.pizza, which=FALSE)
#' 

#' @export 
completeColumns <- function(x, which=TRUE){
  if(which)
    names(Filter(isZero, sapply(x, function(z) sum(is.na(z)))))
  else 
    sapply(x, function(z) sum(is.na(z)))==FALSE
}


