#' Find Matching (or Non-Matching) Elements
#' 
#' \code{%nin%} is a binary operator, which returns a logical vector indicating
#' if there is a match or not for its left operand. A true vector element
#' indicates no match in left operand, false indicates a match.
#' 
#' 
#' @param x a vector (numeric, character, factor)
#' @param table a vector (numeric, character, factor), matching the mode of
#' \code{x}
#' 
#' @return vector of logical values with length equal to length of \code{x}.
#' 
#' @author Frank E Harrell Jr <f.harrell@@vanderbilt.edu> %% ~~who you are~~
#' @seealso \code{\link{match}}, \code{\link{%in%}}, \code{\link{between}}
#' @keywords manip character
#' @examples
#' 
#' c('a','b','c') %nin% c('a','b')
#' 


# Not %in% operator
#' @export
`%nin%` <- function(x, table) 
  match(x, table, nomatch = 0) == 0


