
#' Multiple Gsub 
#' 
#' Performs multiple substitions in one string or multiple strings. 
#' 
#' 
#' @param pattern character string containing a regular expression (or
#' character string for fixed = TRUE) to be matched in the given character
#' vector. Coerced by as.character to a character string if possible.
#' @param replacement a replacement for matched pattern as in \code{\link{sub}}
#' and \code{\link{gsub}}.  See there for more information.
#' @param x a character vector where matches are sought, or an object which can
#' be coerced by as.character to a character vector. Long vectors are
#' supported.
#' 
#' @return a character vector of the same length and with the same attributes
#' as x (after possible coercion to character).
#' 
#' @seealso \code{\link{gsub}}
#' @examples
#' 
#' x <- c("ABC", "BCD", "CDE")
#' mgsub(pattern=c("B", "C"), replacement=c("X","Y"), x)
#' 
 

#' @export
mgsub <- function(pattern, replacement, x) {
  
  stringi::stri_replace_all_fixed(
    x, pattern, replacement, vectorize_all = FALSE
  )
  
}


# old:
# mgsub <- function(pattern, replacement, x, ...) {
#   if (length(pattern) != length(replacement))
#     stop("pattern and replacement do not have the same length.")
#   result <- x
#   for (i in seq_along(pattern))
#     result <- gsub(pattern[i], replacement[i], result, ...)
#   result
# }

