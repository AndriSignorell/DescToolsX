
#' Concordant and Discordant Pairs
#'
#' @description
#' Computes the number of concordant and discordant pairs as well as ties
#' for two variables or for a contingency table.
#'
#' @details
#' The function serves as a wrapper around optimized C++ implementations.
#' It supports two input modes:
#' \itemize{
#'   \item \strong{Vector mode}: \code{x} and \code{y} are numeric or ordinal vectors
#'   \item \strong{Table mode}: \code{x} is a contingency table (matrix or table)
#' }
#'
#' Missing values in vector mode are removed pairwise.
#'
#' @param x A numeric/ordinal vector or a contingency table.
#' @param y Optional second numeric/ordinal vector. If provided, vector mode is used.
#'
#' @return
#' A named numeric vector with elements:
#' \itemize{
#'   \item \code{C} Concordant pairs
#'   \item \code{D} Discordant pairs
#'   \item \code{Ties_X} Ties in \code{x}
#'   \item \code{Ties_Y} Ties in \code{y}
#'   \item \code{Ties_XY} Joint ties
#' }
#'
#' @examples
#' # vector input
#' x <- c(1, 2, 3, 1, 2)
#' y <- c(2, 1, 3, 2, 1)
#' conDisPairs(x, y)
#'
#' # table input
#' tab <- table(x, y)
#' conDisPairs(tab)
#'



#' @family assoc.ordinal  
#' @concept association-measure  
#' @concept ordinal  
#' @concept concordance
#'
#'
#' @export
conDisPairs <- function(x, y = NULL){
  
  # ============================
  # VECTOR MODE
  # ============================
  if(!is.null(y)){
    
    # ---- checks ----
    if(length(x) != length(y)){
      stop("x and y must have the same length.")
    }
    
    if(!is.numeric(x) || !is.numeric(y)){
      stop("x and y must be numeric or integer vectors.")
    }
    
    # ---- remove NA pairwise ----
    ok <- !(is.na(x) | is.na(y))
    x <- x[ok]
    y <- y[ok]
    
    if(length(x) < 2){
      return(setNamesX(rep(NA_real_, 5),
                      c("C","D","Ties_X","Ties_Y","Ties_XY")))
    }
    
    # ---- call C++ ----
    z <- condis_pairs_xy_cpp(x, y)
    
    # expect C++ to return all 5
    res <- z[c("C","D","Ties_X","Ties_Y","Ties_XY")]
    
  } else {
    
    # ============================
    # TABLE MODE
    # ============================
    
    if(!(is.matrix(x) || is.table(x))){
      stop("If 'y' is NULL, 'x' must be a contingency table or matrix.")
    }
    
    if(any(x < 0, na.rm = TRUE)){
      stop("Table counts must be non-negative.")
    }
    
    if(sum(x) < 2){
      return(setNamesX(rep(NA_real_, 5),
                      c("C","D","Ties_X","Ties_Y","Ties_XY")))
    }
    
    # ---- call table version ----
    z <- condis_pairs_tab_cpp(x)
    
    # ensure same output structure
    res <- z[c("C","D","Ties_X","Ties_Y","Ties_XY")]
  }
  
  return(res)
}

