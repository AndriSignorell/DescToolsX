
#' Mode (most Frequent Value(s))
#' 
#' Calculate the mode, the most frequent value, of a numeric or character
#' vector x.
#' 
#' The mode is mainly useful for qualitative data, sometimes still for
#' integer vectors.
#'
#' For numeric vectors, the interest lies less in central tendency than in
#' conspicuous accumulation points, which can indicate data errors.
#' \code{desc()} therefore reports it within the numeric description once the
#' frequency of a single value exceeds a threshold, since ties are generally
#' unexpected in numeric data unless the generating process explains them.
#'
#' \code{NA} handling follows the package standard: a single \code{NA} yields
#' \code{NA}. This is conservative, as the mode is sometimes determined
#' unambiguously despite missing values. For \code{x = c(1,1,1,1,2,2,NA)} the
#' mode is 1 whatever the missing value is, and the modal frequency lies
#' between 4 and 5. Exploiting this is left to the user.
#'
#' The mode is elsewhere often obtained by tabulating every element and
#' returning the most frequent. This function uses a dedicated C++ data
#' structure and determines only the most frequent element, making it orders
#' of magnitude faster, especially for large numeric vectors with many
#' distinct values.
#'  
#' \strong{Note:} \verb{     }There are other approaches for determining the mode, e.g. one might use\cr
#' \code{density(x)$x[which.max(density(x)$y)]} \cr for quantitative data, resp. 
#' \code{hist()}.\cr Another interesting idea for a more
#' robust estimation of the mode: 
#' \preformatted{ peak <- optimize(function(x, model) 
#'   predict(model, data.frame(x = x)), 
#'     c(min(x), max(x)), maximum = TRUE, model = y.loess)
#'   points(peak$maximum, peak$objective) 
#' }
#' 
#' @param x a non-empty numeric vector of data values
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}.
#' @return the most frequent value as a number or character, depending on
#' \code{class(x)}. If there is more than one, all are returned in a vector.\cr
#' The modal frequency is attached as attribute named \code{"freq"}.
#' 
#' @note Great Rcpp part contributed by Joseph Wood and Ralf Stubner.
#' 
#' 
#' @references
#' \href{https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type/}{rcpp-fast-statistical-mode}
#' 
#' @examples
#' 
#' # normal mode
#' modeX(c(0:5, 5))
#' 
#' modeX(5)
#' modeX(NA)
#' modeX(c(NA, NA))
#' modeX(c(NA, 0:5))
#' modeX(c(NA, 0:5), na.rm=TRUE)
#' modeX(c(NA, 0:5, 5), na.rm=TRUE)
#' 
#' # returns all encountered modes, if several exist
#' modeX(c(0:5, 4, 5, 6))
#' 
#' modeX(Pizza$driver)
#' modeX(Pizza$driver, na.rm=TRUE)
#' modeX(as.character(Pizza$driver), na.rm=TRUE)
#' 
#' # use sapply for evaluating data.frames (resp. apply for matrices)
#' sapply(Pizza[,c("driver", "temperature", "date")], modeX, na.rm=TRUE)
#' 
#' 
#' @family location
#' @concept location
#' @export
modeX <- function(x, na.rm=FALSE) {
  
  # // Source
  # // https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type
  # // Author: Ralf Stubner, Joseph Wood
  
  if(!is.atomic(x) || is.matrix(x))
    stop("modeX supports only atomic vectors. Use sapply(*, modeX) instead.")
  
  if (na.rm) 
    x <- x[!is.na(x)]
  
  if (anyNA(x)) 
    # there are NAs, so no mode exist nor frequency
    return(structure(NA_real_, freq = NA_integer_))
  
  if(length(x) == 1L)
    # only one value in x, x is the mode
    # return(structure(x, freq = 1L)) 
    # changed to: only one value in x, no mode defined
    return(structure(NA_real_, freq = NA_integer_))
  
  # we don't have NAs so far, either there were then we've already stopped
  # or they've been stripped above
  res <- fast_mode_cpp(x, narm=FALSE)
  
  # No mode exists if the largest frequency is 1 - every value occurs once.
  #
  # NOTE: the original condition `length(res) == 0L & attr(res, "freq") == 1L`
  # was CORRECT, contrary to what I first assumed. fastModeImplX() only
  # pushes a value into `modes` once its count REACHES two (a first
  # occurrence lands in the hash map, not in modes), so an all-distinct
  # input yields an empty result with myMax still at 1. The two halves of
  # that condition are therefore equivalent, not complementary.
  # Keeping the frequency test alone: same behaviour, one clause instead
  # of a redundant pair, and no non-short-circuiting `&`.
  if(attr(res, "freq") == 1L)
    return(structure(NA_real_, freq = NA_integer_))
  
  else
    # order results kills the attribute
    return(structure(res[order(res)], freq = attr(res, "freq")))
  
}

