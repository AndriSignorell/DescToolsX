#' (Robust) Range 
#' 
#' Determines the range of the data, which can possibly be trimmed before
#' calculating the extreme values. The robust range version is calculated on
#' the basis of the trimmed mean and variance (see Details). 
#' 
#' The R base function range returns the minimum and maximum value of a numeric
#' object. Here we return the span of a (possibly trimmed) numeric vector, say
#' the difference of maximum and minimum value.
#' 
#' If robust is set to `TRUE` the function determines the trimmed mean m
#' and then the "upper trimmed mean" s of absolute deviations from m,
#' multiplied by `fac` (fac is 3 by default). The robust minimum is then
#' defined as m-fac*s or min(x), whichever is larger, and similarly for the
#' maximum.
#' 
#' @param x a numeric vector
#' @param trim the fraction (0 to 0.5) of observations to be trimmed from each
#' end of `x` before the range is computed. Values of `trim` outside
#' that range are taken as the nearest endpoint. Default is 0 for
#' `robust = FALSE` and 0.2 for `robust = TRUE`.
#' @param robust logical; whether to return the robust or conventional range
#' @param na.rm a logical value indicating whether `NA` values should be
#' stripped before the computation proceeds
#' @param ... further arguments passed to `.robRange`, including
#' `fac`. Only used if `robust = TRUE`.
#' 
#' @return a numeric scalar containing the range width. The corresponding
#' lower and upper bounds are returned in the `"bounds"` attribute.
#' Note that the attribute is dropped by subsetting and by
#' [as.vector()], so read it before computing on the result.
#' 
#' @note Robust range contributed by Werner Stahel.
#' 
#' @examples
#' 
#' x <- c(0:10, 50)
#' xm <- rangeX(x)
#' c(xm, rangeX(x, trim = 0.10))
#' 
#' x <- c(rnorm(20), rnorm(3, 5, 20))
#' rangeX(x, robust=TRUE)
#' 
#' # compared to
#' rangeX(x)
#' 
#' @seealso [range()], [min()], [max()] 
#' 
#' @family dispersion  
#' @concept dispersion
#'
#'
#' @export
rangeX <- function(x, trim=NULL, robust=FALSE, na.rm = FALSE, ...){

  if(robust)
    .robRange(x=x, trim=trim, na.rm=na.rm, ...)
  
  else {
    if(is.null(trim))
      trim <- 0
    
    rng <- range(trim(x, trim=trim, na.rm=na.rm), na.rm=na.rm)
    res <- diff(rng)
    attr(res, "bounds") <- rng
    
    res
    
  }
  
}




## == internal helper functions ============================================

.robRange <- function(x, trim = NULL, fac = 3, na.rm = FALSE) {
  
  if(is.null(trim))
    trim <- 0.2
  
  # author: Werner Stahel
  # from:   regr.r
  
  # x[is.finite(x)] below removes NAs unconditionally, so na.rm = FALSE was
  # silently ignored in this branch while the conventional branch returned NA.
  # Missing values are now answered with NA in both branches.
  if (!na.rm && anyNA(x)) {
    res <- NA_real_
    attr(res, "bounds") <- c(NA_real_, NA_real_)
    return(res)
  }

  if(na.rm) x <- na.omit(x)
  
  ldat <- x[is.finite(x)]
  if (is.character(ldat)|length(ldat) == 0) stop("invalid data")
  trim <- c(trim, 0.2)[1]
  if (!is.finite(trim)) trim <- 0.2
  lmn <- mean(ldat, trim=trim)
  lds <- sort(abs(ldat - lmn))
  ln <- ceiling((1 - trim) * length(ldat))
  if (ln < 3) {
    warning("Not enough valid data. returning ordinary range")
    lsd <- Inf
  } else {
    lsd <- fac * sum(lds[seq_len(ln)] / (ln-1))
    if (lsd == 0) {
      warning("Robust range has width 0. returning ordinary range")
      lsd <- Inf }
  }
  bounds <- c(max(lmn - lsd, min(ldat)), min(lmn + lsd, max(ldat)))
  
  res <- diff(bounds)
  attr(res, "bounds") <- bounds
  
  return(res)
  
}
