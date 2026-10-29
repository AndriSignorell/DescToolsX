#' (Robust) Range 
#' 
#' Computes the width of a numeric vector, optionally after trimming, or a
#' robust range based on a trimmed mean and absolute deviations.
#'
#' Unlike [range()], which returns the minimum and maximum, `rangeX()` returns
#' their difference and stores the endpoints in the `"bounds"` attribute.
#'
#' With `robust = TRUE`, only finite observations are used after handling
#' missing values according to `na.rm`. Let `n` be their number and `m` their
#' mean after trimming a fraction `trim` from each tail. The absolute
#' deviations from `m` are sorted, and the smallest
#' `k = ceiling((1 - trim) * n)` deviations are retained. The scale `s` is
#' their sum divided by `k - 1`; it is not a variance or standard deviation.
#' The bounds are `max(m - fac * s, min(x))` and
#' `min(m + fac * s, max(x))`, with extrema taken over the finite observations.
#' Thus, `trim` acts on both tails when estimating the centre but only on the
#' upper tail of the absolute deviations when estimating the scale.
#'
#' If `k < 3` or the scaled deviation measure is zero, a warning is issued
#' and the ordinary range of the finite observations is returned. If no
#' finite observations remain, the robust computation stops with an error.
#'
#' @param x a numeric vector whose range width is to be computed. In robust
#' mode, `Inf` and `-Inf` are excluded; see `na.rm` for missing values.
#' @param trim a single numeric trimming fraction between 0 and 0.5, or
#' `NULL` (the default). `NULL` selects 0 when `robust = FALSE` and 0.2 when
#' `robust = TRUE`. In conventional mode, this fraction is removed from each
#' end of `x` before computing the extrema. In robust mode, it controls
#' trimming of both the centre and the absolute deviations as described
#' above; it does not directly specify the fraction of observations outside
#' the returned bounds. A non-finite value in robust mode is replaced by 0.2.
#' @param robust a single logical value; `FALSE` (the default) returns the
#' conventional, optionally trimmed range width. `TRUE` returns the width
#' between the robust bounds described above.
#' @param na.rm a single logical value indicating whether `NA` and `NaN`
#' values should be removed before computation. The default is `FALSE`;
#' missing values then produce a missing range width and missing bounds.
#' In robust mode, infinite values are excluded regardless of `na.rm`.
#' @param ... additional arguments passed to the internal helper `.robRange`
#' when `robust = TRUE`. Currently, the supported argument is `fac`, a
#' positive finite numeric scalar (default 3) multiplying the deviation
#' scale. Larger values widen the robust bounds up to the observed finite
#' extrema; smaller values narrow them. For example,
#' `rangeX(x, robust = TRUE, fac = 2)` uses a multiplier of 2. This factor
#' does not specify a confidence level or a guaranteed coverage probability.
#' Arguments in `...` are ignored when `robust = FALSE`.
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
#' rangeX(x, robust = TRUE)
#' rangeX(x, robust = TRUE, trim = 0.1, fac = 2)
#' attr(rangeX(x, robust = TRUE), "bounds")
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
