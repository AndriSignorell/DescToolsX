
#' Hodges-Lehmann Estimator of Location
#'
#' Function to compute the Hodges-Lehmann estimator of location
#' in the one and two sample case following a clever fast algorithm
#' by John Monahan (1984).
#'
#' The Hodges-Lehmann estimator is the median of the combined
#' data points and Walsh averages.
#'
#' It is the same as the pseudo median returned as a by-product
#' of \code{\link{wilcox.test}}
#' (which however does not calculate correctly as soon as ties
#' are present).
#'
#' Note that in the two-sample case the estimator for the
#' difference in location parameters does not estimate the
#' difference in medians (a common misconception) but rather
#' the median of the difference between a sample from x and
#' a sample from y.
#'
#' @param x Numeric vector.
#' @param y Optional numeric vector.
#' @param conf.level Confidence level.
#' @param sides Character string specifying the side of the interval:
#'   \code{"two.sided"}, \code{"left"}, or \code{"right"}.
#' @param method Confidence interval method.
#' @param na.rm Logical; should missing values be removed?
#' @param ... Additional arguments passed to bootstrap procedures.
#'
#' @return
#' If \code{conf.level = NA}:
#' a single numeric estimate.
#'
#' Otherwise:
#' named numeric vector with:
#' \itemize{
#'   \item \code{est}: Hodges-Lehmann estimate
#'   \item \code{lci}: lower confidence limit
#'   \item \code{uci}: upper confidence limit
#' }
#'
#' @export
hodgesLehmann <- function(x,
                          y = NULL,
                          conf.level = NA,
                          sides = c("two.sided", "left", "right"),
                          method = c("boot"),
                          na.rm = FALSE,
                          ...) {
  

  if (na.rm) {
    if (is.null(y)) {
      x <- na.omit(x)
    } else {
      ok <- complete.cases(x, y)
      x <- x[ok]
      y <- y[ok]
    }
  }
  
  if (anyNA(x) || (!is.null(y) && anyNA(y))) {
    if (is.na(conf.level)) {
      return(NA)
    } else {
      return(c(
        est = NA,
        lci = NA,
        uci = NA
      ))
    }
  }
  
  if (!is.numeric(x))
    stop("'x' must be numeric")
  
  if (!is.null(y) && !is.numeric(y))
    stop("'y' must be numeric")
  
  if (length(x) < 1)
    stop("'x' must contain at least one observation")
  
  method <- match.arg(method)

  if (is.null(y)) {
    res <- hlqest(x)
  } else {
    res <- hl2qest(x, y)
  }
  
  if (is.na(conf.level)) {
    result <- res
    names(result) <- NULL
  } else {
    
    if (method == "boot") {
      
      # ToDo *******************
      # implement here the two sample case!!
      # ToDo *******************
      
      result <- .hodgesLehmann.boot(
        x,
        conf.level = conf.level,
        sides = sides,
        ...
      )
      
    } else {
      
      # we'll do that later down the road
      
      # lci <- n^2/2 +
      #   qnorm((1-conf.level)/2) *
      #   sqrt(n^2 * (2*n+1)/12) - 0.5
      
      # uci <- n^2/2 -
      #   qnorm((1-conf.level)/2) *
      #   sqrt(n^2 * (2*n+1)/12) - 0.5
      
      warning(
        "Confidence intervals not yet implemented ",
        "for Hodges-Lehmann estimator."
      )
      
      result <- c(
        est = res,
        lci = NA,
        uci = NA
      )
    }
  }
  
  result
}




# == internal helper functions ================================================

.hodgesLehmann.boot <- function(x,
                                conf.level,
                                sides = c("two.sided", "left", "right"),
                                ...) {
  
  sides <- match.arg(
    sides,
    choices = c("two.sided", "left", "right"),
    several.ok = FALSE
  )
  
  if (sides != "two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)
  
  args <- .extractBootArgs(list(...))
  
  # adjusted bootstrap percentile (BCa) interval
  
  boot.fun <- boot::boot(
    
    x,
    
    function(x, d)
      hlqest(x[d]),
    
    R        = args$R,
    parallel = args$parallel,
    ncpus    = args$ncpus
  )
  
  ci <- boot::boot.ci(
    boot.fun,
    conf = conf.level,
    type = args$type
  )
  
  if (args$type == "norm") {
    
    res <- c(
      est = boot.fun$t0,
      lci = ci[[4]][2],
      uci = ci[[4]][3]
    )
    
  } else {
    
    res <- c(
      est = boot.fun$t0,
      lci = ci[[4]][4],
      uci = ci[[4]][5]
    )
  }
  
  if (sides == "left") {
    
    res[3] <- Inf
    
  } else if (sides == "right") {
    
    res[2] <- -Inf
  }
  
  res
}

