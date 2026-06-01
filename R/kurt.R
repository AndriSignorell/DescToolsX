
#' Kurtosis
#' 
#' \code{kurtX()} returns the excess kurtosis, therefore the kurtosis calculates
#' as \code{kurtX(x) + 3} if required.
#' 
#' If \code{na.rm} is \code{TRUE} then missing values are removed before
#' computation proceeds. \cr
#' 
#' The estimator for calculating kurtosis can either be:\cr 
#' \code{1: g_2 = m_4 / m_2^2 - 3 } \cr
#' \code{2: G_2 = ((n+1) g_2 + 6) * (n-1) / ((n-2)(n-3)) } \cr 
#' \code{3: b_2 = m_4 / s^4 - 3 = (g_2 + 3) (1 - 1/n)^2 - 3 } \cr
#' 
#' 1 is the typical definition used in Stata and in many older textbooks.  \cr
#' 2 is used in SAS and SPSS.  \cr 3 is used in MINITAB and BMDP. \cr
#' 
#' Cramer (1997) mentions the asymptotic standard error of the kurtosis: \cr 
#' \preformatted{ASE.kurt = sqrt((24*n*(n - 1)^2) / ((n - 3)*(n - 2)*(n + 3)*(n + 5)))} 
#' to be used for calculating the confidence intervals.  
#' This is implemented here with \code{method="classic"}. \cr 
#' However, Joanes and Gill (1998) advise
#' against this approach, pointing out that the normal assumptions would
#' virtually always be violated.  They suggest using the bootstrap method.
#' That's why the default method for the confidence interval type is set to
#' \code{"boot"}. 
#' If not further specified the boot ci type will be chosen as \code{"bca"}.\cr
#' 
#' This implementation is comparably fast, as the expensive sums are coded in C.
#' 
#' @name kurtX
#' @aliases kurtX
#' 
#' @inheritParams ConfidenceIntervals
#' @param x a numeric vector. An object which is not a vector is coerced (if
#' possible) by \code{as.vector}.
#' @param estimator integer, either 1, 2 or 3 (default) defining the algorithm
#' used for calculation. See Details.
#' @param weights a numerical vector of weights the same length as \code{x}
#' giving the weights to use for elements of \code{x}.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. Defaults to \code{FALSE}.
#' @param \dots the dots are passed to the function \code{\link[boot]{boot}},
#' when confidence intervalls are calculated.
#' 
#' @return
#' If \code{conf.level = NA}:
#' numeric kurtosis estimate.
#'
#' Otherwise:
#' named numeric vector with:
#' \itemize{
#'   \item \code{est}: kurtosis estimate
#'   \item \code{lci}: lower confidence limit
#'   \item \code{uci}: upper confidence limit
#' }
#' 
#' @seealso \code{\link{skewX}}, \code{\link{meanX}}, \code{\link{sdX}}, similar code in
#' \code{library(e1071)}
#' @references Cramer, D. (1997): \emph{Basic Statistics for Social Research}
#' Routledge.
#' 
#' Joanes, D. N., Gill, C. A. (1998): Comparing measures of sample skewness and
#' kurtosis. \emph{The Statistician}, 47, 183-189.
#' 
#' @examples
#' 
#' kurtX(d.pizza$price, na.rm=TRUE)
#' 
#' # use sapply to calculate skewness for a data.frame
#' sapply(d.pizza[,c("temperature","price","delivery_min")], kurtX, na.rm=TRUE)
#' 
#' # or apply to do that columnwise with a matrix
#' apply(as.matrix(d.pizza[,c("temperature","price","delivery_min")]), 2, 
#'       kurtX, na.rm=TRUE)
#' 


#' @family shape
#' @concept descriptive-statistics
#' @concept robust-statistics
#'
#'
#' @export
kurtX <- function(x,
                  conf.level = NA,
                  sides = c("two.sided", "left", "right"),
                  method = c("boot", "classic"),
                  estimator = 3,
                  weights = NULL,
                  na.rm = FALSE,
                  ...) {
  
  if (!is.numeric(x))
    stop("'x' must be numeric")
  
  if (!is.null(weights) &&
      length(weights) != length(x))
    stop("'weights' must have same length as 'x'")
  
  if (!estimator %in% c(1, 2, 3))
    stop("'estimator' must be one of 1, 2, or 3")
  
  if (is.na(conf.level)) {
    
    res <- .i.kurt(
      x,
      weights = weights,
      na.rm = na.rm,
      estimator = estimator
    )["est"]
    
  } else {
    
    sides <- match.arg(
      sides,
      choices = c("two.sided", "left", "right"),
      several.ok = FALSE
    )
    
    if (sides != "two.sided")
      conf.level <- 1 - 2 * (1 - conf.level)
    
    method <- match.arg(method)
    
    res <- switch(
      
      method,
      
      classic = .kurtX.classic(
        x,
        conf.level = conf.level,
        estimator = estimator,
        weights = weights,
        na.rm = na.rm
      ),
      
      boot = .kurtX.boot(
        x,
        conf.level = conf.level,
        estimator = estimator,
        weights = weights,
        na.rm = na.rm,
        ...
      )
    )
    
    if (sides == "left") {
      
      res[3] <- Inf
      
    } else if (sides == "right") {
      
      res[2] <- -Inf
    }
  }
  
  res
}


# == internal helper functions ================================================



.i.kurt <- function(x,
                    weights = NULL,
                    na.rm = FALSE,
                    estimator = 3) {
  
  # estimator 1: older textbooks
  
  if (!is.null(weights)) {
    
    # use a standard treatment for weights
    z <- .normWeights(
      x,
      weights,
      na.rm = na.rm
    )
    
    r.kurt <- kurt_weighted_cpp(
      as.numeric(z$x),
      as.numeric(meanX(
        z$x,
        weights = z$weights
      )),
      as.numeric(z$weights)
    )
    
    n <- z$wsum
    
  } else {
    
    if (na.rm)
      x <- na.omit(x)
    
    r.kurt <- kurt_cpp(
      as.numeric(x),
      as.numeric(mean(x))
    )
    
    n <- length(x)
  }
  
  se <- sqrt(
    (24 * n * (n - 2) * (n - 3)) /
      ((n + 1)^2 * (n + 3) * (n + 5))
  )
  
  # se <- sqrt(
  #   (24 * n * (n - 1)^2) /
  #   ((n - 3) * (n - 2) * (n + 3) * (n + 5))
  # )
  
  if (estimator == 2) {
    
    # estimator 2: SAS/SPSS
    
    r.kurt <- (
      ((r.kurt + 3) * (n + 1) / (n - 1) - 3) *
        (n - 1)^2 / (n - 2) / (n - 3)
    )
    
    se <- se * (
      ((n - 1) * (n + 1)) /
        ((n - 2) * (n - 3))
    )
    
  } else if (estimator == 3) {
    
    # estimator 3: MINITAB/BDMP
    
    r.kurt <- (r.kurt + 3) * (1 - 1 / n)^2 - 3
    
    se <- se * ((n - 1) / n)^2
  }
  
  c(
    est = r.kurt,
    var = se^2
  )
}


.kurtX.classic <- function(x,
                           conf.level,
                           estimator = 3,
                           weights = NULL,
                           na.rm = FALSE) {
  
  res <- .i.kurt(
    x,
    weights = weights,
    na.rm = na.rm,
    estimator = estimator
  )
  
  c(
    est = res["est"],
    lci = qnorm((1 - conf.level) / 2) * sqrt(res["var"]),
    uci = qnorm(1 - (1 - conf.level) / 2) * sqrt(res["var"])
  )
}


.kurtX.boot <- function(x,
                        conf.level,
                        estimator = 3,
                        weights = NULL,
                        na.rm = FALSE,
                        ...) {
  
  # Problematic standard errors and confidence intervals
  # for skewness and kurtosis.
  #
  # Wright DB, Herrington JA. (2011)
  # recommend only bootstrap intervals.
  #
  # adjusted bootstrap percentile (BCa) interval
  
  args <- .extractBootArgs(list(...))
  
  boot.fun <- boot::boot(
    
    x,
    
    function(x, d)
      .i.kurt(
        x[d],
        weights = weights,
        estimator = estimator
      ),
    
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
    
    c(
      est = unname(boot.fun$t0[1]),
      lci = ci[[4]][2],
      uci = ci[[4]][3]
    )
    
  } else {
    
    c(
      est = unname(boot.fun$t0[1]),
      lci = ci[[4]][4],
      uci = ci[[4]][5]
    )
  }
}




