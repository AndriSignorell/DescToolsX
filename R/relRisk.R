
#' Relative Risk
#'
#' Computes the relative risk for a 2x2 contingency table together with
#' optional confidence intervals.
#'
#' The relative risk compares the event probability in the exposed group with
#' the event probability in the unexposed group.
#'
#' The function expects the exposure groups in the rows and the outcome in the
#' columns:
#'
#' \preformatted{
#'                outcome = 1   outcome = 0
#' exposed = 1        n00           n01
#' exposed = 0        n10           n11
#' }
#'
#' The relative risk is defined as:
#'
#' \deqn{
#' RR =
#' \frac{n_{00} / (n_{00} + n_{01})}
#'      {n_{10} / (n_{10} + n_{11})}
#' }
#'
#' Confidence intervals can be calculated using the score method of Koopman
#' (1984), a Wald approximation, or via transformation of the odds ratio.
#'
#' @param x a numeric 2x2 matrix or table containing non-negative counts
#' @param y an optional vector. If supplied, \code{table(x, y, ...)} is
#'   computed.
#' @param method character string specifying the confidence interval method.
#'   One of \code{"score"}, \code{"wald"}, or \code{"use-or"}.
#' @param delta small continuity correction added to the event counts for the
#'   Wald interval. Only used if \code{method = "wald"}.
#' @param conf.level confidence level for the interval estimate. If
#'   \code{NA} (default), only the point estimate is returned.
#' @param \dots further arguments passed to \code{\link{table}}
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{relative risk estimate}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @details
#' The score interval is based on the method of Koopman (1984) and
#' Miettinen and Nurminen (1985). The Wald interval is asymptotic and may
#' perform poorly for small counts or extreme probabilities.
#'
#' If the table orientation differs from the required structure, rows or
#' columns can be reversed using \code{\link[bedrock]{revX}} or transposed
#' with \code{\link{t}}.
#'
#' @references
#' Koopman, P. A. R. (1984). Confidence intervals for the ratio of two
#' binomial proportions. \emph{Biometrics}, \emph{40}(2), 513--517.
#'
#' Miettinen, O., & Nurminen, M. (1985). Comparative analysis of two rates.
#' \emph{Statistics in Medicine}, \emph{4}(2), 213--226.
#'
#' Rothman, K. J., Greenland, S., & Lash, T. L. (2008).
#' \emph{Modern Epidemiology} (3rd ed.). Lippincott Williams & Wilkins.
#'
#' @examples
#' m <- matrix(
#'   c(78, 50,
#'     1422, 950),
#'   nrow = 2,
#'   dimnames = list(
#'     water = c("cont", "clean"),
#'     diarrhea = c("yes", "no")
#'   )
#' )
#'
#' relRisk(m, conf.level = 0.95)
#'
#'
#' mm <- cbind(c(9, 20), c(41, 29))
#'
#' relRisk(t(mm), conf.level = 0.95)
#'
#' relRisk(
#'   t(mm),
#'   conf.level = 0.95,
#'   method = "wald"
#' )
#'
#' relRisk(
#'   t(mm),
#'   conf.level = 0.95,
#'   method = "use-or"
#' )
#'
#'
#' @family effect.size  
#' @concept effect-size  
#' @concept binary-outcome
#'
#'
#' @export
relRisk <- function(
    x,
    y = NULL,
    method = c("score", "wald", "use-or"),
    delta = 0.5,
    conf.level = NA,
    ...
) {
  
  if (!is.null(y))
    x <- table(x, y, ...)
  
  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")
  
  if (anyNA(x))
    stop("Argument 'x' must not contain missing values.")
  
  if (length(dim(x)) != 2L)
    stop("Argument 'x' must be a matrix.")
  
  if (!all(dim(x) == c(2L, 2L)))
    stop("Argument 'x' must be a 2x2 matrix.")
  
  if (any(x < 0))
    stop("Argument 'x' must contain non-negative counts.")
  
  if (any(x %% 1 != 0))
    stop("Argument 'x' must contain integer counts.")
  
  if (any(rowSums(x) == 0))
    stop("Rows of 'x' must contain positive totals.")
  
  method <- match.arg(method)
  
  x1 <- x[1L, 1L]
  x2 <- x[2L, 1L]
  
  n1 <- sum(x[1L, ])
  n2 <- sum(x[2L, ])
  
  estimate <- (x1 / n1) / (x2 / n2)
  
  if (is.na(conf.level))
    return(estimate)
  
  ci <- switch(
    method,
    
    "score" = .relRiskScore(
      x1 = x1,
      x2 = x2,
      n1 = n1,
      n2 = n2,
      conf.level = conf.level
    ),
    
    "wald" = .relRiskWald(
      estimate = estimate,
      x1 = x1,
      x2 = x2,
      n1 = n1,
      n2 = n2,
      delta = delta,
      conf.level = conf.level
    ),
    
    "use-or" = .relRiskUseOr(
      x = x,
      x2 = x2,
      n2 = n2,
      conf.level = conf.level
    )
  )
  
  c(
    est = estimate,
    lci = ci[["lci"]],
    uci = ci[["uci"]]
  )
  
}



# == internal helper functions ================================================

.relRiskScore <- function(
    x1,
    x2,
    n1,
    n2,
    conf.level
) {
  
  z <- abs(qnorm((1 - conf.level) / 2))
  
  if (x1 == 0 && x2 == 0) {
    return(c(lci = 0, uci = Inf))
  }
  
  roots <- .relRiskScoreRoots(
    x1 = x1,
    x2 = x2,
    n1 = n1,
    n2 = n2,
    z = z
  )
  
  p0low <- roots[["p0low"]]
  p0up  <- roots[["p0up"]]
  
  if (x2 == 0 && x1 != 0) {
    
    lci <- .relRiskScoreBound(
      p0 = p0low,
      x1 = x1,
      x2 = x2,
      n1 = n1,
      n2 = n2
    )
    
    return(c(lci = lci, uci = Inf))
    
  }
  
  if (x2 != n2 && x1 == 0) {
    
    uci <- .relRiskScoreBound(
      p0 = p0up,
      x1 = x1,
      x2 = x2,
      n1 = n1,
      n2 = n2
    )
    
    return(c(lci = 0, uci = uci))
    
  }
  
  if (x2 == n2 && x1 == n1) {
    
    return(c(
      lci = n1 / (n1 + z^2),
      uci = (n2 + z^2) / n2
    ))
    
  }
  
  lci <- .relRiskScoreBound(
    p0 = p0low,
    x1 = x1,
    x2 = x2,
    n1 = n1,
    n2 = n2
  )
  
  uci <- .relRiskScoreBound(
    p0 = p0up,
    x1 = x1,
    x2 = x2,
    n1 = n1,
    n2 = n2
  )
  
  c(lci = lci, uci = uci)
  
}



.relRiskScoreRoots <- function(
    x1,
    x2,
    n1,
    n2,
    z
) {
  
  a1 <- n2 * (
    n2 * (n2 + n1) * x1 +
      n1 * (n2 + x1) * z^2
  )
  
  a2 <- -n2 * (
    n2 * n1 * (x2 + x1) +
      2 * (n2 + n1) * x2 * x1 +
      n1 * (n2 + x2 + 2 * x1) * z^2
  )
  
  a3 <- (
    2 * n2 * n1 * x2 * (x2 + x1) +
      (n2 + n1) * x2^2 * x1 +
      n2 * n1 * (x2 + x1) * z^2
  )
  
  a4 <- -n1 * x2^2 * (x2 + x1)
  
  b1 <- a2 / a1
  b2 <- a3 / a1
  b3 <- a4 / a1
  
  c1 <- b2 - b1^2 / 3
  c2 <- b3 - b1 * b2 / 3 + 2 * b1^3 / 27
  
  acosArg <- sqrt(27) * c2 / (2 * c1 * sqrt(-c1))
  acosArg <- min(1, max(-1, acosArg))
  
  ceta <- acos(acosArg)
  
  t1 <- -2 * sqrt(-c1 / 3) * cos(pi / 3 - ceta / 3)
  t2 <- -2 * sqrt(-c1 / 3) * cos(pi / 3 + ceta / 3)
  t3 <-  2 * sqrt(-c1 / 3) * cos(ceta / 3)
  
  p01 <- t1 - b1 / 3
  p02 <- t2 - b1 / 3
  p03 <- t3 - b1 / 3
  
  p0up <- min(p01, p02, p03)
  
  p0sum <- p01 + p02 + p03
  
  p0low <- p0sum - p0up - max(p01, p02, p03)
  
  list(
    p0low = p0low,
    p0up  = p0up
  )
  
}



.relRiskScoreBound <- function(
    p0,
    x1,
    x2,
    n1,
    n2
) {
  
  (
    1 -
      (n1 - x1) * (1 - p0) /
      (x2 + n1 - (n1 + n2) * p0)
  ) / p0
  
}



.relRiskWald <- function(
    estimate,
    x1,
    x2,
    n1,
    n2,
    delta,
    conf.level
) {
  
  x1d <- x1 + delta
  x2d <- x2 + delta
  
  logEstimate <- log(estimate)
  
  seLogEstimate <- sqrt(
    1 / x1d - 1 / n1 +
      1 / x2d - 1 / n2
  )
  
  z <- abs(qnorm((1 - conf.level) / 2))
  
  c(
    lci = exp(logEstimate - z * seLogEstimate),
    uci = exp(logEstimate + z * seLogEstimate)
  )
  
}



.relRiskUseOr <- function(
    x,
    x2,
    n2,
    conf.level
) {
  
  or <- oddsRatio(
    x,
    conf.level = conf.level
  )
  
  p2 <- x2 / n2
  
  rrCi <- or / ((1 - p2) + p2 * or)
  
  c(
    lci = unname(rrCi[2L]),
    uci = unname(rrCi[3L])
  )
  
}

