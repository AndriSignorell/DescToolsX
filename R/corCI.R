#' Confidence Interval for a Pearson Correlation
#'
#' Computes confidence intervals for a Pearson correlation coefficient
#' using Fisher's \eqn{z}-transformation. Since the sampling distribution
#' of the correlation coefficient is not normally distributed, Fisher's
#' transformation is applied to obtain approximately normally distributed
#' values, from which confidence intervals can be derived.
#'
#' @param rho Numeric. Pearson correlation coefficient. Must be a single
#'   value in the interval \eqn{[-1, 1]}.
#' @param n Integer. Sample size used to estimate the correlation.
#'   Must be at least 3.
#' @param conf.level Numeric. Confidence level for the interval.
#'   Must be a single value in the open interval \eqn{(0, 1)}.
#'   Default is \code{0.95}.
#' @param alternative Character string specifying the alternative hypothesis.
#'   Must be one of \code{"two.sided"} (default), \code{"less"}, or
#'   \code{"greater"}. Partial matching is allowed.
#'
#' @details
#' Fisher's \eqn{z}-transformation is defined as
#' \deqn{z = \frac{1}{2} \log\left(\frac{1 + r}{1 - r}\right),}
#' which stabilizes the variance of the correlation coefficient. The
#' transformed values are approximately normally distributed with standard
#' error \eqn{1 / \sqrt{n - 3}}. Confidence intervals are computed on the
#' transformed scale and then back-transformed.
#'
#' For \code{alternative = "two.sided"}, a symmetric interval on the
#' \eqn{z}-scale is used. For one-sided alternatives, the interval is
#' unbounded on one side.
#'
#' @return A named numeric vector with elements:
#' \describe{
#'   \item{cor}{The input correlation coefficient \code{rho}.}
#'   \item{lwr.ci}{Lower bound of the confidence interval.}
#'   \item{upr.ci}{Upper bound of the confidence interval.}
#' }
#'
#' @author William Revelle \email{revelle@@northwestern.edu},
#'   modified by Andri Signorell \email{andri@@signorell.net}
#'
#' @seealso \code{\link{fisherZ}}, \code{\link{fisherZInv}}, \code{\link{cor.test}}
#'
#' @examples
#' # Confidence interval for a single correlation
#' corCI(0.5, n = 30)
#'
#' # Compare multiple correlations
#' r <- seq(0, 0.9, by = 0.1)
#' t(sapply(r, corCI, n = 30))
#'


#' @keywords univar
#' @export
corCI <- function(rho, n, conf.level = 0.95,
                  alternative = c("two.sided","less","greater")) {
  
  if (!is.numeric(rho) || length(rho) != 1L || !is.finite(rho))
    stop("'rho' must be a single finite numeric value")
  
  if (abs(rho) > 1)
    stop("'rho' must be between -1 and 1")
  
  if (n < 3L)
    stop("not enough finite observations")
  
  if (length(conf.level) != 1 || !is.finite(conf.level) ||
      conf.level <= 0 || conf.level >= 1)
    stop("'conf.level' must be in (0,1)")
  
  alternative <- match.arg(alternative)
  
  # handle perfect correlation
  if (abs(rho) >= 1) {
    return(c(cor = rho, lwr.ci = rho, upr.ci = rho))
  }
  
  # numerical stability
  rho <- pmin(pmax(rho, -0.9999999), 0.9999999)
  
  z <- fisherZ(rho)
  sigma <- 1 / sqrt(n - 3)
  
  ci <- switch(alternative,
               less = c(-Inf, z + sigma * qnorm(conf.level)),
               greater = c(z - sigma * qnorm(conf.level), Inf),
               two.sided = z + c(-1, 1) * sigma * qnorm((1 + conf.level)/2)
  )
  
  ci <- fisherZInv(ci)
  
  setNames(c(rho, ci), c("cor", "lwr.ci", "upr.ci"))
}

