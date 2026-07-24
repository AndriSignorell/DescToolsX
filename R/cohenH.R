
#' Cohen's h for a 2x2 Table
#'
#' Computes Cohen's \eqn{h}, a standardized effect size for the
#' difference between two proportions in a 2x2 contingency table.
#'
#' Cohen's \eqn{h} is defined as:
#'
#' \deqn{
#' h =
#' 2\arcsin(\sqrt{p_1})
#' -
#' 2\arcsin(\sqrt{p_2})
#' }
#'
#' where \eqn{p_1} and \eqn{p_2} are the event probabilities
#' in the first and second row, respectively.
#'
#' Optionally, an approximate asymptotic confidence interval
#' is computed.
#'
#' @param x a 2x2 contingency table, matrix, or vector that can
#'   be coerced into a table
#' @param y an optional second variable used together with \code{x}
#'   to create a contingency table via \code{table(x, y, ...)}
#' @param conf.level confidence level for the interval.
#'   If \code{NA}, only the point estimate is returned.
#' @param ... additional arguments passed to \code{table()}
#'
#' @return if \code{conf.level = NA}, a numeric scalar containing Cohen's
#' \eqn{h}; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Cohen's \eqn{h}.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#'
#' @details
#' Cohen's \eqn{h} is a variance-stabilized standardized effect
#' size for comparing two proportions.
#'
#' Approximate interpretation thresholds suggested by Cohen are:
#'
#' \tabular{ll}{
#' |h| < 0.2 \tab negligible effect \cr
#' |h| >= 0.2 \tab small effect \cr
#' |h| >= 0.5 \tab medium effect \cr
#' |h| >= 0.8 \tab large effect
#' }
#'
#' The confidence interval is based on the asymptotic standard error:
#'
#' \deqn{
#' SE(h) =
#' \sqrt{
#' \frac{1}{n_1}
#' +
#' \frac{1}{n_2}
#' }
#' }
#'
#' @examples
#' tab <- matrix(
#'   c(26, 26,
#'     6, 7),
#'   nrow = 2,
#'   byrow = TRUE
#' )
#'
#' cohenH(tab)
#'
#' x <- c(rep("A", 52), rep("B", 13))
#' y <- c(rep(c("yes", "no"), c(26,26)),
#'        rep(c("yes", "no"), c(6,7)))
#'
#' cohenH(x, y)
#'
#' @references
#' Cohen J (1988). Statistical Power Analysis for the Behavioral
#' Sciences (2nd ed.). Lawrence Erlbaum Associates.
#'



#' @family effect.size  
#' @concept effect-size  
#' @concept binary-outcome
#'
#'
#' @export
cohenH <- function(x,
                    y = NULL,
                    conf.level = 0.95,
                    ...) {
  
  if (!is.null(y))
    x <- table(x, y, ...)
  
  if (!all(dim(x) == c(2,2)))
    stop("Input must be a 2x2 table.")
  
  a <- x[1,1]
  b <- x[1,2]
  c <- x[2,1]
  d <- x[2,2]
  
  n1 <- a + b
  n2 <- c + d
  
  p1 <- a / n1
  p2 <- c / n2
  
  h <- 2 * asin(sqrt(p1)) -
    2 * asin(sqrt(p2))
  
  if (is.na(conf.level))
    return(h)
  
  se <- sqrt(1/n1 + 1/n2)
  
  alpha <- 1 - conf.level
  z <- qnorm(1 - alpha/2)
  
  lci <- h - z * se
  uci <- h + z * se
  
  out <- c(
    est = h,
    lci = lci,
    uci = uci
  )
  
  return(out)
}

