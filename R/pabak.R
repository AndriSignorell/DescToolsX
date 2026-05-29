
#' Prevalence-Adjusted and Bias-Adjusted Kappa (PABAK)
#'
#' Computes the prevalence-adjusted and bias-adjusted kappa (PABAK) as a
#' measure of inter-rater agreement, together with an asymptotic confidence
#' interval and the auxiliary prevalence and bias indices.
#'
#' @details
#' Cohen's kappa can be paradoxically low when the prevalence of one category
#' differs markedly from 0.5 (prevalence effect) or when the two raters
#' systematically disagree on the marginal frequency of a category (bias
#' effect).  PABAK corrects for both distortions by replacing the chance
#' agreement term with the fixed value \eqn{p_e = 0.5}.  The statistic
#' simplifies to
#' \deqn{\mathrm{PABAK} = 2 p_o - 1}
#' where \eqn{p_o} is the observed proportion of agreement (diagonal sum of
#' the relative-frequency table).
#'
#' The asymptotic standard error is derived from the delta method applied to
#' the above identity:
#' \deqn{\mathrm{SE} = 2 \sqrt{p_o (1 - p_o) / n}}
#'
#' Two diagnostic indices quantify the prevalence and bias effects that can
#' cause Cohen's kappa to differ from PABAK.  They are defined only for
#' \eqn{2 \times 2} tables following Byrt et al. (1993); for larger tables
#' \code{pi} and \code{bi} are \code{NA}:
#' \describe{
#'   \item{Prevalence index (PI)}{
#'     \eqn{|p_{11} - p_{22}|}.  A high PI indicates that one outcome
#'     dominates, which deflates Cohen's kappa relative to PABAK.}
#'   \item{Bias index (BI)}{
#'     \eqn{|p_{1\cdot} - p_{\cdot 1}|}.  A high BI signals that the two
#'     raters assign the positive label at systematically different rates.}
#' }
#' When both PI and BI are near zero, PABAK and Cohen's kappa will be nearly
#' identical.
#'
#' The Wald confidence interval is truncated to the admissible range
#' \eqn{[-1, 1]}.
#'
#' Data can be passed either as a square confusion matrix (or data frame) in
#' \code{x}, or as two vectors \code{x} and \code{y}, in which case
#' \code{table(x, y, \dots)} is computed internally.
#'
#' Missing values are handled as \code{\link{table}} does — excluded by
#' default.  Pass \code{useNA = "ifany"} via \code{...} to include them.
#'
#' @param x        A square confusion matrix (or data frame), or a
#'   categorical vector when \code{y} is provided.
#' @param y        \code{NULL} (default) or a categorical vector of the same
#'   length as \code{x}.  When supplied, \code{table(x, y, \dots)} is
#'   computed internally.
#' @param conf.level Confidence level of the interval.  A single numeric
#'   value in \eqn{(0, 1)}, or \code{NA} (default) to return only the
#'   point estimate.
#' @param sides    A character string specifying the side of the interval:
#'   \code{"two.sided"} (default), \code{"left"}, or \code{"right"}.
#'   Partial matching is supported.  \code{"left"} sets \code{uci = Inf};
#'   \code{"right"} sets \code{lci = -Inf}.  Ignored when
#'   \code{conf.level = NA}.
#' @param ...      Further arguments passed to \code{\link{table}} (vector
#'   interface only), e.g. \code{useNA}.
#'
#' @return
#' If \code{conf.level = NA}, only the PABAK estimate is returned as a single
#' numeric value.
#'
#' Otherwise, a named numeric vector is returned containing:
#' \describe{
#'   \item{\code{est}}{PABAK estimate.}
#'   \item{\code{lci}}{Lower confidence bound.}
#'   \item{\code{uci}}{Upper confidence bound.}
#'   \item{\code{pi}}{Prevalence index (\code{NA} for \eqn{k > 2}).}
#'   \item{\code{bi}}{Bias index (\code{NA} for \eqn{k > 2}).}
#' }
#'
#' @references
#' Byrt, T., Bishop, J., & Carlin, J. B. (1993). Bias, prevalence and kappa.
#'   \emph{Journal of Clinical Epidemiology}, \emph{46}(5), 423--429.
#'   \doi{10.1016/0895-4356(93)90018-V}
#'
#' Hoehler, F. K. (2000). Bias and prevalence effects on kappa viewed in
#'   terms of sensitivity and specificity.
#'   \emph{Journal of Clinical Epidemiology}, \emph{53}(5), 499--503.
#'   \doi{10.1016/S0895-4356(99)00174-2}
#'
#' @seealso \code{\link{cohenKappa}}
#'
#' @family topic.agreement
#' @concept agreement
#' @concept inter-rater-reliability
#' @concept association-measures
#' @concept prevalence-adjustment
#'
#' @examples
#' # 2x2 table: two raters classifying 100 patients as positive / negative
#' m2 <- matrix(c(45, 15, 5, 35), nrow = 2, byrow = TRUE,
#'              dimnames = list(rater1 = c("pos", "neg"),
#'                              rater2 = c("pos", "neg")))
#'
#' pabak(m2)
#' pabak(m2, conf.level = 0.95)
#'
#' # Compare with cohenKappa: PI/BI illustrate where the gap comes from
#' cohenKappa(m2)
#'
#' # 4x4 table (from LehmacherTest example): pi and bi are NA for k > 2
#' m4 <- matrix(c(400, 40, 20, 10,
#'                 50,300, 60, 20,
#'                 10, 40,120,  5,
#'                  5, 90, 50, 80),
#'              nrow = 4, byrow = TRUE)
#'
#' pabak(m4, conf.level = 0.95)
#'
#' # vector interface
#' x <- bedrock::untable(m2)
#' pabak(x$rater1, x$rater2, conf.level = 0.95)
#'

#' @export
pabak <- function(x,
                  y          = NULL,
                  conf.level = NA,
                  sides      = c("two.sided", "left", "right"),
                  ...) {
  
  # --- conf.level: scalar check (type and range validated below) --------
  if (length(conf.level) != 1L)
    stop("Argument 'conf.level' must be a single value or NA.")
  
  # --- build / validate confusion matrix --------------------------------
  x <- normalizeToConfusion(x = x, y = y, ...)
  
  # --- guard: empty table -----------------------------------------------
  n <- sum(x)
  if (n == 0)
    stop("Confusion matrix is empty (all cells zero).")
  
  # --- relative frequencies ---------------------------------------------
  p <- x / n
  
  # --- PABAK point estimate ---------------------------------------------
  # po: observed agreement (diagonal sum)
  # PABAK = 2*po - 1  (fixes pe = 0.5)
  po  <- sum(diag(p))
  est <- 2 * po - 1
  
  if (is.na(conf.level))
    return(est)
  
  # --- diagnostic indices -----------------------------------------------
  # PI and BI are defined by Byrt et al. (1993) for 2x2 tables only.
  # No generally accepted extension to k > 2 exists; return NA in that case.
  if (ncol(x) == 2L) {
    rowFreqs <- rowSums(p)
    colFreqs <- colSums(p)
    pi_val   <- abs(p[1L, 1L] - p[2L, 2L])
    bi_val   <- abs(rowFreqs[1L] - colFreqs[1L])
  } else {
    pi_val <- NA_real_
    bi_val <- NA_real_
  }
  
  # --- asymptotic SE (delta method on PABAK = 2*po - 1) ----------------
  # Var(po) ≈ po*(1-po)/n  (binomial approximation for diagonal sum)
  # Var(PABAK) = 4 * Var(po)  →  SE = 2*sqrt(po*(1-po)/n)
  se <- 2 * sqrt(po * (1 - po) / n)
  
  # --- CI ---------------------------------------------------------------
  if (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")
  
  sides <- match.arg(sides)
  
  conf_adj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  alpha    <- 1 - conf_adj
  z        <- qnorm(1 - alpha / 2)
  
  # Truncate to [-1, 1]: the Wald approximation can produce bounds outside
  # the admissible range, most commonly uci > 1 near perfect agreement.
  lci <- max(-1, est - z * se)
  uci <- min( 1, est + z * se)
  
  if (sides == "left")  uci <- Inf
  if (sides == "right") lci <- -Inf
  
  c(est = est, lci = lci, uci = uci, pi = pi_val, bi = bi_val)
}
