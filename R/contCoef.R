
#' Pearson's Contingency Coefficient
#'
#' Calculates Pearson's contingency coefficient for a table \code{x}. If
#' \code{x} and \code{y} are given, the corresponding table is built
#' first (more details in \link{Association}).
#' The contingency coefficient goes from 0 to
#' \eqn{\sqrt(\frac{min(r, c) - 1}{min(r, c)})}. Sakoda (1977) proposed the
#' corrected contingency coefficient with a range between 0 and 1.
#'
#' @inheritParams Association
#' @param method character string specifying the confidence interval method
#' for the contingency coefficient; currently only \code{"boot"} is implemented
#'
#' @param correct logical; whether Sakoda's adjusted Pearson's C should be
#' returned; defaults to \code{FALSE}
#'
#' @return if \code{conf.level = NA}, a numeric scalar containing Pearson's
#' contingency coefficient; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the contingency coefficient.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#'
#' @details
#' Two arguments of the bootstrap are read out of \code{\dots}:
#' \code{R}, the number of replicates (default 5000), and \code{type},
#' the interval type, either \code{"perc"} (default) or \code{"bca"}.
#'
#' For Pearson's contingency coefficient
#' no generally accepted analytical confidence intervals exist. If interval
#' estimation is required, resampling methods such as the bootstrap may be
#' applied. In applied research, effect size measures with better inferential
#' properties (e.g. Cramer's V) are usually preferred.
#'
#' \code{sides} names the side on which the finite bound lies:
#' \code{"left"} yields an interval bounded below and \code{"right"} one
#' bounded above. The coefficient is bounded, so the open side is reported
#' at the range boundary rather than as \eqn{\pm\infty} (design_rules.md
#' 4.1): 0 below, and above either 1 (with \code{correct = TRUE}) or
#' \eqn{\sqrt{(m-1)/m}} with \eqn{m = \min(r, c)}.
#'
#' @section Random number generation:
#' \code{conf.level} triggers a bootstrap, which draws a seed from R's
#' global random number generator and therefore advances it. Call
#' \code{\link[base]{set.seed}} beforehand for reproducible intervals.
#'
#' @references
#' Sakoda, J.M. (1977) Measures of Association for Multivariate Contingency
#' Tables, \emph{Proceedings of the Social Statistics Section of the American
#' Statistical Association} (Part III), 777-780.
#'
#' @seealso \code{\link[bedrock]{pairApply}}, \code{\link{cramerV}}
#'
#' @examples
#'
#' tab <- apply(HairEyeColor, c(1, 2), sum)
#' contCoef(tab)
#'
#' # just x and y
#' with(bedrock::untable(tab), contCoef(Hair, Eye))
#'
#' set.seed(1)
#' contCoef(tab, conf.level = 0.95)
#'
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @concept chi-square-based
#' @export
contCoef <- function(x, y = NULL,
                     correct = FALSE,
                     conf.level = NA,
                     sides = c("two.sided", "left", "right"),
                     method = c("boot"),
                     ...) {

  sides  <- match.arg(sides)
  method <- match.arg(method)

  # ------------------------------------------
  # Extract bootstrap arguments from ...
  # R and type are described in @details, not with @param: they are not
  # formals, and a @param for an argument that never reaches \usage makes
  # R CMD check report "documented arguments not in \usage".
  # ------------------------------------------
  dots <- list(...)

  R    <- dots$R    %||% 5000
  type <- dots$type %||% "perc"

  type <- match.arg(type, c("perc", "bca"))

  if (!is.numeric(R) || length(R) != 1L || !is.finite(R) || R <= 0 ||
      R %% 1 != 0)
    stop("'R' must be a positive whole number.")

  # ------------------------------------------
  # Normalize input
  # ------------------------------------------
  # only the genuine table() arguments are forwarded; R and type belong to
  # the bootstrap and would be rejected by table()
  tabArgs <- dots[setdiff(names(dots), c("R", "type"))]
  tab <- do.call(normalizeToConfusion,
                 c(list(x, y, mode = "association"), tabArgs))

  # ------------------------------------------
  # Point estimate (C++)
  # ------------------------------------------
  cc <- contcoef_table_cpp(tab, correct)

  if (is.na(conf.level))
    return(cc)

  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("'conf.level' must be a single number in (0, 1), or NA")

  # the attainable range of C - used to close the open side of a one-sided
  # interval and to clamp the two-sided one
  mn   <- min(dim(tab))
  cMax <- if (correct) 1 else sqrt((mn - 1) / mn)

  # ------------------------------------------
  # Deterministic base seed from R RNG
  # ------------------------------------------
  base_seed <- as.integer(sample.int(.Machine$integer.max, 1))

  # A one-sided interval carries the full alpha on its single finite side,
  # so the two-sided machinery runs at a doubled alpha and the irrelevant
  # bound is closed at the range boundary afterwards.
  confAdj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  alpha   <- 1 - confAdj

  # ------------------------------------------
  # Percentile bootstrap
  # ------------------------------------------
  if (type == "perc") {

    boot_vals <- contcoef_table_boot_cpp(
      tab     = tab,
      R       = as.integer(R),
      seed    = base_seed,
      correct = correct
    )

    ci <- quantile(boot_vals, probs = c(alpha / 2, 1 - alpha / 2),
                   names = FALSE)
  }

  # ------------------------------------------
  # BCa bootstrap
  # ------------------------------------------
  if (type == "bca") {

    # confAdj, not conf.level: 'sides' was ignored entirely on this branch
    res <- contcoef_table_boot_bca_cpp(
      tab,
      as.integer(R),
      base_seed,
      correct,
      confAdj
    )

    cc <- res$estimate
    ci <- c(res$conf.low, res$conf.high)
  }

  ci <- c(max(ci[1L], 0), min(ci[2L], cMax))

  # sides names the side carrying the FINITE bound. The former version had
  # this the wrong way round: "left" asked for probs c(0, conf.level),
  # i.e. an interval open at the bottom and bounded above.
  if (sides == "left")  ci[2L] <- cMax
  if (sides == "right") ci[1L] <- 0

  return(setNamesX(c(cc, ci), names = c("est", "lci", "uci")))
}
