
#' Pearson's Contingency Coefficient
#'
#' Computes Pearson's contingency coefficient for a contingency table. If
#' `x` and `y` are supplied, the table is constructed first; see
#' [Association()].
#'
#' @inheritParams Association
#'
#' @param correct logical; whether Sakoda's adjusted Pearson's C should be
#' returned; defaults to `FALSE`
#'
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#'
#' @param ... further arguments. Named arguments known to
#'   [normalizeToConfusion()] or [table()] are used to
#'   build the table; `R` and `type` configure the bootstrap and
#'   are described under Details. Anything else is an error rather than a
#'   silent no-op.
#'
#' @return if `conf.level = NA`, a numeric scalar containing Pearson's
#' contingency coefficient; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of the contingency coefficient}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' @details
#' Pearson's contingency coefficient ranges from 0 to
#' \eqn{\sqrt{(m-1)/m}}, where \eqn{m = \min(r, c)}. Consequently, its
#' attainable maximum depends on the dimensions of the table.
#'
#' Sakoda's correction divides the coefficient by this maximum:
#' \deqn{C_{\mathrm{S}} = \frac{C}{C_{\max}}
#'       = C\sqrt{\frac{m}{m-1}}.}
#' The corrected coefficient therefore ranges from 0 to 1.
#'
#' Since no generally accepted analytical interval is available,
#' only bootstrap intervals are implemented. The interval is obtained from 
#' a multinomial bootstrap over the cells of
#' the table. Two arguments configure it, both passed through `\dots`:
#' `R`, the number of replicates (default 999), and `type`, one
#' of `"perc"` (default) or `"bca"`.
#'
#' `"perc"` is the default deliberately. Under independence the
#' parameter sits *on* the boundary of its range, where the sampling
#' distribution of \eqn{C} is not normal under any monotone
#' transformation - which is precisely what BCa assumes. Both of its
#' ingredients degrade there: the bias correction is read off the share of
#' replicates below the estimate, which collapses when the estimate is at
#' the edge of the bootstrap distribution, and the acceleration is a
#' jackknife, which is not consistent for a functional that is not smooth.
#' `"bca"` is the better choice for tables with a clearly non-zero
#' association and a reasonable number of observations.
#'
#' Confidence intervals are restricted to the attainable range. Measures
#' such as [cramerV] may be preferable when inference is central.
#' 
#' For further information see [ConfidenceIntervals()].
#'
#' @references
#' Sakoda, J.M. (1977) Measures of Association for Multivariate Contingency
#' Tables, *Proceedings of the Social Statistics Section of the American
#' Statistical Association* (Part III), 777-780.
#'
#' Efron, B., Tibshirani, R.J. (1993) *An Introduction to the
#' Bootstrap*, Chapman & Hall, chapter 14.
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
#' set.seed(1)
#' contCoef(tab, conf.level = 0.95, type = "bca", R = 999)
#'
#'
#' @seealso [bedrock::pairApply]
#'
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @concept chi-square-based
#' @export
contCoef <- function(x, y = NULL,
                     conf.level = NA,
                     sides = c("two.sided", "left", "right"),
                     correct = FALSE,
                     ...) {

  sides  <- match.arg(sides)
  conf.level <- checkConfLevel(conf.level)

  # Refused here as everywhere else in the suite. contCoef could in fact
  # compute it - the probs switch below stays valid below 0.5 - but it was
  # the last function that accepted what cramerV, lambda, tukeyBiweight
  # and the whole assoc family reject, and one answer per question beats
  # a defensible exception.
  if (sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  dots <- list(...)
  nms  <- names(dots)
  if (is.null(nms)) nms <- rep("", length(dots))

  # A misspelt name used to travel on into table() and either die there
  # with a message about the wrong function or, worse, be swallowed.
  known <- unique(c(.bootArgNames,
                    names(formals(normalizeToConfusion)),
                    names(formals(table))))

  bad <- setdiff(nms[nzchar(nms)], known)
  if (length(bad))
    stop(gettextf("unused argument(s) in '...': %s",
                  paste(sQuote(bad, FALSE), collapse = ", ")), domain = NA)

  # logical subsetting, not setdiff() on the names: with a mixture of
  # named and unnamed arguments the latter drops the unnamed ones and
  # produces an NA element
  tabArgs <- dots[!nms %in% .bootArgNames]

  tab <- do.call(normalizeToConfusion,
                 c(list(x, y, mode = "association"), tabArgs))

  cc <- contcoef_table_cpp(tab, correct)

  if (is.na(conf.level))
    return(cc)

  # perc/bca only: the other three boot.ci() types would be accepted by the
  # shared validator, but nothing here computes them. Checked before the
  # degenerate-table exit, so that a bad argument is a bad argument
  # whatever the data happen to be.
  args <- .extractBootArgs(dots,
                           types    = c("perc", "bca"),
                           default  = "perc",
                           parallel = FALSE)

  if (!is.finite(cc))
    return(setNamesX(c(cc, NA_real_, NA_real_),
                     names = c("est", "lci", "uci")))

  # the attainable range of C - used to close the open side of a one-sided
  # interval and to clamp the two-sided one
  mn   <- min(dim(tab))
  cMax <- if (correct) 1 else sqrt((mn - 1) / mn)

  # deterministic base seed drawn from R's RNG, so that set.seed() governs
  # the compiled bootstrap as well
  baseSeed <- sample.int(.Machine$integer.max, 1L)

  # 'sides' names the side carrying the FINITE bound; the open side gets
  # probability 0 or 1 and is closed at the range boundary by
  # applySides() below. This reads more directly than the doubled-alpha
  # construction it replaced, but it computes the same thing - that
  # construction was correct, contrary to what an earlier comment here
  # claimed.
  alpha <- 1 - conf.level

  probs <- switch(sides,
                  two.sided = c(alpha / 2, 1 - alpha / 2),
                  left      = c(alpha, 1),
                  right     = c(0, 1 - alpha))

  bootVals <- contcoef_table_boot_cpp(tab,
                                      R       = as.integer(args$R),
                                      seed    = baseSeed,
                                      correct = correct)

  if (args$type == "bca")
    probs <- .bcaProbs(probs,
                       z0 = .bcaZ0(bootVals, cc),
                       a  = contcoef_jackknife_a_cpp(tab, correct))

  # one quantile convention for both types. The bca branch used to read a
  # raw order statistic at floor(p * R) while perc interpolated through
  # quantile(), so the same replicates gave two different bounds depending
  # on the type - and the unclamped index could read past the end of the
  # vector.
  ci <- stats::quantile(bootVals, probs = probs, names = FALSE)

  # C runs from 0 to sqrt((m-1)/m), or to 1 after Sakoda's correction -
  # the one range in the suite that depends on the data. applySides()
  # clamps to it and closes the open side there.
  c(est = cc, applySides(ci, sides, lo = 0, hi = cMax))
}
