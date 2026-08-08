
#' Pearson's Contingency Coefficient
#'
#' Computes Pearson's contingency coefficient for a contingency table. If
#' \code{x} and \code{y} are supplied, the table is constructed first; see
#' \code{\link{Association}}.
#'
#' @inheritParams Association
#'
#' @param correct logical; whether Sakoda's adjusted Pearson's C should be
#' returned; defaults to \code{FALSE}
#'
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#'
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See details in \code{\link{ConfidenceIntervals}}.
#'
#' @param method character string specifying the interval method (here only
#' \code{"boot"}).
#'   
#'
#' @param ... further arguments. Named arguments known to
#'   \code{\link{normalizeToConfusion}} or \code{\link{table}} are used to
#'   build the table; \code{R} and \code{type} configure the bootstrap and
#'   are described under Details. Anything else is an error rather than a
#'   silent no-op.
#'
#' @return if \code{conf.level = NA}, a numeric scalar containing Pearson's
#' contingency coefficient; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the contingency coefficient}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
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
#' the table. Two arguments configure it, both passed through \code{\dots}:
#' \code{R}, the number of replicates (default 999), and \code{type}, one
#' of \code{"perc"} (default) or \code{"bca"}.
#'
#' \code{"perc"} is the default deliberately. Under independence the
#' parameter sits \emph{on} the boundary of its range, where the sampling
#' distribution of \eqn{C} is not normal under any monotone
#' transformation - which is precisely what BCa assumes. Both of its
#' ingredients degrade there: the bias correction is read off the share of
#' replicates below the estimate, which collapses when the estimate is at
#' the edge of the bootstrap distribution, and the acceleration is a
#' jackknife, which is not consistent for a functional that is not smooth.
#' \code{"bca"} is the better choice for tables with a clearly non-zero
#' association and a reasonable number of observations.
#'
#' Confidence intervals are restricted to the attainable range. Measures
#' such as [cramerV] may be preferable when inference is central.
#'
#' @references
#' Sakoda, J.M. (1977) Measures of Association for Multivariate Contingency
#' Tables, \emph{Proceedings of the Social Statistics Section of the American
#' Statistical Association} (Part III), 777-780.
#'
#' Efron, B., Tibshirani, R.J. (1993) \emph{An Introduction to the
#' Bootstrap}, Chapman & Hall, chapter 14.
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
                     correct = FALSE,
                     conf.level = NA,
                     sides = c("two.sided", "left", "right"),
                     method = c("boot"),
                     ...) {

  sides  <- match.arg(sides)
  method <- match.arg(method)

  # Type and length BEFORE is.na(): NA is logical, so a test that leads
  # with !is.numeric() rejects the function's own default, and is.na() on
  # a vector of length != 1 makes the `if` itself the error message.
  if (length(conf.level) != 1L ||
      !(is.numeric(conf.level) || is.logical(conf.level)) ||
      is.nan(conf.level) ||
      (!is.na(conf.level) && (conf.level <= 0 || conf.level >= 1)))
    stop("'conf.level' must be a single number in (0, 1), or NA")

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
  # probability 0 or 1 and is closed at the range boundary below. The
  # former version instead ran the two-sided machinery at a doubled alpha,
  # which for conf.level <= 0.5 turned the two probabilities around and
  # reported a bound from the wrong tail without a word.
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

  ci <- c(max(ci[1L], 0), min(ci[2L], cMax))

  if (sides == "left")  ci[2L] <- cMax
  if (sides == "right") ci[1L] <- 0

  setNamesX(c(cc, ci), names = c("est", "lci", "uci"))
}
