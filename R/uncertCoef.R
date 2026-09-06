#' Uncertainty Coefficient
#'
#' Computes directional or symmetric uncertainty coefficients. The
#' directional coefficient U(C|R) measures the proportion of uncertainty
#' (entropy) in the column variable Y that is explained by the row variable X.
#' The function has interfaces for a table, a matrix, a data frame, and single
#' vectors.
#'
#' The uncertainty coefficient is computed as \deqn{U(C|R) = \frac{H(X) + H(Y)
#' - H(XY)}{H(Y)} } and ranges from \verb{[0, 1]}.
#'
#' @param x a numeric vector, factor, matrix, or data frame
#' @param y `NULL` (default) or a vector, an ordered factor, matrix or
#' data frame with compatible dimensions to `x`
#' @param direction direction of calculation, one of `"symmetric"`
#' (default), `"row"`, or `"column"`. The row direction calculates
#' U(R|C), and the column direction calculates U(C|R).
#' @param pZeroCorrection small positive value used to replace zero cells
#' before taking logarithms
#' 
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()].
#' 
#' @param \dots further arguments are passed to the function
#' [table()], allowing, for example, `useNA` to be set. This
#' refers only to the
#' vector interface.
#' 
#' @return if `conf.level = NA`, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{uncertainty coefficient estimate}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' @note Based on code from Antti Arppe
#'
#' @section Confidence interval:
#' The interval is based on the asymptotic standard error (Goodman & Kruskal)
#' and is truncated to the parameter range \eqn{[0, 1]}. For a one-sided
#' interval (`sides = "left"` or `"right"`) the open side is reported
#' at the corresponding range limit, not at \eqn{\pm\infty}.
#'
#' @seealso [Association()]
#' @references Theil, H. (1972), *Statistical Decomposition Analysis*,
#' Amsterdam: North-Holland Publishing Company.
#'
#' @examples
#'
#' # example from Goodman Kruskal (1954)
#'
#' m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))
#' dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
#' m
#'
#' # direction default is "symmetric"
#' uncertCoef(m)
#' uncertCoef(m, conf.level=0.95)
#'
#' uncertCoef(m, direction="row")
#' uncertCoef(m, direction="column")
#'
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @concept information-theory
#'
#' @export
uncertCoef <- function(x, y = NULL,
                       conf.level = NA,
                       sides = c("two.sided", "left", "right"),
                       direction = c("symmetric", "row", "column"),
                       pZeroCorrection = 1/sum(x)^2,
                       ... ) {

  # Theil's UC (1970)
  # slightly nudge zero values so that their logarithm can be
  # calculated (cf. Theil 1970: x->0 => xlogx->0)

  if(!is.null(y)) x <- table(x, y, ...)

  sides     <- match.arg(sides)
  direction <- match.arg(direction)

  conf.level <- checkConfLevel(conf.level)

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1. At or below 0.5 that level is
  # not positive and the normal quantile turns negative, which hands back
  # the two bounds in reverse order.
  if(sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  confAdj <- if(sides == "two.sided") conf.level else 2 * conf.level - 1

  if(length(dim(x)) != 2L)
    stop("'x' must be a two-dimensional contingency table, ",
         "or 'y' must be supplied.")

  if(!is.numeric(x) || anyNA(x) || any(x < 0))
    stop("'x' must contain non-negative counts without missing values.")

  if(any(dim(x) < 2L))
    # with a single row or column the corresponding entropy is 0 and the
    # directional coefficient would be 0/0
    stop("'x' must have at least two rows and two columns.")

  # force pZeroCorrection before x is modified below
  pZero <- pZeroCorrection
  x[x == 0] <- pZero

  n    <- sum(x)
  rsum <- rowSums(x)
  csum <- colSums(x)

  hx  <- -sum(rsum * log(rsum/n)) / n
  hy  <- -sum(csum * log(csum/n)) / n
  hxy <- -sum(x * log(x/n)) / n

  res <- switch(direction,
                "symmetric" = 2 * (hx + hy - hxy) / (hx + hy),
                "row"       =     (hx + hy - hxy) / hx,
                "column"    =     (hx + hy - hxy) / hy)

  if(is.na(conf.level))
    return(res)

  if(!is.numeric(conf.level) || length(conf.level) != 1L ||
     conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")

  sigma2 <- switch(direction,
                   "symmetric" =
                     4 * sum(x * (hxy * log(rsum %o% csum / n^2) -
                                    (hx + hy) * log(x/n))^2) /
                     (n^2 * (hx + hy)^4),
                   "row" =
                     sum(x * (hx * log(x / csum[col(x)]) +
                                (hy - hxy) * log(rsum[row(x)] / n))^2) /
                     (n^2 * hx^4),
                   "column" =
                     sum(x * (hy * log(x / rsum[row(x)]) +
                                (hx - hxy) * log(csum[col(x)] / n))^2) /
                     (n^2 * hy^4))

  z  <- qnorm(1 - (1 - confAdj)/2)
  ci <- res + c(-1, 1) * z * sqrt(sigma2)

  # the uncertainty coefficient lies in [0, 1]; applySides() clamps to
  # that range and closes the open side there (design_rules.md 4.1)
  c(est = res, applySides(ci, sides, lo = 0, hi = 1))

}
