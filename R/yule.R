#' Yule's Coefficients of Association (Q and Y)
#'
#' Computes Yule's Q or Y for a 2x2 contingency table, optionally with
#' asymptotic confidence interval based on the log odds ratio.
#'
#' @param x a 2x2 contingency table (matrix or table). If \code{y} is supplied,
#'   \code{x} and \code{y} are cross-tabulated via \code{table()}.
#' @param y optional second variable for cross-tabulation
#' @param conf.level confidence level for the interval. Defaults to \code{0.95};
#'   use \code{NA} to return only the point estimate.
#' @param sides type of confidence interval, one of \code{"two.sided"},
#'   \code{"left"}, or \code{"right"}
#' @param correction logical; if \code{TRUE}, applies the Haldane--Anscombe
#'   correction by adding 0.5 to all cells
#' @param ... further arguments passed to \code{table()}
#'
#' @details
#' For a 2x2 table with cell counts \eqn{a, b, c, d}:
#'
#' Odds ratio:
#' \deqn{OR = \frac{ad}{bc}}
#'
#' Yule's Q:
#' \deqn{Q = \frac{OR - 1}{OR + 1}
#'      = \tanh\left(\frac{1}{2}\log(OR)\right)}
#'
#' Yule's Y:
#' \deqn{Y = \frac{\sqrt{OR} - 1}{\sqrt{OR} + 1}
#'      = \tanh\left(\frac{1}{4}\log(OR)\right)}
#'
#' Both coefficients are computed from the \code{tanh} form, which stays
#' well defined when a zero cell drives the odds ratio to 0 or \code{Inf}
#' (the coefficient is then -1 or 1).
#'
#' Confidence intervals are obtained from the asymptotic normal approximation:
#' \deqn{\log(OR) \pm z \cdot \sqrt{1/a + 1/b + 1/c + 1/d}}
#' and then transformed to the selected coefficient. With a zero cell the
#' standard error is infinite and the interval degenerates to
#' \eqn{[-1, 1]}; use \code{correction = TRUE} to obtain a finite interval.
#'
#' For a one-sided interval the open side is reported at the range limit
#' (-1 resp. 1), not at \eqn{\pm\infty}.
#'
#' @name yule
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Yule's Q or Y}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @references
#' Yule, G. U. (1912). On the methods of measuring association between two attributes.
#'
#' @examples
#' m <- matrix(c(12, 5, 3, 20), nrow = 2)
#' yuleQ(m)                        # 0.8823529
#' yuleY(m, conf.level = 0.95)     # 0.6
#'
#' # a zero cell yields the limiting value 1 (and not NaN)
#' yuleQ(matrix(c(12, 5, 0, 20), nrow = 2), conf.level = NA)
#'
#' # ... a finite interval requires the Haldane-Anscombe correction
#' yuleQ(matrix(c(12, 5, 0, 20), nrow = 2), correction = TRUE)
#'
#' @rdname yule
#'
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @concept binary-association
#'
#' @export
yuleQ <- function(x, y = NULL,
                  conf.level = 0.95,
                  sides = c("two.sided", "left", "right"),
                  correction = FALSE, ...){

  .yuleCoef(x, y, conf.level = conf.level, sides = match.arg(sides),
            correction = correction, divisor = 2, ...)

}


#' @rdname yule
#' @export
yuleY <- function(x, y = NULL,
                  conf.level = 0.95,
                  sides = c("two.sided", "left", "right"),
                  correction = FALSE, ...){

  .yuleCoef(x, y, conf.level = conf.level, sides = match.arg(sides),
            correction = correction, divisor = 4, ...)

}


# Yule's Q and Y differ only in the divisor of the log odds ratio:
#   Q = tanh(log(OR)/2),  Y = tanh(log(OR)/4)
# Everything else - table construction, checks, continuity correction and the
# confidence interval - is identical, so it lives here once.
.yuleCoef <- function(x, y = NULL, conf.level, sides, correction,
                      divisor, ...){

  if(!is.null(y))
    x <- table(x, y, ...)

  if(!isTRUE(correction) && !isFALSE(correction))
    stop("Argument 'correction' must be TRUE or FALSE.")

  if(length(dim(x)) != 2L || !all(dim(x) == c(2L, 2L)))
    stop("'x' must be a 2x2 table, or 'y' must be supplied.")

  if(!is.numeric(x) || anyNA(x) || any(x < 0))
    stop("'x' must contain non-negative counts without missing values.")

  if(correction)
    x <- x + 0.5

  # note: do not name these a, b, c, d - 'c' would mask base::c()
  n11 <- x[1L, 1L]; n12 <- x[1L, 2L]
  n21 <- x[2L, 1L]; n22 <- x[2L, 2L]

  if((n11 * n22 == 0) && (n12 * n21 == 0))
    # both diagonals are zero, the odds ratio is 0/0
    return(if(is.na(conf.level)) NA_real_
           else c(est = NA_real_, lci = NA_real_, uci = NA_real_))

  logOR <- log(n11 * n22) - log(n12 * n21)   # +-Inf for a single zero cell
  est   <- tanh(logOR / divisor)             # stays in [-1, 1]

  if(is.na(conf.level))
    return(est)

  if(!is.numeric(conf.level) || length(conf.level) != 1L ||
     conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")

  se <- sqrt(1/n11 + 1/n12 + 1/n21 + 1/n22)  # Inf if any cell is 0

  conf_adj <- if(sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  if(conf_adj <= 0)
    stop("For a one-sided interval 'conf.level' must be greater than 0.5.")
  z <- qnorm(1 - (1 - conf_adj)/2)

  # logOR - z*se would be Inf - Inf = NaN for a zero cell, hence the
  # explicit limits
  lower <- if(is.finite(logOR) && is.finite(se)) tanh((logOR - z*se)/divisor) else -1
  upper <- if(is.finite(logOR) && is.finite(se)) tanh((logOR + z*se)/divisor) else  1

  if(sides == "left")  upper <-  1
  if(sides == "right") lower <- -1

  c(est = est, lci = lower, uci = upper)

}
