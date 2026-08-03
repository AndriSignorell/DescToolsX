
#' Hoeffding's D Statistic (Fast Computation)
#'
#' Computes Hoeffding's D statistic for testing independence between two variables
#' using an efficient \eqn{O(n \log n)} algorithm based on rank statistics and
#' Fenwick trees (Even-Zohar & Leng, 2020).
#'
#' This implementation is substantially faster than classical \eqn{O(n^2)} approaches
#' and yields results numerically identical to those from \code{Hmisc::hoeffd()}
#' for continuous data without ties.
#'
#' @param x numeric vector
#' @param y numeric vector with the same length as \code{x}
#' @param jitter logical. If \code{TRUE}, small random noise is added to
#'   \strong{both} \code{x} and \code{y} to break ties. This is useful when
#'   the data contain ties, since the fast algorithm assumes continuous data.
#' @param eps optional numeric half-width of the uniform jitter noise, which
#'   is drawn from \eqn{U(-eps, eps)}. Defaults to \code{1e-10} times the
#'   standard deviation of the affected variable. A variable with zero
#'   variance cannot be jittered and raises an error.
#' @param seed optional integer random seed for reproducibility when
#'   \code{jitter = TRUE}. The state of R's random number generator is
#'   restored afterwards, so passing a seed does not disturb the calling
#'   session.
#'
#' @return numeric scalar containing Hoeffding's D statistic on the
#' conventional scale: 1 under perfect monotone or antitone dependence,
#' 0 under independence, with a lower bound that approaches \eqn{-0.5}
#' for large \eqn{n} (for \eqn{n = 7} the minimum over all permutations
#' is \eqn{-0.26}).
#'
#' Note that the raw statistic of Hollander and Wolfe lies in
#' \eqn{[-1/60, 1/30]}; this function returns 30 times that value, which
#' is the scale \code{Hmisc::hoeffd()} reports. The two differ by exactly
#' the factor 30, so results are comparable only after accounting for it.
#'
#' @details
#' The algorithm requires a strict ordering of the data and therefore assumes
#' no ties. If ties are present and \code{jitter = FALSE}, a warning is issued
#' and results may be biased.
#'
#' Missing values are an error rather than a silent approximation:
#' \code{\link{order}} and \code{\link{rank}} place \code{NA}s at the end
#' by default, which yields a formally valid permutation and hence a number
#' that looks like an answer. Remove or impute them before calling.
#'
#' Setting \code{jitter = TRUE} resolves ties by adding small random noise,
#' yielding a fast and practical approximation.
#'
#' In contrast, \code{Hmisc::hoeffd()} handles ties via midranks but uses a
#' slower algorithm.
#'
#' @references
#' Even-Zohar, C. and Leng, C. (2020).
#' Fast computation of Hoeffding's D statistic.
#'
#' Hollander, M., Wolfe, D. A. and Chicken, E. (2013).
#' Nonparametric Statistical Methods (3rd ed.).
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(200)
#' y <- x^2 + rnorm(200)
#'
#' # fast computation
#' hoeffdingD(x, y)
#'
#' # with ties
#' y2 <- round(y, 1)
#' hoeffdingD(x, y2)                # warning
#' hoeffdingD(x, y2, jitter = TRUE) # recommended
#'
#' # perfect monotone dependence is 1 on this scale
#' hoeffdingD(1:50, (1:50)^3)
#'
#' @seealso \code{\link{spearmanCor}}, \code{\link{kendallTauB}}
#'
#' @section Random number generation:
#' \code{jitter = TRUE} draws from R's random number generator and
#' therefore advances it, unless \code{seed} is supplied - in which case
#' the previous state is restored.
#'
#' @family assoc.continuous
#' @concept association-measure
#' @concept nonlinear-association
#' @export
hoeffdingD <- function(x, y, jitter = FALSE, eps = NULL, seed = NULL) {
  
  if (length(x) != length(y))
    stop("x and y must have same length")
  
  if (!is.numeric(x) || !is.numeric(y))
    stop("'x' and 'y' must be numeric")

  n <- length(x)
  if (n < 5)
    stop("Need at least 5 observations")

  # order() and rank() sort NAs to the end by default, so a missing value
  # produced a formally valid permutation and hence a plausible-looking
  # number with no warning anywhere.
  if (anyNA(x) || anyNA(y))
    stop("'x' and 'y' must not contain missing values")

  if (!all(is.finite(x)) || !all(is.finite(y)))
    stop("'x' and 'y' must not contain infinite values")

  tiesX <- anyDuplicated(x) > 0L
  tiesY <- anyDuplicated(y) > 0L

  if ((tiesX || tiesY) && !jitter) {
    warning(
      "Ties detected. The fast algorithm assumes continuous data.\n",
      "Use jitter = TRUE to break ties (recommended), ",
      "or expect small bias."
    )
  }

  if (jitter) {

    # Restore the generator afterwards: set.seed(seed) used to reset the
    # caller's stream permanently, so a single hoeffdingD(..., seed = 42)
    # silently reseeded the whole session.
    if (!is.null(seed)) {
      if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
        oldSeed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
        on.exit(assign(".Random.seed", oldSeed, envir = globalenv()), add = TRUE)
      } else {
        on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())),
                add = TRUE)
      }
      set.seed(seed)
    }

    # x was never jittered, although the tie check above looks at both
    # variables - and jitter = TRUE suppresses the warning. Ties in x
    # therefore went through silently biased, which is the opposite of
    # what the argument promises.
    if (tiesX) x <- .jitterTies(x, eps, "x")
    if (tiesY) y <- .jitterTies(y, eps, "y")
  }

  ordX  <- order(x)
  rankY <- rank(y, ties.method = "first")
  perm  <- rankY[ordX] - 1

  hoeffdingD_cpp(perm)

}


# == internal helper functions ================================================

# Adds U(-eps, eps) noise to break ties. The default eps is relative to
# the variable's own spread, which is why it has to be computed per
# variable rather than once from y.
#' @noRd
.jitterTies <- function(z, eps = NULL, name = "x") {

  if (is.null(eps)) {

    sdZ <- sd(z)

    # eps = 1e-10 * sd(z) is zero for a constant vector, so the jitter
    # silently did nothing and the ties survived into the algorithm
    if (!is.finite(sdZ) || sdZ == 0)
      stop(gettextf("'%s' has no variation; ties cannot be broken by jittering",
                    name), domain = NA)

    eps <- 1e-10 * sdZ
  }

  if (!is.numeric(eps) || length(eps) != 1L || !is.finite(eps) || eps <= 0)
    stop("'eps' must be a single positive number")

  z + runif(length(z), -eps, eps)
}

