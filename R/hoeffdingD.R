
#' Hoeffding's D Statistic
#'
#' Computes Hoeffding's D statistic for testing independence between two
#' variables. Two algorithms are available: an \eqn{O(n \log n)} one based on
#' rank statistics and Fenwick trees (Even-Zohar & Leng, 2020), and the
#' classical \eqn{O(n^2)} one, which is slower but handles ties exactly.
#'
#' @param x numeric vector
#' @param y numeric vector with the same length as `x`
#' @param engine character string selecting the algorithm:
#'   `"fast"` (default) or `"exact"`. Both compute the same
#'   quantity; on data without ties they agree to floating point accuracy.
#'   With ties they differ - see Details.
#' @param R number of permutations used for the test of independence. Only
#'   used when `output = "test"`; defaults to 999.
#' @param output output format, either `"def"` (default), which returns
#'   the statistic, or `"test"`, which returns an object of class
#'   `"htest"` with a permutation P value for the hypothesis of
#'   independence.
#' @param jitter logical. If `TRUE`, small random noise is added to
#'   **both** `x` and `y` to break ties. This is useful when
#'   the data contain ties, since the fast algorithm assumes continuous data.
#' @param eps optional numeric half-width of the uniform jitter noise, which
#'   is drawn from \eqn{U(-eps, eps)}. Defaults to `1e-10` times the
#'   standard deviation of the affected variable. A variable with zero
#'   variance cannot be jittered and raises an error.
#' @param seed optional integer random seed for reproducibility when
#'   `jitter = TRUE`. The state of R's random number generator is
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
#' is the scale `Hmisc::hoeffd()` reports. The two differ by exactly
#' the factor 30, so results are comparable only after accounting for it.
#'
#' @details
#' \subsection{Choice of engine}{
#' `engine = "fast"` runs in \eqn{O(n \log n)} and is what makes six-figure
#' sample sizes practical. It needs a strict ordering and therefore assumes no
#' ties; with tied values it is biased, and the size of the bias grows with the
#' number of ties.
#'
#' `engine = "exact"` is the classical formulation of Hollander and Wolfe.
#' It resolves ties through midranks and the fractional counts \eqn{Q_i}, so it
#' is exact for tied data - the same quantity `Hmisc::hoeffd()` reports.
#' It costs \eqn{O(n^2)} time, which is a few milliseconds at \eqn{n = 1000}
#' and roughly a second at \eqn{n = 10000}.
#'
#' On data without ties the two agree to floating point accuracy, so the
#' choice only matters when ties are present. Then there are three options,
#' in descending order of preference: `engine = "exact"` answers the
#' question exactly, `jitter = TRUE` answers it approximately but
#' quickly, and doing neither answers a slightly different question. Only the
#' last one is a mistake.
#' }
#'
#' \subsection{Ties and jittering}{
#' `jitter = TRUE` breaks ties by adding small random noise, which makes
#' the fast engine applicable at the cost of a small random perturbation of
#' the result. It applies to the fast engine only; with
#' `engine = "exact"` it is unnecessary and is refused rather than
#' silently ignored.
#' }
#'
#' \subsection{Test of independence}{
#' `output = "test"` returns an `"htest"` object carrying a
#' permutation P value for the null hypothesis that \eqn{x} and \eqn{y} are
#' independent. Under that hypothesis every pairing of the two samples is
#' equally likely, so the null distribution is obtained by recomputing the
#' statistic on `R` random pairings. The P value is
#' \eqn{(1 + \#\{D^* \ge D\}) / (R + 1)}, which is never exactly zero - with
#' `R` permutations no evidence stronger than \eqn{1/(R+1)} has been
#' gathered, and reporting 0 would claim otherwise.
#'
#' The permutation route was chosen over the tabulated asymptotic
#' distribution on purpose: it needs neither a table nor a scaling constant,
#' it is exact by construction, and it costs `R` evaluations of a
#' statistic that is fast - which is the whole point of the fast engine.
#' With `engine = "exact"` the permutations are applied to `y`
#' itself, so the tie structure of both variables is preserved and the null
#' distribution belongs to the same tied data.
#' }
#'
#' \subsection{Why there is no confidence interval}{
#' D is a U-statistic whose kernel is degenerate exactly under independence
#' and non-degenerate otherwise. Away from independence
#' \eqn{\sqrt{n}(\hat{D} - D)} is asymptotically normal and an interval is in
#' principle available. At \eqn{D = 0} the limit is not a normal
#' distribution but a weighted sum of chi-squares (Blum, Kiefer and
#' Rosenblatt, 1961), and the variance of the first-order projection
#' vanishes - so an interval built on the non-degenerate asymptotics
#' collapses precisely where most data sit.
#'
#' The obvious escape does not work either. For a degenerate U-statistic the
#' ordinary bootstrap is not consistent; a valid resampling scheme has to be
#' built on the second-order term of the Hoeffding decomposition (Arcones
#' and Gine, 1992). A percentile or BCa interval from the usual machinery
#' would therefore be serviceable for strongly dependent data and wrong
#' exactly at independence, which is the one place a reader would look.
#'
#' What is well founded here is the test, and that is what
#' `output = "test"` provides. An interval over the first-order
#' projection could be constructed for data that are clearly far from
#' independence, but it is deliberately not offered: its coverage cannot be
#' relied on in the situation the statistic is most often used for.
#' }
#'
#' Missing values are an error rather than a silent approximation:
#' [order()] and [rank()] place `NA`s at the end
#' by default, which yields a formally valid permutation and hence a number
#' that looks like an answer. Remove or impute them before calling.
#'
#' @references
#' Even-Zohar, C. and Leng, C. (2020).
#' Fast computation of Hoeffding's D statistic.
#'
#' Hollander, M., Wolfe, D. A. and Chicken, E. (2013).
#' Nonparametric Statistical Methods (3rd ed.).
#'
#' Blum, J. R., Kiefer, J. and Rosenblatt, M. (1961).
#' Distribution free tests of independence based on the sample distribution
#' function. *Annals of Mathematical Statistics* **32**, 485-498.
#'
#' Arcones, M. A. and Gine, E. (1992).
#' On the bootstrap of U and V statistics.
#' *Annals of Statistics* **20**, 655-674.
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
#' hoeffdingD(x, y2)                        # warning: the fast engine is biased
#' hoeffdingD(x, y2, engine = "exact")      # exact, and the recommended answer
#' hoeffdingD(x, y2, jitter = TRUE)         # fast approximation
#'
#' # without ties the two engines agree
#' all.equal(hoeffdingD(x, y),
#'           hoeffdingD(x, y, engine = "exact"))
#'
#' # perfect monotone dependence is 1 on this scale, antitone likewise
#' hoeffdingD(1:50, (1:50)^3)
#' hoeffdingD(1:50, 50:1)
#'
#' # test of independence - the quadratic relation is invisible to
#' # correlation but not to D
#' set.seed(3)
#' hoeffdingD(x, y, output = "test")
#' cor.test(x, y)$p.value
#'
#' @seealso [spearmanCor()], [kendallTauB()]
#'
#' @section Random number generation:
#' `jitter = TRUE` draws from R's random number generator and
#' therefore advances it, unless `seed` is supplied - in which case
#' the previous state is restored.
#'
#' @family assoc.continuous
#' @concept association-measure
#' @concept nonlinear-association
#' @export
hoeffdingD <- function(x, y,
                       engine = c("fast", "exact"),
                       R = 999,
                       jitter = FALSE, eps = NULL, seed = NULL,
                       output = c("def", "test")) {
  
  engine <- match.arg(engine)
  output <- match.arg(output)
  checkFlag(jitter)
  
  if (!is.numeric(R) || length(R) != 1L || !is.finite(R) ||
      R < 1 || R %% 1 != 0 || R > .Machine$integer.max)
    stop("'R' must be a single positive whole number")
  
  dataName <- paste(deparse(substitute(x)), "and", deparse(substitute(y)))
  
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

  # Hoisted out of the jitter block: both the jittering and the permutation
  # test draw from R's generator, and a caller who supplies a seed expects
  # it to cover whichever of the two runs. Restoring the stream afterwards
  # is the point - set.seed(seed) alone used to reseed the whole session.
  if (!is.null(seed)) {

    if (!is.numeric(seed) || length(seed) != 1L || !is.finite(seed))
      stop("'seed' must be a single number, or NULL")

    if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      oldSeed <- get(".Random.seed", envir = globalenv(), inherits = FALSE)
      on.exit(assign(".Random.seed", oldSeed, envir = globalenv()), add = TRUE)
    } else {
      on.exit(suppressWarnings(rm(".Random.seed", envir = globalenv())),
              add = TRUE)
    }

    set.seed(seed)
  }

  # The exact engine resolves ties itself, so neither the warning nor the
  # jittering applies to it.
  if (engine == "exact") {

    if (jitter)
      stop("'jitter' applies to engine = \"fast\" only; the exact engine ",
           "resolves ties through midranks.")

    d <- .hoeffdingDExact(x, y)

    if (output == "def")
      return(d)

    # y itself is permuted, so both tie structures survive into the null
    # distribution - the exact engine's whole reason for existing
    nullD <- vapply(seq_len(R),
                    function(i) .hoeffdingDExact(x, y[sample.int(length(y))]),
                    numeric(1L))

    return(.hoeffdingTest(d, nullD, R, length(x), dataName,
                          "exact, permutation"))
  }

  if ((tiesX || tiesY) && !jitter) {
    warning(
      "Ties detected. The fast algorithm assumes continuous data.\n",
      "Use engine = \"exact\" for the exact answer, or jitter = TRUE for a ",
      "fast approximation; otherwise expect bias."
    )
  }

  if (jitter) {

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

  d <- hoeffdingD_cpp(perm)

  if (output == "def")
    return(d)

  # The fast engine reads the data only through 'perm', the permutation
  # linking the order of x to the ranks of y. Under independence every one
  # of the n! permutations is equally likely, so drawing them directly IS
  # the permutation null - no reordering and no re-ranking per replicate.
  n     <- length(perm)
  nullD <- vapply(seq_len(R),
                  function(i) hoeffdingD_cpp(sample.int(n) - 1),
                  numeric(1L))

  .hoeffdingTest(d, nullD, R, n, dataName, "fast, permutation")

}


# == internal helper functions ================================================

# Assembles the htest object from the observed statistic and the permutation
# null. D is large under dependence and near zero under independence, so the
# test is upper-tailed by construction; there is no two-sided variant to
# offer, which is why no 'alternative' argument exists.
#' @noRd
.hoeffdingTest <- function(d, nullD, R, n, dataName, engineLabel) {

  if (anyNA(nullD))
    stop("some permutation replicates could not be computed.")

  # (1 + #{D* >= D}) / (R + 1), not #{...}/R: with R permutations no
  # evidence stronger than 1/(R+1) has been gathered, and a reported 0
  # would claim otherwise. The correction also keeps the test valid at
  # its nominal level.
  pValue <- (1 + sum(nullD >= d)) / (R + 1)

  structure(
    list(
      statistic = c(D = d),
      parameter = c(n = n, R = R),
      p.value   = pValue,
      method    = sprintf(
        "Hoeffding's test of independence (%s, R = %d)", engineLabel, R),
      data.name = dataName
    ),
    class = "htest"
  )
}


# Hollander and Wolfe's classical formulation, O(n^2) in time and O(n) in
# memory. This is the version that handles ties, through midranks in R and
# S and through the fractional counts in Q:
#
#   Q_i = 1 + #{j: x_j < x_i, y_j < y_i}
#           + 1/4 #{j != i: x_j = x_i, y_j = y_i}
#           + 1/2 #{j != i: x_j = x_i, y_j < y_i}
#           + 1/2 #{j != i: x_j < x_i, y_j = y_i}
#
# The four categories are mutually exclusive, so they can be counted
# separately; only the second contains i itself, hence the -1 there.
#
# Verified against the documented reference points: 1 for perfect monotone
# and antitone dependence, approximately 0 under independence, and
# -0.2619 for the minimum over all permutations at n = 7.
#' @noRd
.hoeffdingDExact <- function(x, y) {

  n <- length(x)

  # midranks - rank()'s default, and the reason this engine copes with ties
  R <- rank(x)
  S <- rank(y)

  Q <- numeric(n)

  # one vectorised pass per observation: O(n^2) time, but O(n) memory, so
  # it does not fall over at the sample sizes where outer() would
  for (i in seq_len(n)) {

    lx <- x < x[i]
    ex <- x == x[i]
    ly <- y < y[i]
    ey <- y == y[i]

    Q[i] <- 1 +
      sum(lx & ly) +
      0.25 * (sum(ex & ey) - 1) +
      0.50 * sum(ex & ly) +
      0.50 * sum(lx & ey)
  }

  D1 <- sum((Q - 1) * (Q - 2))
  D2 <- sum((R - 1) * (R - 2) * (S - 1) * (S - 2))
  D3 <- sum((R - 2) * (S - 2) * (Q - 1))

  # the factor 30 puts the result on the scale Hmisc::hoeffd() reports,
  # matching the fast engine
  30 * ((n - 2) * (n - 3) * D1 + D2 - 2 * (n - 2) * D3) /
    (n * (n - 1) * (n - 2) * (n - 3) * (n - 4))
}


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

