
#' Cohen's Kappa and Weighted Kappa
#'
#' Computes Cohen's kappa and weighted kappa as measures of inter-rater
#' agreement, together with asymptotic confidence intervals.
#'
#' @details
#' Cohen's kappa is the diagonal sum of the (possibly weighted) relative
#' frequencies, corrected for chance agreement and standardised by its
#' maximum value.
#'
#' The equal-spacing weights (Cicchetti & Allison, 1971) are defined as
#' \deqn{1 - \frac{|i - j|}{r - 1}}{1 - |i - j| / (r - 1)}
#' and the Fleiss-Cohen weights as
#' \deqn{1 - \frac{(i - j)^2}{(r - 1)^2}}{1 - (i - j)^2 / (r - 1)^2}
#' where \eqn{r} is the number of rows/columns.  The Fleiss-Cohen weights
#' attach greater importance to closer disagreements.
#'
#' Data can be passed either as a square confusion matrix (or data frame)
#' in \code{x}, or as two vectors \code{x} and \code{y}, in which case
#' \code{table(x, y, \dots)} is computed internally.  Note that the vector
#' interface supports \strong{unweighted kappa only}: the function raises
#' an error if \code{weights} is not \code{"unweighted"} and \code{y} is
#' supplied, because the level ordering of two independent factors cannot
#' be guaranteed to be consistent when constructing the confusion table.
#'
#' Missing values are handled as \code{\link{table}} does -- excluded by
#' default.  Pass \code{useNA = "ifany"} via \code{...} to include them.
#'
#' \code{sides} names the side on which the finite bound lies:
#' \code{"left"} yields an interval bounded below, with the upper limit
#' fixed at 1, and \code{"right"} one bounded above, with the lower limit
#' fixed at -1. Kappa is a bounded parameter, so the open side is reported
#' at the range boundary rather than as \eqn{\pm\infty} (design_rules.md
#' 4.1), matching \code{\link{ccc}}. 
#'
#' @param x        a square confusion matrix (or data frame), or a
#'   categorical vector when \code{y} is provided
#' @param y        \code{NULL} (default) or a categorical vector with
#'   compatible dimensions to \code{x}. When supplied,
#'   \code{table(x, y, \dots)} is computed. The vector interface is
#'   available for unweighted kappa only (see Details).
#' @param weights  either a character string selecting a built-in weight
#'   scheme -- \code{"unweighted"} (default), \code{"equal-spacing"}, or
#'   \code{"fleiss-cohen"} -- or a numeric matrix with the same dimensions
#'   as \code{x} supplying user-defined weights for each cell
#'   
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See \code{\link{ConfidenceIntervals}}.
#'   
#' @param ...      further arguments passed to \code{\link{table}} for the
#'   vector interface, for example \code{useNA}
#'
#' @return if \code{conf.level = NA}, a numeric scalar containing kappa;
#' otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of kappa.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#'
#' @note
#' Based on code by David Meyer, adapted to conform to package standards.
#'
#' @references
#' Cohen, J. (1960). A coefficient of agreement for nominal scales.
#'   \emph{Educational and Psychological Measurement}, \emph{20}(1),
#'   37--46.
#'
#' Everitt, B. S. (1968). Moments of statistics kappa and weighted kappa.
#'   \emph{The British Journal of Mathematical and Statistical Psychology},
#'   \emph{21}(1), 97--103.
#'
#' Fleiss, J. L., Cohen, J., & Everitt, B. S. (1969). Large sample
#'   standard errors of kappa and weighted kappa.
#'   \emph{Psychological Bulletin}, \emph{72}(5), 323--327.
#'
#' Cicchetti, D. V., & Allison, T. (1971). A new procedure for assessing
#'   reliability of scoring EEG sleep recordings.
#'   \emph{American Journal of EEG Technology}, \emph{11}(3), 101--109.
#'
#' @seealso \code{\link[bedrock]{pairApply}}
#'
#' @examples
#' # from Bortz et al. (1990), p. 459
#' m <- matrix(c(53,  5, 2,
#'               11, 14, 5,
#'                1,  6, 3), nrow = 3, byrow = TRUE,
#'             dimnames = list(rater1 = c("V", "N", "P"),
#'                             rater2 = c("V", "N", "P")))
#'
#' cohenKappa(m)
#' cohenKappa(m, conf.level = 0.95)
#'
#' # vector interface (unweighted only)
#' x <- bedrock::untable(m)
#' cohenKappa(x$rater1, x$rater2)
#'
#' # equal-spacing weights
#' cats <- c("<10%", "11-20%", "21-30%", "31-40%", "41-50%", ">50%")
#' mw <- matrix(
#'   c(5,8,1,2,4,2, 3,5,3,5,5,0, 1,2,6,11,2,1,
#'     0,1,5,4,3,3, 0,0,1,2,5,2, 0,0,1,2,1,4),
#'   nrow = 6, byrow = TRUE,
#'   dimnames = list(rater1 = cats, rater2 = cats))
#'
#' cohenKappa(mw, weights = "equal-spacing", conf.level = 0.95)
#'
#' # user-supplied weight matrix
#' wm <- outer(1:6, 1:6, function(i, j) 1 - abs(i - j) / (6 - 1))
#' cohenKappa(mw, weights = wm, conf.level = 0.95)
#'
#' # pairwise kappa across raters
#' rating <- data.frame(
#'   rtr1 = c(4,2,2,5,2,1,3,1,1,5,1,1,2,1,2,3,1,1,2,1,5,2,2,1,1,2,1,2,1,5),
#'   rtr2 = c(4,2,3,5,2,1,3,1,1,5,4,2,2,4,2,3,1,1,2,3,5,4,2,1,4,2,1,2,3,5))
#'
#' bedrock::pairApply(rating, FUN = cohenKappa, symmetric = TRUE)
#'
#' @family assoc.agreement
#' @concept categorical-agreement
#' @export
cohenKappa <- function(x,
                       y          = NULL,
                       conf.level = NA,
                       sides      = c("two.sided", "left", "right"),
                       weights    = c("unweighted", "equal-spacing",
                                      "fleiss-cohen"),
                       ...) {

  # --- conf.level: length check first (before NA test) -----------------
  # NULL used to slip through here and then blow up on is.na(NULL), which
  # is logical(0) - "argument is of length zero".
  if (is.null(conf.level) ||
      !(length(conf.level) == 1L && is.atomic(conf.level)))
    stop("Argument 'conf.level' must be a single value or NA.")

  # --- weight argument -------------------------------------------------
  if (is.matrix(weights)) {

    # validated below, once normalizeToConfusion() has fixed the dimension

  } else if (is.character(weights)) {
    weights <- match.arg(weights)
  } else {
    stop("Argument 'weights' must be a character string or a numeric matrix.")
  }

  if (!is.null(y) && !identical(weights, "unweighted"))
    stop(
      "The vector interface supports unweighted kappa only. ",
      "Provide a confusion matrix for weighted kappa."
    )

  # --- build / validate confusion matrix --------------------------------
  # normalizeToConfusion() guarantees a square numeric matrix
  x  <- normalizeToConfusion(x = x, y = y, ...)
  nc <- ncol(x)   # == nrow(x) guaranteed by normalizeToConfusion

  # --- guard: empty table ----------------------------------------------
  n <- sum(x)
  if (n == 0)
    stop("Confusion matrix is empty (all cells zero).")

  # --- shared quantities -----------------------------------------------
  p        <- x / n                       # relative frequencies
  colFreqs <- colSums(p)
  rowFreqs <- rowSums(p)

  # --- weight matrix W -------------------------------------------------
  W <- if (is.matrix(weights)) {

    # validate user-supplied weight matrix
    if (!is.numeric(weights))
      stop("Weight matrix must be numeric.")
    if (!all(dim(weights) == nc))
      stop("Weight matrix must have the same dimensions as 'x' (",
           nc, " x ", nc, ").")
    if (any(!is.finite(weights)))
      stop("Weight matrix must not contain NA, NaN, or Inf.")
    if (any(weights < 0 | weights > 1))
      warning("Weight matrix contains values outside [0, 1]; ",
              "results may not be interpretable as kappa.")
    if (!isTRUE(all.equal(weights, t(weights))))
      warning("Weight matrix is not symmetric; weighted kappa assumes ",
              "symmetric weights.")
    weights

  } else {
    idx <- outer(seq_len(nc), seq_len(nc), `-`)
    switch(weights,
           "unweighted"    = diag(nc),
           "equal-spacing" = 1 - abs(idx) / (nc - 1),
           "fleiss-cohen"  = 1 - (idx / (nc - 1))^2
    )
  }

  # --- point estimate --------------------------------------------------
  po <- sum(W * p)

  # pc = sum_ij w_ij * p_i. * p_.j. The former outer product was built the
  # other way round (colFreqs %o% rowFreqs); for the symmetric built-in
  # weights that is numerically identical, but it differs as soon as a
  # user supplies an asymmetric weight matrix, which the argument check
  # merely warns about.
  pc    <- sum(W * (rowFreqs %o% colFreqs))
  denom <- 1 - pc

  # guard: degenerate marginal structure where pc is close to 1
  if (abs(denom) < sqrt(.Machine$double.eps))
    stop(
      "Expected agreement (pc) is too close to 1; kappa is undefined. ",
      "This occurs with degenerate marginal distributions."
    )

  k <- (po - pc) / denom

  if (is.na(conf.level))
    return(k)

  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")

  sides <- match.arg(sides)

  # --- asymptotic SE (Fleiss, Cohen & Everitt 1969) --------------------
  #
  # SE = sqrt( [sum_ij p_ij (w_ij - (w_i. + w_.j)(1-k))^2
  #             - (k - pc(1-k))^2 ] / (1-pc)^2 / n )
  #
  Wc  <- as.vector(W  %*% colFreqs)   # row-wise weighted column marginals
  Wr  <- as.vector(t(W) %*% rowFreqs) # col-wise weighted row marginals
  D   <- outer(Wc, Wr, `+`) * (1 - k) # correction matrix
  num <- sum(p * (W - D)^2) - (k - pc * (1 - k))^2

  # clamp numerical noise: num should be >= 0 by construction
  num <- max(num, 0)
  se  <- sqrt(num / denom^2 / n)

  # --- CI --------------------------------------------------------------
  confAdj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  z       <- qnorm(1 - (1 - confAdj) / 2)

  res <- c(est = k,
           lci = max(k - z * se, -1),
           uci = min(k + z * se,  1))

  # kappa is bounded: the open side is reported at the range boundary
  if (sides == "left")  res[["uci"]] <-  1
  if (sides == "right") res[["lci"]] <- -1

  res
}
