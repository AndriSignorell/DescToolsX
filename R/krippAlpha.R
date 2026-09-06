
#' Krippendorff's Alpha for Wide Data
#'
#' @description
#' Computes Krippendorff's alpha coefficient of interrater reliability 
#' from data in wide format (with \eqn{m} raters). The function 
#' supports nominal, ordinal, interval, and ratio measurement levels.
#'
#' @name krippAlpha
#' @aliases krippAlpha 
#' 
#' @param x a data frame, matrix, or similar wide-format object containing
#'   ratings (columns = raters, rows = subjects/items)
#' @param metric character string specifying the measurement level, i.e.
#'   the difference function \eqn{\delta^2} used to compare two categories.
#'   One of `"nominal"`, `"ordinal"`, `"interval"`,
#'   or `"ratio"`. This selects *which* alpha is computed and has
#'   nothing to do with the confidence interval - the bootstrap interval
#'   type travels as `type` through `\dots`. It is called
#'   `metric` and not `method` because `method` means the
#'   interval method everywhere else in the suite.
#' @param levels optional vector specifying the possible categories or scale
#'   values (required for the `"interval"` and
#'   `"ratio"` metrics).
#'   If `NULL`, levels are inferred from the data.
#' @param raters optional vector specifying which columns of `x`
#'   are the raters. If `NULL`, all columns are assumed to be raters.
#' @param conf.level confidence level for bootstrap confidence intervals
#'   of Krippendorff's alpha. If `NA` (default), no bootstrap is computed.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See details in [ConfidenceIntervals()].
#'   Alpha lies in \eqn{[-1, 1]}, so the open side is reported at that
#'   boundary rather than at an infinity it cannot reach.
#' @param output output format, either `"def"` (default) or
#'   `"ext"` for extended results
#' 
#' @param ... further arguments passed to [boot::boot()]. Supported
#' arguments are `type` (`"norm"`, `"basic"`,
#' `"stud"`, `"perc"`, `"bca"`), `parallel` and the number
#' of bootstrap replicates `R`. Defaults are `"basic"` for
#' `type`, option `"boot.parallel"` (or `"no"` if unset) for
#' `parallel`, and `999` for `R`.
#'
#' @details
#' The function constructs the coincidence matrix from the wide-format data 
#' using `CoincidenceFromWide` and then calculates 
#' Krippendorff's alpha based on the chosen distance metric:
#' \itemize{
#'   \item `"nominal"`: Disagreement is 0 if equal, 1 otherwise.
#'   \item `"ordinal"`: Squared difference of cumulative proportions.
#'   \item `"interval"`: Squared Euclidean distances of scale values.
#'   \item `"ratio"`: Squared relative differences of scale values.
#' }
#'
#' @return if `output = "def"` and `conf.level = NA`, a numeric
#' scalar. If `output = "def"` and a confidence interval is requested, a named
#' numeric vector with elements:
#' \describe{
#'   \item{`est`}{point estimate of Krippendorff's alpha}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' If `output = "ext"`, a list with elements:
#' \describe{
#'   \item{`alpha`}{point estimate of Krippendorff's alpha}
#'   \item{`Do`}{observed disagreement}
#'   \item{`De`}{expected disagreement under chance}
#'   \item{`O`}{coincidence matrix}
#'   \item{`nV`}{category totals in coincidence space}
#'   \item{`delta2`}{pairwise distance matrix for the selected metric}
#'   \item{`ci`}{named numeric vector with `est`, `lci`, and
#'     `uci`, or `NA` if no interval is requested}
#' }
#'
#' @references
#' Krippendorff, K. (2018). *Content Analysis: An Introduction to Its Methodology*.
#' Sage Publications.  
#'
#' @seealso [lumen::bootCI()]
#'
#' @examples
#' # Example with nominal data (3 raters, 5 subjects)
#' dat <- data.frame(
#'   r1 = c(1, 2, 1, 3, 2),
#'   r2 = c(1, 2, 2, 3, 2),
#'   r3 = c(1, 2, 1, 3, 1)
#' )
#' krippAlpha(dat, metric = "nominal")
#'
#' # Interval-scaled example
#' dat2 <- data.frame(
#'   r1 = c(1, 4, 5, 7, 2),
#'   r2 = c(2, 5, 6, 7, 1),
#'   r3 = c(1, 4, 6, 6, 2)
#' )
#' krippAlpha(dat2, metric = "interval", levels = 1:7)
#'
#'
#' @rdname krippAlpha
#'
#' @family assoc.agreement  
#' @concept agreement  
#' @concept categorical-agreement  
#' @concept reliability
#'
#'
#' @export
krippAlpha <- function(x,
                       conf.level = NA,
                       sides      = c("two.sided", "left", "right"),
                       metric     = c("nominal", "ordinal",
                                      "interval", "ratio"),
                       levels = NULL,
                       raters = NULL,
                       output = c("def", "ext"),
                       ...) {

  # Krippendorff's alpha from wide data (m raters), using O from above.
  
  # Build Krippendorff's coincidence matrix O from wide data (m raters),
  # with per-item weighting by 1 / (m_s - 1) as in KALPHA.
  
  metric <- match.arg(metric)
  sides  <- match.arg(sides)
  output <- match.arg(output)

  conf.level <- checkConfLevel(conf.level)

  if(sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1; the open side is closed at
  # alpha's own range further down.
  confAdj <- if(sides == "two.sided") conf.level else 2 * conf.level - 1
  
  O <- .CoincidenceFromWide(x, raters = raters, levels = levels)
  
  nV  <- rowSums(O)           # category totals in coincidence space
  nPairs <- sum(nV)          # total pairable values
  if (nPairs <= 1) stop("Too few valid pairs (n < 2).")
  
  K <- nrow(O)
  delta2 <- matrix(0, K, K, dimnames = dimnames(O))
  
  if (metric == "nominal") {
    delta2[] <- 1
    diag(delta2) <- 0
    
  } else if (metric == "ordinal") {
    # mid-cumulative proportions (mu_k) based on nV:
    p  <- nV / sum(nV)
    mu <- cumsum(p) - 0.5 * p
    delta2 <- (outer(mu, mu, `-`))^2
    diag(delta2) <- 0
    
  } else {
    
    if (is.null(levels)) levels <- seq_len(K)
    if (length(levels) != K) stop("'levels' must have length K.")
    levels <- as.numeric(levels)
    
    if (metric == "interval") {
      D <- outer(levels, levels, `-`)
      delta2 <- D * D
      diag(delta2) <- 0
    } else { # ratio
      S <- outer(levels, levels, `+`)
      D <- outer(levels, levels, `-`)
      delta2 <- (D / pmax(S, .Machine$double.eps))^2
      diag(delta2) <- 0
    }
  }
  
  Do <- sum(O * delta2)
  De <- sum(outer(nV, nV, `*`) * delta2) / (nPairs - 1)
  
  alpha <- if (De <= 0) NA_real_ else 1 - Do / De
  
  if(!is.na(conf.level)){
    
    calc_alpha <- function(x) 
      krippAlpha(x, 
                 metric = metric,
                 levels = levels, 
                 raters = raters, 
                 conf.level = NA)
    
    # conf.level was not passed on at all, so bootCI() used its own
    # default and krippAlpha(x, conf.level = 0.99) quietly returned a 95%
    # interval. 'sides' deliberately stays two.sided here: the level is
    # already adjusted, and the open side is closed at alpha's range
    # below rather than by whatever convention bootCI() applies.
    boot <- bootCI(x = x, FUN = calc_alpha, conf.level = confAdj, ...)
    
    # read by name, not by position: assigning names to whatever comes
    # back relabels a shorter or differently ordered vector instead of
    # failing (cf. .relRiskUseOr(), .pearsonCI())
    if(length(boot) != 3L)
      stop("bootCI() did not return three values (est, lci, uci).")
    
    if(is.null(names(boot)))
      names(boot) <- c("est", "lci", "uci")
    
    if(!all(c("est", "lci", "uci") %in% names(boot)))
      stop("bootCI() did not return the expected 'est'/'lci'/'uci' vector.")
    
    # the estimate comes from the full sample, never from the bootstrap
    ci <- c(est = alpha,
            applySides(unname(boot[c("lci", "uci")]), sides,
                        lo = -1, hi = 1))
    
  } else {
    # named NA_real_ triple rather than a bare logical NA, so that
    # callers can index $ci[["est"]] regardless of conf.level
    ci <- setNamesX(rep(NA_real_, 3), c("est", "lci", "uci"))
  }
  
  if(output == "def"){
    res <- if(is.na(conf.level)) alpha else ci
  } else {
    res <- list(alpha = alpha, Do = Do, De = De, 
                O = O, nV = nV, delta2 = delta2, ci=ci)
  }
  
  return(res)
  
}



.CoincidenceFromWide <- function(df, raters = NULL, levels = NULL) {
  
  if(is.matrix(df))
    df <- as.data.frame(df)
  
  # pick rater columns (drop typical ID col "Unit" if present)
  if (is.null(raters)) {
    raters <- setdiff(names(df), c("Unit","unit","ID","Id","id"))
  }
  X <- df[raters]
  
  # collect global levels if not given
  if (is.null(levels)) {
    levels <- sort(unique(unlist(X, use.names = FALSE)))
  }
  levels <- as.character(levels)
  K <- length(levels)
  
  O <- matrix(0, K, K, dimnames = list(levels, levels))
  
  for (s in seq_len(nrow(X))) {
    row <- unlist(X[s, ], use.names = FALSE)
    row <- row[!is.na(row)]
    m_s <- length(row)
    if (m_s < 2L) next
    
    tab <- table(factor(row, levels = levels))
    tvec <- as.numeric(tab)
    
    # unordered coincidences, weighted by 1/(m_s - 1)
    Os <- tvec %o% tvec
    diag(Os) <- diag(Os) - tvec
    
    O <- O + Os / (m_s - 1)
  }
  O
}


# 
# # "Krippendorff alpha ordinal (wide, Hayes/Krippendorff Beispiel)"
# 
# df <- data.frame(
#   Unit = 1:40, 
#   obs1 = c(1L, 1L, 2L, NA, 0L, 0L, 1L, 1L, 2L, 2L, NA, 0L, 1L, 
#            3L, 1L, 1L, 2L, 1L, 1L, 0L, 0L, 0L, 2L, 0L, 1L, 0L, 
#            0L, 1L, 1L, 1L, 1L, 2L, 2L, 3L, 2L, 2L, 2L, 2L, 2L, 1L), 
#   obs2 = c(1L, 1L, 3L, 0L, 0L, 0L, 0L, NA, 2L, 1L, 1L, 0L, 2L, 3L, 
#            1L, 1L, 1L, 2L, 1L, 0L, 0L, 0L, 3L, 0L, 2L, 1L, 0L, 2L, 
#            1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 1L), 
#   obs3 = c(2L, 0L, 3L, 0L, 0L, 0L, 2L, 2L, 2L, 1L, 0L, 0L, 2L, 2L, 
#            1L, 1L, 2L, 3L, 0L, 0L, 1L, NA, 3L, 0L, NA, 1L, 0L, 1L, 
#            2L, 2L, 0L, 2L, NA, 2L, 2L, 3L, 2L, NA, 2L, 1L), 
#   obs4 = c(NA, 1L, 3L, NA, NA, NA, NA, 0L, NA, 1L, 0L, 0L, 2L, 2L, 
#            NA, NA, NA, 3L, 1L, NA, 1L, 0L, 3L, 0L, 2L, 1L, 1L, 2L, 
#            2L, NA, NA, 1L, 2L, 2L, NA, NA, NA, 1L, 2L, NA), 
#   obs5 = c(2L, NA, NA, 0L, 0L, 0L, 1L, NA, 2L, NA, NA, NA, NA, 3L, 
#            1L, 1L, 2L, NA, NA, 0L, NA, 0L, NA, NA, 2L, NA, 0L, NA, 
#            NA, 2L, 0L, NA, 2L, NA, 2L, 2L, 2L, 2L, NA, 1L)) 
# 
# res <- krippAlpha(df, 
#           metric = "ordinal",
#           levels = 0:3, raters = paste0("obs",1:5))
# 
# testthat::expect_equal(res$alpha, 0.7598, tolerance = 1e-4)


