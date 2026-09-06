
#' Percent Agreement with Design-Based SE and CI
#'
#' Computes the proportion of agreement (percent agreement) along with
#' design-based standard errors and confidence intervals following
#' Klein and Gwet.
#'
#' For a confusion matrix the agreement indicator of a subject is 1 on the
#' diagonal and 0 elsewhere. For a ratings matrix the subject-wise agreement
#' is the proportion of agreeing rater *pairs*,
#' \deqn{p_{o,i} = \frac{\sum_j n_{ij}(n_{ij}-1)}{m_i (m_i - 1)},}
#' where \eqn{n_{ij}} is the number of raters who assigned subject \eqn{i} to
#' category \eqn{j} and \eqn{m_i} the number of non-missing ratings for that
#' subject. Subjects with fewer than two ratings carry no information about
#' agreement and are excluded (they still count towards \eqn{n} in the
#' variance, following Gwet).
#'
#' @param x a confusion matrix or a matrix or data frame of ratings with
#'   subjects in rows and raters in columns
#' @param y optional second rating vector used to construct a confusion matrix
#'
#' @param conf.level confidence level of the interval. If set to `NA`
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()]. A proportion of
#'   agreement lies in \eqn{[0, 1]}, so the open side is reported at that
#'   boundary rather than at \eqn{\pm\infty}.
#'
#' @param input character string specifying the input format:
#'   `"auto"`, `"confusion"`, or `"ratings"`
#' @param fpc finite population correction, the sampling fraction \eqn{n/N}
#'   in \eqn{[0, 1)} (default `0`)
#' @param output output format, either `"def"` (default) or
#'   `"ext"` for extended results
#' @param ... must be empty. Named arguments are rejected rather than
#'   silently ignored.
#'
#' @return if `output = "def"` and `conf.level = NA`, a numeric
#' scalar; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{`est`}{proportion of agreement}
#'   \item{`lci`}{lower confidence interval bound}
#'   \item{`uci`}{upper confidence interval bound}
#' }
#'
#' if `output = "ext"`, a list with the elements `est`,
#' `se`, `ci` (the named triple above), `n`,
#' `nPairable` and `method`.
#'
#'
#' @family assoc.agreement  
#' @concept agreement  
#' @concept categorical-agreement
#'
#'
#' @export
percAgreement <- function(x, y = NULL,
                          conf.level = NA,
                          sides = c("two.sided", "left", "right"),
                          input = c("auto", "confusion", "ratings"),
                          fpc = 0,
                          output = c("def", "ext"),
                          ...) {

  input  <- match.arg(input)
  sides  <- match.arg(sides)
  output <- match.arg(output)

  conf.level <- checkConfLevel(conf.level)

  if (sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  if (length(fpc) != 1L || !is.numeric(fpc) || is.na(fpc) || fpc < 0 || fpc >= 1)
    stop("'fpc' must be a single number in [0, 1).")

  # '...' was documented as reserved for future extensions, which in
  # practice means a misspelled argument disappeared without a word. There
  # is nothing here to pass anything on to.
  dots <- list(...)
  if (length(dots))
    stop(gettextf("unused argument(s): %s",
                  paste(sQuote(names(dots)[nzchar(names(dots))], FALSE),
                        collapse = ", ")), domain = NA)

  # --- resolve input ---
  tmp <- .resolveAgreementInput(x, y, input)
  x <- tmp$x
  input <- tmp$input
  
  # ===============================
  # CONFUSION MATRIX
  # ===============================
  if (input == "confusion") {
    
    if (!isConfusionTable(x)) {
      stop("Input does not appear to be a valid confusion matrix.")
    }
    
    n <- sum(x)
    a <- sum(diag(x))
    Po <- a / n
    
    var_hat <- (1 - fpc) / (n * (n - 1)) *
      sum(c(a, n - a) * (c(1, 0) - Po)^2)
    
    n0 <- NA_integer_
    
  } else {

    x <- .asRatingsMatrix(x)

    n <- nrow(x)
    
    poi <- apply(x, 1, function(row) {
      v <- row[!is.na(row)]
      m <- length(v)
      if (m < 2) return(NA_real_)
      tab <- table(v)
      sum(tab * (tab - 1)) / (m * (m - 1))
    })
    
    n0 <- sum(!is.na(poi))
    Po <- if (n0 > 0) mean(poi, na.rm = TRUE) else NA_real_
    
    ki <- numeric(n)
    if (n0 > 0) ki[!is.na(poi)] <- (n / n0) * poi[!is.na(poi)]
    
    var_hat <- (1 - fpc) / (n * (n - 1)) * sum((ki - Po)^2)
  }
  
  # ===============================
  # SE + CI
  # ===============================
  se <- if (is.na(Po) || n <= 1) NA_real_ else sqrt(var_hat)

  if (is.na(conf.level) || is.na(se)) {

    if (!is.na(conf.level) && !is.na(Po))
      warning("the design-based standard error is undefined here; ",
              "no interval computed", call. = FALSE)

    ci <- c(lci = NA_real_, uci = NA_real_)

  } else {

    # A one-sided bound at level gamma is the corresponding end of the
    # two-sided interval at level 2*gamma - 1.
    confAdj <- if (sides == "two.sided") conf.level else 2 * conf.level - 1
    tcrit   <- qt(1 - (1 - confAdj) / 2, df = n - 1)

    # a proportion of agreement lies in [0, 1] - applySides() clamps to
    # that range and closes the open side there
    ci <- applySides(Po + c(-1, 1) * tcrit * se, sides, lo = 0, hi = 1)
  }

  res <- c(est = Po, ci)

  if (output == "def")
    return(if (is.na(conf.level)) unname(Po) else res)

  list(
    est = Po,
    se = se,
    ci = res,
    n = n,
    nPairable = n0,
    method = "Percent agreement (design-based; Klein/Gwet)"
  )
}



# == internal helper functions ===============================================

# Was .resolveInput(x, y, input, fun) and reconstructed the default from
# formals(fun)$input[[1]] -- which is the symbol `c`, not "auto", because the
# default is the *call* c("auto","confusion","ratings"). The branch was
# unreachable from percAgreement() (a formal with an unused default carries
# missing-bit 2, which does not propagate to missing() in the callee), so it
# never fired; it would have broken the moment the helper was reused. The
# default is now resolved by match.arg() in the exported function and the
# helper only maps the resolved value. The name was also too generic for a
# top-level object in a package with ~200 files.
.resolveAgreementInput <- function(x, y, input) {

  # --- y supplied => force confusion matrix ---
  if (!is.null(y)) {
    x <- normalizeToConfusion(x, y)
    input <- "confusion"
  }

  # --- auto detection ---
  if (input == "auto") {
    input <- if (isConfusionTable(x)) "confusion" else "ratings"
  }

  list(x = x, input = input)
}
