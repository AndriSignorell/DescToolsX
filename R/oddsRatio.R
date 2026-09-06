
#' Compute Odds Ratios
#'
#' Computes odds ratios, either from a 2x2 contingency table or from a
#' binomial generalized linear model. The table method returns a single odds
#' ratio, the model method one odds ratio per regression coefficient.
#'
#' @details
#' \subsection{Contingency tables}{
#' For a 2x2 table the odds ratio is
#'
#' \deqn{OR = \frac{n_{11} n_{22}}{n_{12} n_{21}}}
#'
#' Three interval methods are available. `"wald"` is the asymptotic
#' interval on the log scale, fast and adequate for reasonably large counts.
#' `"exact"` is the conditional interval based on the noncentral
#' hypergeometric distribution (Fisher); it guarantees coverage but is
#' conservative, sometimes markedly so. `"midp"` halves the probability
#' of the observed table and lies between the two: it has coverage closer to
#' the nominal level than the exact interval without the Wald interval's
#' reliance on large counts. With a zero cell the point estimate is 0 or
#' `Inf` and only `"exact"` and `"midp"` still deliver a
#' finite bound on the informative side.
#' }
#'
#' \subsection{Binomial models}{
#' For a model fitted with `glm(family = binomial)`, each coefficient is
#' exponentiated: \eqn{\exp(\beta_j)} is the factor by which the odds of the
#' response are multiplied when the corresponding predictor increases by one
#' unit, all other predictors held fixed. For a dummy variable this is the
#' odds ratio between the level and its reference level.
#'
#' The intercept is exponentiated along with the rest, but
#' \eqn{\exp(\beta_0)} is *not* an odds ratio - it is the odds of the
#' response when all predictors are zero. It is reported for completeness
#' and is usually not the quantity of interest.
#'
#' Two interval methods are available. `"wald"` is the symmetric
#' interval on the log-odds scale, back-transformed. `"profile"` inverts
#' the likelihood ratio test through [stats::confint.glm()]; it is
#' asymmetric on the odds scale, generally more reliable in small samples or
#' with sparse cells, and considerably slower because the model is refitted
#' along each coefficient. Profile intervals are two-sided by construction,
#' so `sides` is ignored for them and a warning is issued.
#'
#' Unlike the table method, the model method computes an interval by default
#' (`conf.level = 0.95`): a coefficient table without intervals would
#' be less informative than `summary()` itself.
#' }
#'
#' @param x a 2x2 contingency table, two vectors to be cross-tabulated, or a
#'   binomial [stats::glm()] object
#' @param ... further arguments passed to methods. For the default method
#'   with two vectors, these reach [table()], so `useNA` can
#'   be set here.
#'
#' @return
#' For a contingency table with `conf.level = NA` a numeric scalar,
#' otherwise a named numeric vector with the elements `est`, `lci`
#' and `uci`.
#'
#' For a binomial model an object of class `"OddsRatio"`, a list with:
#' \describe{
#'   \item{`coefficients`}{a data frame with one row per coefficient and
#'     the columns `term`, `est` (the exponentiated coefficient),
#'     `logEst` (the coefficient itself), `stdError` (on the log
#'     scale), `pValue`, `lci` and `uci`}
#'   \item{`source`}{`"glm"`}
#'   \item{`method`, `conf.level`, `sides`}{as supplied - with
#'     `sides` recording what was computed, which for
#'     `method = "profile"` is always `"two.sided"`}
#'   \item{`nObs`}{number of observations used in the fit}
#'   \item{`call`}{the model call}
#' }
#' There is a `print` method; the interval bounds are on the odds scale,
#' the standard error on the log scale.
#'
#' @references
#' Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.).
#' Wiley.
#'
#' Fisher, R. A. (1935). The logic of inductive inference.
#' *Journal of the Royal Statistical Society*,
#' *98*(1), 39--82.
#'
#' Gart, J. J. (1966). Alternative analyses of contingency tables.
#' *Journal of the Royal Statistical Society Series B*,
#' *28*(1), 164--179.
#'
#' @note
#' No short alias is exported by default to avoid conflicts with
#' \pkg{rlang} and base R naming conventions.  Call
#' [attachAliases()] once per session (or script) to make
#' `or()` available as a convenient shorthand.
#'
#' @seealso [attachAliases], [relRisk], [stats::confint.glm()]
#'
#' @examples
#' # --- 2x2 table -------------------------------------------------
#' tab <- matrix(c(10, 20,
#'                  5, 30), nrow = 2)
#'
#' oddsRatio(tab)
#' oddsRatio(tab, conf.level = 0.95)
#'
#' # the exact interval is the widest, the Wald interval the narrowest
#' sapply(c("wald", "exact", "midp"),
#'        function(m) oddsRatio(tab, conf.level = 0.95, method = m))
#'
#' # one-sided: "left" carries the finite lower bound
#' oddsRatio(tab, conf.level = 0.95, sides = "left")
#'
#'
#' # --- binomial model --------------------------------------------
#' fit <- glm(vs ~ am + wt, data = mtcars, family = binomial)
#'
#' oddsRatio(fit)
#'
#' # the exponentiated intercept is the baseline odds, not an odds ratio
#' res <- oddsRatio(fit)
#' res$coefficients
#'
#' # profile likelihood intervals: asymmetric on the odds scale, slower
#' oddsRatio(fit, method = "profile")
#'
#'
#' @family effect.size
#' @concept effect-size
#' @concept binary-outcome
#' @export
oddsRatio <- function(x, ...) {
  UseMethod("oddsRatio")
}



#' @param y optional second variable. If supplied,
#'   `table(x, y, ...)` is computed.
#' @param conf.level confidence level of the interval. For the table method
#'   `NA` (the default) returns the point estimate only; the model
#'   method computes an interval by default.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of `"two.sided"` (default), `"left"` or
#'   `"right"`). See [ConfidenceIntervals()]. An odds ratio
#'   is bounded below by 0 and unbounded above, so the open side is reported
#'   at 0 or `Inf` accordingly. Ignored, with a warning, for
#'   `method = "profile"`.
#' @param method character string specifying the interval method. For a
#'   contingency table one of `"wald"`, `"exact"` or
#'   `"midp"`; for a binomial model one of `"wald"` or
#'   `"profile"`. See Details.
#' @param interval numeric vector of length two giving the search interval
#'   for the root finding in the mid-p method. Only used by
#'   `method = "midp"`; widen it if the reported bound sits at one of
#'   its ends.
#'
#' @rdname oddsRatio
#' @method oddsRatio default
#' @export
oddsRatio.default <- function(
    x,
    y = NULL,
    conf.level = NA,
    sides = c("two.sided", "left", "right"),
    method = c("wald", "exact", "midp"),
    interval = c(0, 1000),
    ...
) {
  
  # All argument checks up front, none behind a branch.
  method <- match.arg(method)
  sides  <- match.arg(sides)

  conf.level <- checkConfLevel(conf.level)

  if (sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  # A fitted model reaches this method whenever there is no oddsRatio
  # method for its class - an lm, say. Without this it fails on
  # !is.numeric(x) with "Argument 'x' must be numeric", which sends the
  # caller looking at the wrong end of the problem.
  if (inherits(x, c("lm", "glm", "merMod", "gam")))
    stop(gettextf(
      "odds ratios are computed from a 2x2 table or a binomial glm, not from an object of class %s",
      dQuote(class(x)[1L], FALSE)), domain = NA)

  if (!is.null(y))
    x <- table(x, y, ...)
  
  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")
  
  if (anyNA(x))
    stop("Argument 'x' must not contain missing values.")
  
  if (length(dim(x)) != 2L)
    stop("Argument 'x' must be a matrix.")
  
  if (!all(dim(x) == c(2L, 2L)))
    stop("Argument 'x' must be a 2x2 matrix.")
  
  if (any(x < 0))
    stop("Argument 'x' must contain non-negative counts.")
  
  if (any(x %% 1 != 0))
    stop("Argument 'x' must contain integer counts.")
  
  if (any(rowSums(x) == 0))
    stop("Rows of 'x' must contain positive totals.")

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1. All three methods build their
  # two-sided interval by inverting two one-sided constructions at
  # alpha/2, so reading one end off the adjusted interval gives exactly
  # the one-sided bound - and the helpers no longer need to know 'sides'
  # at all.
  #
  # That removes two defects they had: the exact method mapped
  # sides = "left" to alternative = "less", which is fisher.test's
  # interval with a finite UPPER bound and therefore the wrong end; and
  # the mid-p method ignored 'sides' outright and always returned the
  # two-sided interval.
  confAdj <- if (sides == "two.sided") conf.level else 2 * conf.level - 1

  res <- switch(
    method,
    
    "wald" = .oddsRatioWald(
      x = x,
      conf.level = confAdj
    ),
    
    "exact" = .oddsRatioExact(
      x = x,
      conf.level = confAdj
    ),
    
    "midp" = .oddsRatioMidP(
      x = x,
      conf.level = confAdj,
      interval = interval
    )
  )

  if (is.na(conf.level))
    return(res)

  # an odds ratio is bounded below by 0 and unbounded above
  c(est = unname(res[["est"]]),
    applySides(unname(res[c("lci", "uci")]), sides, lo = 0, hi = Inf))
  
}



#' @rdname oddsRatio
#' @method oddsRatio glm
#' @export
oddsRatio.glm <- function(
    x,
    conf.level = 0.95,
    sides = c("two.sided", "left", "right"),
    method = c("wald", "profile"),
    ...
) {
  
  method <- match.arg(method)
  sides  <- match.arg(sides)

  conf.level <- checkConfLevel(conf.level)

  if (sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)
  
  if (!inherits(x, "glm"))
    stop("Object must inherit from class 'glm'.")
  
  if (family(x)$family != "binomial")
    stop("Model must use binomial family.")
  
  coefTable <- summary(x)$coefficients
  
  beta <- coefTable[, "Estimate"]
  se   <- coefTable[, "Std. Error"]
  pval <- coefTable[, "Pr(>|z|)"]
  
  est <- exp(beta)

  if (is.na(conf.level)) {

    # the object always carries lci/uci columns, so they are filled with
    # NA rather than dropped - a caller reading $coefficients$lci should
    # not have to test for the column's existence
    lci <- rep(NA_real_, length(beta))
    uci <- rep(NA_real_, length(beta))

  } else if (method == "wald") {

    # one adjusted level instead of three branches; the numbers are the
    # same, since qnorm(1 - (1 - (2*gamma - 1))/2) == qnorm(gamma)
    confAdj <- if (sides == "two.sided") conf.level else 2 * conf.level - 1
    z       <- qnorm(1 - (1 - confAdj) / 2)

    lci <- exp(beta - z * se)
    uci <- exp(beta + z * se)

    # per coefficient, so that a model with a single term does not fall
    # through to a length-2 vector
    bounds <- vapply(seq_along(lci),
                     function(i) applySides(c(lci[i], uci[i]), sides,
                                             lo = 0, hi = Inf),
                     numeric(2))
    lci <- bounds[1L, ]
    uci <- bounds[2L, ]

  } else {

    if (sides != "two.sided") {
      warning("profile likelihood intervals are always two-sided; ",
              "'sides' is ignored", call. = FALSE)
      # recorded as what was actually computed, so that print() does not
      # announce a one-sided interval it did not produce
      sides <- "two.sided"
    }
    
    ci <- exp(
      confint(
        x,
        level = conf.level
      )
    )
    
    ci <- ci[rownames(coefTable), , drop = FALSE]
    
    lci <- ci[, 1]
    uci <- ci[, 2]
    
  }
  
  coefficients <- data.frame(
    term = rownames(coefTable),
    est = est,
    logEst = beta,
    stdError = se,
    pValue = pval,
    lci = lci,
    uci = uci,
    row.names = NULL
  )
  
  res <- list(
    coefficients = coefficients,
    source = "glm",
    method = method,
    conf.level = conf.level,
    sides = sides,
    nObs = nobs(x),
    call = x$call
  )
  
  class(res) <- "OddsRatio"
  
  res
  
}


#' @rdname oddsRatio
#' @param digits number of digits used for printing
#' @export
print.OddsRatio <- function(x, digits = 3, ...) {
  
  cat("\nCall:\n")
  print(x$call)
  
  cat(
    "\nOdds Ratios (",
    x$conf.level * 100,
    "% ",
    x$sides,
    " CI, method = ",
    x$method,
    "):\n\n",
    sep = ""
  )
  
  tab <- x$coefficients
  
  tabPrint <- data.frame(
    est = round(tab$est, digits),
    lci = round(tab$lci, digits),
    uci = round(tab$uci, digits),
    pValue = signif(tab$pValue, digits)
  )
  
  rownames(tabPrint) <- tab$term
  
  print(tabPrint)
  
  cat("\n")
  
  invisible(x)
  
}



# == internal helper functions ==============================================

# The helpers return a TWO-SIDED interval at the level they are given.
# Opening the relevant side is the caller's job, in one place, via
# applySides() - see oddsRatio.default().
.oddsRatioWald <- function(
    x,
    conf.level
) {
  
  if (any(x == 0))
    x <- x + 0.5
  
  logEst <- (
    log(x[1, 1]) +
      log(x[2, 2]) -
      log(x[1, 2]) -
      log(x[2, 1])
  )
  
  est <- exp(logEst)
  
  if (is.na(conf.level))
    return(est)
  
  se <- sqrt(sum(1 / x))
  
  z <- qnorm(1 - (1 - conf.level) / 2)
  
  c(
    est = est,
    lci = exp(logEst - z * se),
    uci = exp(logEst + z * se)
  )
  
}



.oddsRatioExact <- function(
    x,
    conf.level
) {
  
  # always two.sided: fisher.test() builds it by inverting two one-sided
  # tests at alpha/2, so the caller reads the one-sided bound off the
  # interval at the adjusted level. The former switch() mapped
  # sides = "left" to alternative = "less" - whose interval has a finite
  # UPPER bound - and thus returned the wrong end.
  fit <- fisher.test(
    x,
    conf.int = !is.na(conf.level),
    conf.level = if (is.na(conf.level)) 0.95 else conf.level,
    alternative = "two.sided"
  )
  
  est <- unname(fit$estimate)
  
  if (is.na(conf.level))
    return(est)
  
  c(
    est = est,
    lci = fit$conf.int[1],
    uci = fit$conf.int[2]
  )
  
}



.oddsRatioMidP <- function(
    x,
    conf.level,
    interval
) {
  
  a1 <- x[1, 1]
  a0 <- x[1, 2]
  b1 <- x[2, 1]
  b0 <- x[2, 2]
  
  .mue <- function(or) {
    
    mm <- matrix(
      c(a1, a0, b1, b0),
      nrow = 2,
      byrow = TRUE
    )
    
    fisher.test(
      mm,
      or = or,
      alternative = "less"
    )$p.value -
      fisher.test(
        mm,
        or = or,
        alternative = "greater"
      )$p.value
    
  }
  
  .midp <- function(or) {
    
    mm <- matrix(
      c(a1, a0, b1, b0),
      nrow = 2,
      byrow = TRUE
    )
    
    pLower <- fisher.test(
      mm,
      or = or,
      alternative = "less"
    )$p.value
    
    pUpper <- fisher.test(
      mm,
      or = or,
      alternative = "greater"
    )$p.value
    
    0.5 * (pLower - pUpper + 1)
    
  }
  
  est <- uniroot(
    .mue,
    interval = interval
  )$root
  
  if (is.na(conf.level))
    return(est)
  
  alpha <- 1 - conf.level
  
  lci <- uniroot(
    function(or) {
      1 - .midp(or) - alpha / 2
    },
    interval = interval
  )$root
  
  uci <- 1 / uniroot(
    function(or) {
      .midp(1 / or) - alpha / 2
    },
    interval = interval
  )$root
  
  c(
    est = est,
    lci = lci,
    uci = uci
  )
  
}
