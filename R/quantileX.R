
#' (Weighted) Sample Quantiles
#'
#' Compute sample quantiles, with optional weights.
#'
#' Without `weights` the call is handed to [stats::quantile()]
#' unchanged, so all nine types are available and the results are identical
#' to base R.
#'
#' With `weights` only types 5 and 7 exist, and they interpret the
#' weights **differently**:
#'
#' \describe{
#'   \item{`type = 5`}{treats them as relative weights: only the ratios
#'     matter, and multiplying every weight by a constant leaves the result
#'     unchanged. This follows the Eurostat definition (EU-SILC 131-rev/04).}
#'   \item{`type = 7`}{treats them as *frequency* weights, i.e. as
#'     replication counts. The effective sample size is `sum(weights)`,
#'     so the result is **not** scale-invariant, and weights that have
#'     been normalized to sum to 1 are degenerate - see below.}
#' }
#'
#' This difference is inherited from the two source implementations and is
#' not a free choice of the caller: it is worth knowing which of the two is
#' meant before picking a type. Because `type = 7` needs
#' `sum(weights)` to act as a sample size, it requires that sum to be
#' at least 2 and raises an error otherwise.
#' 
#' @param x a numeric vector
#' @param weights an optional numeric vector giving the sample weights
#' @param probs numeric vector of probabilities with values in \eqn{[0,1]}
#' @param na.rm a logical indicating whether missing values in `x` should
#' be omitted
#' @param names logical; if true, the result has a [names()]
#' attribute.  Set to `FALSE` for speedup with many `probs`.
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#' algorithms of [stats::quantile()]. All nine are available for
#' unweighted data. With `weights` only 5 and 7 (default) exist; any
#' other value is an error. See Details for how the two differ in their
#' reading of the weights.
#' @param digits used only when `names` is true: the precision to use when
#' formatting the percentages. In `R` versions up to 4.0.x, this had been
#' set to `max(2, getOption("digits"))`, internally.
#' @return a numeric vector containing the weighted quantiles of `x` at
#' probabilities `probs`, named when `names = TRUE`
#' 
#' @note Based on code by Andreas Alfons, Matthias Templ, 
#' adapted to conform to package standards.
#' 
#' @references Working group on Statistics on Income and Living Conditions
#' (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay
#' gap.  *EU-SILC 131-rev/04*, Eurostat.
#' 
#' @examples
#' # Pizza$temperature contains missing values, so na.rm is needed - without
#' # it the function returns NA for every prob, silently.
#' quantileX(Pizza$temperature, rep(c(1:3), length.out = nrow(Pizza)),
#'           na.rm = TRUE)
#'
#' x <- c(3.7, 3.3, 3.5, 2.8)
#'
#' # type 5 only looks at the ratios of the weights ...
#' quantileX(x, weights = c(5, 5, 4, 1),      type = 5)
#' quantileX(x, weights = c(5, 5, 4, 1) / 15, type = 5)   # identical
#'
#' # ... while type 7 reads them as replication counts, so they have to be
#' # on that scale
#' quantileX(x, weights = c(5, 5, 4, 1), type = 7)
#'
#' @seealso [medianX()], [stats::quantile()],
#' [lumen::quantileCI()]
#'
#' @family quantile
#' @concept quantile
#' @concept distribution-summary
#' @export
quantileX <- function(x, weights = NULL, probs = seq(0, 1, 0.25),
                     na.rm = FALSE, names=TRUE, type = 7, digits=7) {
  
  # further weighted quantiles in Hmisc and modi, both on CRAN
  
  if(is.null(weights)){
    stats::quantile(x=x, probs=probs, na.rm=na.rm, names=names,
                    type=type, digits=digits)
    
  } else {

    # Everything from here on runs with weights present - the outer if()
    # already dispatched the NULL case to stats::quantile(). The original
    # kept the is.null(weights) branches of laeken::weightedQuantile()
    # inside this arm, where none of them can be reached: the guard at
    # line 80, the reweighting at 104, `rw <- (1:n)/n`, and the whole
    # unweighted type-7 block. All removed.

    # initializations
    if (!is.numeric(x)) stop("'x' must be a numeric vector")

    n <- length(x)

    if (!is.numeric(weights)) stop("'weights' must be a numeric vector")
    if (length(weights) != n) stop("'weights' must have the same length as 'x'")
    if (!all(is.finite(weights))) stop("missing or infinite weights")

    # An error, not a warning: a negative weight makes cumsum(weights)
    # non-monotonic, and both branches below read it as an increasing
    # index - type 5 through `which(rw >= p)`, type 7 through approx().
    # The results are not merely imprecise, they are meaningless.
    if (any(weights < 0)) stop("'weights' must not be negative")

    if (!is.numeric(probs) || all(is.na(probs)) ||
        isTRUE(any(probs < 0 | probs > 1)))
      stop("'probs' must be a numeric vector with values in [0,1]")

    qNames <- NULL
    if (names && length(probs) > 0L) {
      stopifnot(is.numeric(digits), digits >= 1)
      qNames <- names(stats::quantile(
        0, probs=probs, names=TRUE, type=1, digits=digits
      ))
    }

    if (n == 0 || (!isTRUE(na.rm) && any(is.na(x)))) {
      # zero length or missing values. NA_real_ rather than the logical
      # NA, and named like every other return of this function.
      qs <- rep.int(NA_real_, length(probs))
      if (!is.null(qNames)) names(qs) <- qNames
      return(qs)
    }

    if (all(weights == 0)) {
      # The former version returned rep.int(0, length(probs)) here - a
      # fabricated zero that has nothing to do with the data and reads
      # like a legitimate quantile. Undefined is NA.
      warning("all weights equal to zero")
      qs <- rep.int(NA_real_, length(probs))
      if (!is.null(qNames)) names(qs) <- qNames
      return(qs)
    }

    # remove NAs (if requested)
    if(isTRUE(na.rm)){
      indices <- !is.na(x)
      x <- x[indices]
      weights <- weights[indices]
      n <- length(x)
    }

    # sort values and weights
    order <- order(x)
    x <- x[order]
    weights <- weights[order]

    # Drop zero-weight observations. They contribute nothing by definition,
    # but they leave a repeated value in cumsum(weights), and approx() in
    # the type-7 branch below reacts to tied x-values by collapsing them
    # and averaging the corresponding y - with a warning about something
    # the caller did not do.
    if (any(weights == 0)) {
      keep <- weights > 0
      x <- x[keep]
      weights <- weights[keep]
      n <- length(x)
    }

    rw <- cumsum(weights)/sum(weights)
    
    # obtain quantiles
    if (type == 5) {
      qs <- sapply(probs,
                   function(p) {
                     if (p == 0) return(x[1])
                     else if (p == 1) return(x[n])
                     select <- min(which(rw >= p))
                     if(rw[select] == p) mean(x[select:(select+1)])
                     else x[select]
                   })
      
    } else if(type == 7){

      # This branch reads the weights as REPLICATION COUNTS: the sum takes
      # the place of the sample size, and cumsum(weights) is used as an
      # index into the order statistics. It is therefore not scale
      # invariant, unlike type 5 above.
      #
      # The degenerate case is worth naming, because it is the natural
      # thing to pass. With weights normalized to sum to 1, sumW is 1 and
      #
      #     ord = 1 + (sumW - 1) * probs = 1   for EVERY prob
      #
      # so every quantile collapses onto the largest observation and, for
      # instance, iqrX() returns 0. That is exactly what the documented
      # example of iqrX did: w <- c(5, 5, 4, 1)/15 gave an IQR of 0, while
      # the same weights unnormalized give 0.4. Silent, and plausible
      # enough to go unnoticed.
      sumW <- sum(weights)

      if (sumW < 2)
        stop(gettextf(
          paste("type = 7 reads 'weights' as replication counts, so their sum",
                "(%g) must be at least 2. Rescale them to counts, or use",
                "type = 5, which depends only on their ratios."),
          sumW), domain = NA)

      ord   <- 1 + (sumW - 1) * probs
      low   <- pmax(floor(ord), 1)
      high  <- pmin(low + 1, sumW)
      ord   <- ord %% 1
      ## Find low and high order statistics
      ## These are minimum values of x such that the cum. freqs >= c(low,high)
      allq <- approx(cumsum(weights), x, xout=c(low, high),
                     method='constant', f=1, rule=2)$y
      k <- length(probs)
      qs <- (1 - ord)*allq[1:k] + ord*allq[-(1:k)]

    } else {
      # was: qs <- NA plus a warning. qs then had length 1 while probs had
      # length k, so the names<- below failed with "'names' attribute [k]
      # must be the same length as the vector [1]" - an error, but about
      # the wrong thing and only after the warning.
      stop(gettextf(
        "type = %s is not implemented for weighted quantiles; use 5 or 7",
        type), domain = NA)
    }
    
    if (!is.null(qNames)) names(qs) <- qNames
    
    return(qs)
    
  }
}


