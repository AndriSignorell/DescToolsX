
#' Price and Quantity Indices
#'
#' Bilateral index numbers comparing prices (or quantities) of a set of items
#' between a base period 0 and a comparison period 1. The unweighted
#' (elementary) indices need prices only, the weighted ones use quantities to
#' weight the items by their economic importance.
#'
#' All vectors are matched by position: element `i` of `p0`, `p1`, `q0` and
#' `q1` must refer to the same item. For panels in long format, with items
#' entering and leaving over time, see [chainedIndex()].
#'
#' Writing \eqn{r_i = p_{i1}/p_{i0}} for the price relatives and
#' \eqn{s_{it} = p_{it} q_{it} / \sum_j p_{jt} q_{jt}} for the expenditure
#' shares, the estimators are:
#' \describe{
#'   \item{`"laspeyres"`}{\eqn{\sum p_1 q_0 / \sum p_0 q_0}, base period
#'     basket}
#'   \item{`"paasche"`}{\eqn{\sum p_1 q_1 / \sum p_0 q_1}, comparison period
#'     basket}
#'   \item{`"fisher"`}{geometric mean of Laspeyres and Paasche}
#'   \item{`"drobisch"`}{arithmetic mean of Laspeyres and Paasche}
#'   \item{`"marshall-edgeworth"`}{basket \eqn{q_0 + q_1}}
#'   \item{`"walsh"`}{basket \eqn{\sqrt{q_0 q_1}}}
#'   \item{`"tornqvist"`}{\eqn{\prod r_i^{(s_{i0} + s_{i1})/2}}}
#'   \item{`"sato-vartia"`}{\eqn{\prod r_i^{w_i}} with weights proportional
#'     to the logarithmic mean of \eqn{s_{i0}} and \eqn{s_{i1}}}
#'   \item{`"geo-laspeyres"`}{\eqn{\prod r_i^{s_{i0}}}}
#'   \item{`"geo-paasche"`}{\eqn{\prod r_i^{s_{i1}}}}
#'   \item{`"palgrave"`}{\eqn{\sum s_{i1} r_i}}
#'   \item{`"jevons"`}{unweighted geometric mean of the price relatives}
#'   \item{`"carli"`}{unweighted arithmetic mean of the price relatives}
#'   \item{`"dutot"`}{ratio of the unweighted mean prices,
#'     \eqn{\sum p_1 / \sum p_0}}
#' }
#'
#' Fisher, Walsh and Törnqvist are superlative indices (Diewert, 1976) and
#' usually agree closely; Laspeyres and Paasche bracket them in the typical
#' case of substitution towards items that became relatively cheaper. Fisher
#' and Sato-Vartia satisfy the factor reversal test: the product of the price
#' and the quantity index equals the ratio of total expenditure.
#'
#' All weighted indices are invariant to the scale of the quantities, i.e.
#' to the size of the population the quantities were observed in. With the
#' exception of `"marshall-edgeworth"` this also holds if the scale differs
#' between the two periods.
#'
#' Estimators that use only one basket need only that basket:
#' `"laspeyres"` and `"geo-laspeyres"` work without `q1`, `"paasche"`,
#' `"geo-paasche"` and `"palgrave"` without `q0`. Quantities supplied but not
#' used by the chosen estimator are ignored.
#'
#' `quantityIndex()` is the same set of formulas with the roles of prices and
#' quantities interchanged.
#'
#' @param p0,p1 numeric vectors of prices in the base and in the comparison
#'   period. Must be non-negative, and strictly positive for estimators built
#'   on price relatives.
#' @param q0,q1 numeric vectors of non-negative quantities in the base and in
#'   the comparison period, or `NULL` where the estimator does not need them
#' @param estimator the index formula, see Details
#' @param na.rm logical, should items with a missing price or quantity be
#'   removed? Defaults to `FALSE`, in which case the result is `NA` as soon as
#'   one value used by the estimator is missing.
#'
#' @return A single unnamed numeric value, the index as a ratio (1 = no
#'   change). `NA` if no items are left or if the index is not defined, e.g.
#'   for a basket without any expenditure.
#'
#' @references
#' Balk, B. M. (2008). *Price and Quantity Index Numbers: Models for Measuring
#' Aggregate Change and Difference*. Cambridge University Press.
#'
#' Diewert, W. E. (1976). Exact and superlative index numbers. *Journal of
#' Econometrics*, 4(2), 115--145.
#'
#' ILO, IMF, OECD, UNECE, Eurostat and World Bank (2004). *Consumer Price
#' Index Manual: Theory and Practice*. International Labour Office, Geneva.
#'
#' @seealso [chainedIndex()] for chained indices over several periods
#'
#' @examples
#' p0 <- c(2.0, 5.0, 10.0)
#' p1 <- c(2.2, 5.5, 9.0)
#' q0 <- c(100, 40, 10)
#' q1 <- c(90, 45, 14)
#'
#' priceIndex(p0, p1, q0)
#' ## [1] 1.06
#' priceIndex(p0, p1, q0, q1, estimator = "paasche")
#' ## [1] 1.048624
#' priceIndex(p0, p1, q0, q1, estimator = "tornqvist")
#' ## [1] 1.054558
#' priceIndex(p0, p1, estimator = "jevons")
#' ## [1] 1.028828
#'
#' # factor reversal: price index x quantity index = expenditure ratio
#' priceIndex(p0, p1, q0, q1, estimator = "fisher") *
#'   quantityIndex(q0, q1, p0, p1, estimator = "fisher")
#' ## [1] 1.143
#' sum(p1 * q1) / sum(p0 * q0)
#' ## [1] 1.143
#'
#' @family index.price
#' @concept price-index
#' @concept index-number
#' @export
priceIndex <- function(p0, p1, q0 = NULL, q1 = NULL,
                       estimator = c("laspeyres", "paasche", "fisher",
                                     "tornqvist", "walsh",
                                     "marshall-edgeworth", "drobisch",
                                     "sato-vartia", "geo-laspeyres",
                                     "geo-paasche", "palgrave",
                                     "jevons", "carli", "dutot"),
                       na.rm = FALSE) {

  estimator <- match.arg(estimator)
  .indexDispatch(p0, p1, q0, q1, estimator = estimator, na.rm = na.rm,
                 argNames = c("p0", "p1", "q0", "q1"))

}


#' @rdname priceIndex
#' @export
quantityIndex <- function(q0, q1, p0 = NULL, p1 = NULL,
                          estimator = c("laspeyres", "paasche", "fisher",
                                        "tornqvist", "walsh",
                                        "marshall-edgeworth", "drobisch",
                                        "sato-vartia", "geo-laspeyres",
                                        "geo-paasche", "palgrave",
                                        "jevons", "carli", "dutot"),
                          na.rm = FALSE) {

  estimator <- match.arg(estimator)
  .indexDispatch(q0, q1, p0, p1, estimator = estimator, na.rm = na.rm,
                 argNames = c("q0", "q1", "p0", "p1"))

}


#' Chained Price Index
#'
#' Price index over several periods for a panel of items in long format. For
#' every pair of adjacent periods a bilateral index is computed on the items
#' observed in both periods (matched items), and these links are multiplied
#' up to the chained index. Items may thus enter and leave the basket over
#' time, and the weights are updated with every link.
#'
#' The periods are taken in the sort order of `period` (level order for a
#' factor). A link between two periods without any common item is not
#' defined; it is returned as `NA` with a warning, and so is the index beyond
#' that break as seen from the base period.
#'
#' New and disappearing items do not contribute to the link in which they
#' enter or leave. Whether an item is the same over time is decided by `id`
#' alone, so the choice of the item level matters: at the level of the single
#' package a switch from an original to a cheaper generic is a change of the
#' basket, at the level of the substance (with unit values as prices) it is a
#' price decrease.
#'
#' @param p numeric vector of prices, one value per item and period
#' @param id vector identifying the item of each price
#' @param period vector identifying the period of each price, of any sortable
#'   type (numeric, Date, factor, ...)
#' @param q numeric vector of quantities belonging to `p`, or `NULL` for the
#'   unweighted estimators
#' @param estimator the index formula used for the links, see [priceIndex()]
#' @param base the period in which the index is set to 1. Defaults to the
#'   first period.
#' @param na.rm logical, should records with a missing price or quantity be
#'   removed? Defaults to `FALSE`, in which case a link involving a missing
#'   value is `NA`.
#'
#' @return A data frame with one row per period and the columns
#'   \describe{
#'     \item{`period`}{the period}
#'     \item{`index`}{the chained index, 1 in the base period}
#'     \item{`link`}{the bilateral index against the previous period, `NA` in
#'       the first period}
#'     \item{`n`}{the number of matched items the link is based on}
#'   }
#'
#' @inherit priceIndex references
#'
#' @seealso [priceIndex()] for the bilateral indices and their formulas
#'
#' @examples
#' # item "d" enters in 2021, item "c" leaves after 2021
#' d <- data.frame(
#'   year = c(2020, 2020, 2020, 2021, 2021, 2021, 2021, 2022, 2022, 2022),
#'   id   = c("a", "b", "c", "a", "b", "c", "d", "a", "b", "d"),
#'   p    = c(2.0, 5.0, 10.0, 2.2, 5.5, 9.0, 20.0, 2.1, 5.6, 16.0),
#'   q    = c(100, 40, 10, 90, 45, 14, 5, 95, 40, 12)
#' )
#'
#' chainedIndex(d$p, d$id, d$year, d$q, estimator = "tornqvist")
#' ##   period    index      link  n
#' ## 1   2020 1.000000        NA NA
#' ## 2   2021 1.054558 1.0545582  3
#' ## 3   2022 0.989307 0.9381246  3
#'
#' # unweighted, rebased to 2022
#' chainedIndex(d$p, d$id, d$year, estimator = "jevons", base = 2022)
#'
#' @family index.price
#' @concept price-index
#' @concept index-number
#' @concept time-series
#' @export
chainedIndex <- function(p, id, period, q = NULL,
                         estimator = c("laspeyres", "paasche", "fisher",
                                       "tornqvist", "walsh",
                                       "marshall-edgeworth", "drobisch",
                                       "sato-vartia", "geo-laspeyres",
                                       "geo-paasche", "palgrave",
                                       "jevons", "carli", "dutot"),
                         base = NULL, na.rm = FALSE) {

  estimator <- match.arg(estimator)

  n <- length(p)
  if (length(id) != n || length(period) != n)
    stop("'p', 'id' and 'period' must have the same length")
  if (anyNA(id) || anyNA(period))
    stop("'id' and 'period' must not contain missing values")

  periods <- sort(unique(period))
  k <- match(period, periods)
  if (anyDuplicated(data.frame(id, k)))
    stop("'id' must be unique within each 'period'")

  nPeriods <- length(periods)
  if (is.null(base)) {
    b <- 1L
  } else {
    b <- if (length(base) == 1L) match(base, periods) else NA_integer_
    if (is.na(b))
      stop("'base' must be a single value of 'period'")
  }

  # validates the arguments even if there is no link to compute
  .indexDispatch(p, p, q, q, estimator = estimator, na.rm = TRUE,
                 argNames = c("p", "p", "q", "q"))

  rows <- split(seq_len(n), factor(k, levels = seq_len(nPeriods)))
  link <- rep(NA_real_, nPeriods)
  nMatched <- rep(NA_integer_, nPeriods)

  for (j in seq_len(nPeriods)[-1L]) {

    i1 <- rows[[j]]
    m <- match(id[i1], id[rows[[j - 1L]]])
    i1 <- i1[!is.na(m)]
    i0 <- rows[[j - 1L]][m[!is.na(m)]]

    nMatched[j] <- length(i1)
    if (nMatched[j] == 0L) {
      warning(sprintf("no matched items between periods %s and %s, link is NA",
                      format(periods[j - 1L]), format(periods[j])))
    } else {
      link[j] <- .indexDispatch(p[i0], p[i1], q[i0], q[i1],
                                estimator = estimator, na.rm = na.rm,
                                argNames = c("p", "p", "q", "q"))
    }

  }

  # chain forward and backward from the base period, so that a missing link
  # only affects the periods beyond it
  index <- rep(1, nPeriods)
  if (b < nPeriods) {
    fwd <- seq(b + 1L, nPeriods)
    index[fwd] <- cumprod(link[fwd])
  }
  if (b > 1L) {
    bwd <- seq(b, 2L)
    index[bwd - 1L] <- 1 / cumprod(link[bwd])
  }

  data.frame(period = periods, index = index, link = link, n = nMatched)

}


# == internal helpers ==========================================================

# estimator properties: which weights are needed, and whether the formula is
# built on relatives x1/x0 (requiring strictly positive x)
.indexUnweighted <- c("jevons", "carli", "dutot")
.indexBaseOnly   <- c("laspeyres", "geo-laspeyres")
.indexCompOnly   <- c("paasche", "geo-paasche", "palgrave")
.indexRelative   <- c("jevons", "carli", "tornqvist", "sato-vartia",
                      "geo-laspeyres", "geo-paasche", "palgrave")


# Validates the input, applies the NA rule and calls the formula.
# x0, x1: the variable the index is about; w0, w1: the weighting variable.
# argNames: names of (x0, x1, w0, w1) in the calling function, for messages.
#' @noRd
.indexDispatch <- function(x0, x1, w0, w1, estimator, na.rm, argNames) {

  if (!isTRUE(na.rm) && !isFALSE(na.rm))
    stop("'na.rm' must be TRUE or FALSE")

  quoteNames <- function(i)
    paste(sprintf("'%s'", unique(argNames[i])), collapse = " and ")

  if (!is.numeric(x0) || !is.numeric(x1) || length(x0) != length(x1))
    stop(quoteNames(1:2), " must be numeric and of the same length")

  # weights the estimator does not use are dropped, missing ones are an error
  use <- c(!estimator %in% c(.indexUnweighted, .indexCompOnly),
           !estimator %in% c(.indexUnweighted, .indexBaseOnly))
  if (!use[1L]) w0 <- NULL
  if (!use[2L]) w1 <- NULL
  if ((use[1L] && is.null(w0)) || (use[2L] && is.null(w1)))
    stop(sprintf("estimator = \"%s\" needs %s",
                 estimator, quoteNames(c(3L, 4L)[use])))

  for (w in list(w0, w1)) {
    if (!is.null(w) && (!is.numeric(w) || length(w) != length(x0)))
      stop(quoteNames(c(3L, 4L)[use]), " must be numeric and of the same ",
           "length as ", quoteNames(1:2))
  }

  # range checks come before any NA return
  x <- c(x0, x1)
  w <- c(w0, w1)
  if (any(is.infinite(x)) || any(is.infinite(w)))
    stop(quoteNames(1:4), " must be finite")
  if (estimator %in% .indexRelative) {
    if (any(x <= 0, na.rm = TRUE))
      stop(sprintf("%s must be positive for estimator = \"%s\"",
                   quoteNames(1:2), estimator))
  } else if (any(x < 0, na.rm = TRUE)) {
    stop(quoteNames(1:2), " must not be negative")
  }
  if (any(w < 0, na.rm = TRUE))
    stop(quoteNames(c(3L, 4L)[use]), " must not be negative")

  # paired data: an item is complete only if all its values are
  ok <- !(is.na(x0) | is.na(x1))
  if (!is.null(w0)) ok <- ok & !is.na(w0)
  if (!is.null(w1)) ok <- ok & !is.na(w1)

  if (!all(ok)) {
    if (!na.rm) return(NA_real_)
    x0 <- x0[ok]
    x1 <- x1[ok]
    w0 <- w0[ok]
    w1 <- w1[ok]
  }
  if (length(x0) == 0L) return(NA_real_)

  res <- .bilateralIndex(x0, x1, w0, w1, estimator)
  if (is.finite(res)) res else NA_real_

}


# The formulas, on clean and complete input.
#' @noRd
.bilateralIndex <- function(x0, x1, w0, w1, estimator) {

  # fixed basket index with basket w
  basket <- function(w) sum(x1 * w) / sum(x0 * w)
  # value shares in period 0 and 1
  share0 <- function() x0 * w0 / sum(x0 * w0)
  share1 <- function() x1 * w1 / sum(x1 * w1)
  # weighted geometric mean of the relatives
  geoMean <- function(w) exp(sum(w * log(x1 / x0)) / sum(w))

  switch(estimator,
    "laspeyres"          = basket(w0),
    "paasche"            = basket(w1),
    "fisher"             = sqrt(basket(w0) * basket(w1)),
    "drobisch"           = (basket(w0) + basket(w1)) / 2,
    "marshall-edgeworth" = basket(w0 + w1),
    "walsh"              = basket(sqrt(w0 * w1)),
    "tornqvist"          = geoMean(share0() + share1()),
    "sato-vartia"        = geoMean(.logMean(share0(), share1())),
    "geo-laspeyres"      = geoMean(share0()),
    "geo-paasche"        = geoMean(share1()),
    "palgrave"           = sum(share1() * x1 / x0),
    "jevons"             = geoMean(rep(1, length(x0))),
    "carli"              = mean(x1 / x0),
    "dutot"              = sum(x1) / sum(x0)
  )

}


# Logarithmic mean (b - a) / (log(b) - log(a)), with the limits a for a == b
# and 0 if either argument is 0.
#' @noRd
.logMean <- function(a, b) {

  res <- (b - a) / (log(b) - log(a))
  res[a == b] <- a[a == b]
  res

}
