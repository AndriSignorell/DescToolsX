
#' Cronbach's Coefficient Alpha
#'
#' Cronbach's alpha is a measure of internal consistency and often used for
#' validating psychometric tests. The unstandardized form implemented here
#' is computed from the item variances and the variance of the total score,
#' expressing the proportion of total-score variance not attributable to
#' item-specific variance. This reduces to Kuder-Richardson formula 20
#' (KR-20) when the columns of the data matrix are dichotomous.
#'
#' The confidence interval follows Feldt (1965) and is based on the
#' \eqn{F} distribution with \eqn{n - 1} and \eqn{(m - 1)(n - 1)} degrees of
#' freedom, where \eqn{n} is the number of subjects (rows) and \eqn{m} the
#' number of items (columns). It inherits the assumptions of the underlying
#' ANOVA derivation - in particular normally distributed scores and
#' essentially parallel items with homogeneous variances and covariances -
#' and should be read with more caution than the point estimate when these
#' are doubtful, for instance with markedly skewed or heterogeneous items.
#'
#' \code{sides} names the side on which the finite bound lies:
#' \code{"left"} yields an interval bounded below and \code{"right"} one
#' bounded above. Alpha cannot exceed 1, so the open upper side is reported
#' at that boundary rather than as \eqn{\infty} (design_rules.md 4.1),
#' while the open lower side stays \eqn{-\infty} because alpha is
#' unbounded below. 
#'
#' Missing values are handled according to package conventions: if
#' \code{na.rm = FALSE} and \code{x} contains missing values, the usual
#' structure is returned with \code{NA_real_} in place of every estimate.
#' If \code{na.rm = TRUE}, complete cases are used. Infinite values leave
#' the variances undefined and are rejected with an error.
#'
#' @param x a \eqn{n \times m} matrix or data frame with item responses,
#' \eqn{n} subjects (in rows) and \eqn{m} items (in columns)
#' @param returnConditional logical; if \code{TRUE}, alpha is additionally
#' calculated for the dataset with each item left out
#' 
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See \code{\link{ConfidenceIntervals}}.
#'
#' @param na.rm logical; if \code{TRUE}, incomplete cases are removed before
#' the computation proceeds
#'
#' @return a named numeric vector, or a list when
#' \code{returnConditional = TRUE}.
#'
#' If \code{na.rm = FALSE} and \code{x} contains missing values, the same
#' structure is returned with \code{NA_real_} throughout.
#'
#' If \code{conf.level = NA}, the numeric vector contains only \code{est};
#' otherwise it has elements:
#' \describe{
#'   \item{\code{est}}{point estimate.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#'
#' If \code{returnConditional = TRUE}, a list with the components:
#'
#' \describe{
#'   \item{\code{unconditional}}{alpha for the full set of items, as above}
#'   \item{\code{conditional}}{a data frame with one row per item, giving the
#'     alpha that would be realized if that item were excluded. \code{NULL}
#'     when \code{x} has fewer than 3 items, since dropping one would leave
#'     too few to compute alpha.}
#' }
#'
#' @note Based on code of Harold C. Doran, adapted to conform to package
#' standards.
#'
#' @references
#' Cronbach, L. J. (1951). Coefficient alpha and the internal structure of
#'   tests. \emph{Psychometrika}, \emph{16}(3), 297-334.
#'   \doi{10.1007/BF02310555}
#'
#' Feldt, L. S. (1965). The approximate sampling distribution of
#'   Kuder-Richardson reliability coefficient twenty.
#'   \emph{Psychometrika}, \emph{30}(3), 357-370.
#'   \doi{10.1007/BF02289499}
#'
#' @examples
#' set.seed(1234)
#' tmp <- data.frame(
#'   item1 = sample(c(0, 1), 20, replace = TRUE),
#'   item2 = sample(c(0, 1), 20, replace = TRUE),
#'   item3 = sample(c(0, 1), 20, replace = TRUE),
#'   item4 = sample(c(0, 1), 20, replace = TRUE),
#'   item5 = sample(c(0, 1), 20, replace = TRUE)
#' )
#'
#' cronbachAlpha(tmp[, 1:4])
#'
#' cronbachAlpha(tmp[, 1:4], conf.level = 0.95)
#'
#' # the conditional table is labelled with the column names of x
#' cronbachAlpha(tmp[, 1:4], returnConditional = TRUE, conf.level = 0.95)
#'
#' # fewer than 3 items: the conditional component is NULL
#' cronbachAlpha(tmp[, 1:2], returnConditional = TRUE, conf.level = 0.95)
#'
#' @family assoc.agreement
#' @concept internal-consistency
#' @concept reliability
#'
#' @export
cronbachAlpha <- function(x,
                          conf.level = NA,
                          sides = c("two.sided", "left", "right"),
                          returnConditional = FALSE,
                          na.rm = FALSE){

  if(!is.matrix(x) && !is.data.frame(x))
    stop("Argument 'x' must be a matrix or a data frame.")

  x <- as.matrix(x)

  if(!is.numeric(x))
    stop("Argument 'x' must contain numeric values only.")

  if(!is.logical(returnConditional) || length(returnConditional) != 1L ||
     is.na(returnConditional))
    stop("Argument 'returnConditional' must be a single non-missing logical value.")

  if(!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("Argument 'na.rm' must be a single non-missing logical value.")

  sides <- match.arg(sides)

  # Checked for type and length before is.na(), which would otherwise be
  # passed a zero-length or multi-element value and make the if() below
  # fail with an internal condition-length error rather than a clear
  # message.
  if(!is.numeric(conf.level) && !is.logical(conf.level))
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  if(length(conf.level) != 1L)
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  # NaN is numeric and NA-like, but suppressing the interval on a NaN
  # confidence level would hide a caller error rather than express an
  # intent to omit it, so only a true NA does that.
  if(is.nan(conf.level))
    stop("Argument 'conf.level' must be a single number between 0 and 1, or NA.")

  if(!is.na(conf.level)) {

    if(!is.numeric(conf.level) ||
       !is.finite(conf.level) ||
       conf.level <= 0 ||
       conf.level >= 1) {

      stop("Argument 'conf.level' must be a single number between 0 and 1.")

    }

  }

  if(na.rm)
    x <- x[complete.cases(x), , drop = FALSE]

  if(ncol(x) < 2L)
    stop("Argument 'x' must have at least 2 items (columns).")

  if(nrow(x) < 2L)
    stop("Argument 'x' must have at least 2 subjects (rows).")

  # Shape consistency with every other exit: a bare NA_real_ forced callers
  # to type-check the result before they could index it, and it silently
  # ignored returnConditional as well.
  if(anyNA(x)) {

    naRes <- .makeEstimateResult(
      est = NA_real_,
      lci = if(is.na(conf.level)) NULL else NA_real_,
      uci = if(is.na(conf.level)) NULL else NA_real_
    )

    if(!returnConditional)
      return(naRes)

    return(list(unconditional = naRes, conditional = NULL))
  }

  # Checked only after the NA policy has been applied: is.finite() is
  # FALSE for NA too, so an earlier check would turn the documented
  # NA-return into an error.
  if(!all(is.finite(x)))
    stop("Argument 'x' must not contain infinite values.")

  res <- .cronbachAlpha(
    x = x,
    conf.level = conf.level,
    sides = sides
  )

  if(!returnConditional)
    return(res)

  nItems <- ncol(x)

  # Dropping one item from a 2-item instrument would leave a single item,
  # for which alpha is undefined. NULL rather than an empty frame keeps
  # the "nothing to report" case distinguishable from a computed result.
  conditional <- NULL

  if(nItems > 2L) {

    # Column names are far more useful than positions in an
    # "alpha if item deleted" table; fall back to indices only when x
    # carries no names.
    itemNames <- colnames(x)

    if(is.null(itemNames))
      itemNames <- as.character(seq_len(nItems))

    condList <- vector("list", nItems)

    for(i in seq_len(nItems)) {

      condList[[i]] <- .cronbachAlpha(
        x = x[, -i, drop = FALSE],
        conf.level = conf.level,
        sides = sides
      )

    }

    conditional <- data.frame(
      item = itemNames,
      do.call(rbind, condList),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    rownames(conditional) <- NULL

  }

  list(
    unconditional = res,
    conditional = conditional
  )

}


# == internal helper functions ================================================

.cronbachAlpha <- function(x, conf.level = NA, sides = "two.sided"){

  nItems <- ncol(x)
  nSubj <- nrow(x)

  colVars <- apply(x, 2, var)
  total <- var(rowSums(x))

  # All subjects sharing the same total score leaves no variance to
  # decompose; alpha is undefined rather than zero.
  if(total == 0)
    return(
      .makeEstimateResult(
        est = NA_real_,
        lci = if(is.na(conf.level)) NULL else NA_real_,
        uci = if(is.na(conf.level)) NULL else NA_real_
      )
    )

  est <- (total - sum(colVars)) / total * (nItems / (nItems - 1))

  if(is.na(conf.level))
    return(.makeEstimateResult(est = est))

  alpha <- 1 - conf.level

  # Feldt (1965): the pivot is F with n-1 and (m-1)(n-1) degrees of
  # freedom, where n counts subjects and m items. Using length(x) here -
  # the number of cells rather than the number of subjects - inflates
  # both df by a factor of m and yields intervals that are far too
  # narrow.
  df1 <- nSubj - 1L
  df2 <- (nItems - 1L) * (nSubj - 1L)

  # The larger F quantile produces the lower bound, since the mapping
  # 1 - (1 - est) * F is decreasing in F. sides names the side on which
  # the finite bound lies, so "left" keeps the lower bound and "right"
  # the upper one.
  if(sides == "two.sided") {

    lci <- 1 - (1 - est) * qf(1 - alpha / 2, df1, df2)
    uci <- 1 - (1 - est) * qf(alpha / 2, df1, df2)

  } else if(sides == "left") {

    lci <- 1 - (1 - est) * qf(1 - alpha, df1, df2)
    uci <- 1

  } else {

    lci <- -Inf
    uci <- 1 - (1 - est) * qf(alpha, df1, df2)

  }

  .makeEstimateResult(
    est = est,
    lci = lci,
    uci = uci
  )

}
