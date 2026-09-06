#' Randolph's Free-Marginal Multirater Kappa
#'
#' Computes Randolph's free-marginal multirater kappa for \eqn{m} raters over
#' \eqn{N} subjects. This agreement coefficient does not assume fixed marginal
#' distributions (i.e., it is free-marginal).
#'
#' @name randolphKappa
#'
#' @param x a matrix of size \eqn{N \times m} with subjects in rows and raters
#'   in columns; cells contain the assigned categories
#' @param categories the categories a rater could have chosen from, or a single
#'   number giving how many there were. Defaults to `NULL`, i.e. the
#'   categories actually observed in `x`. See Details.
#' @param conf.level reserved for future confidence intervals; must be
#'   `NA`
#' @param ... reserved for future bootstrap options and currently ignored
#'
#' @details
#' Let \eqn{k} be the number of categories, \eqn{m} the number of raters, and
#' \eqn{N} the number of subjects. Randolph's kappa is
#' \deqn{\kappa = \frac{P_o - 1/k}{1 - 1/k},}
#' where the observed agreement \eqn{P_o} is the proportion of agreeing rater
#' *pairs*,
#' \deqn{P_o = \frac{1}{N} \sum_{i=1}^{N}
#'       \frac{\sum_{j} n_{ij}(n_{ij}-1)}{m(m-1)}.}
#' Here, \eqn{n_{ij}} denotes the number of raters who assigned subject \eqn{i}
#' to category \eqn{j}. This is the same observed agreement as in Fleiss'
#' kappa (and as computed by [percAgreement()]); Randolph's
#' coefficient differs from Fleiss' only in the chance agreement \eqn{P_e},
#' which is fixed at \eqn{1/k} instead of being estimated from the marginals.
#'
#' \eqn{P_e = 1/k} refers to the categories a rater could have *chosen*,
#' not to those that happen to occur in the data. If a category was available
#' but never used, the default `categories = NULL` understates \eqn{k}
#' and thus overstates chance agreement, which biases \eqn{\kappa} downwards.
#' Supply `categories` whenever the coding scheme is known.
#'
#' Long-format ratings can first be reshaped with [raterFrame()].
#'
#' @return a numeric scalar containing Randolph's kappa
#'
#' @references
#' Randolph, J. J. (2005). Free-Marginal Multirater Kappa (multirater \eqn{\kappa_{\mathrm{free}}}):
#' An Alternative to Fleiss’ Fixed-Marginal Multirater Kappa. Online submission.
#'
#' @examples
#' ## Matrix (subjects x raters), 5 subjects, 3 raters
#' x <- matrix(c(
#'   1,1,1,
#'   2,2,2,
#'   1,2,1,
#'   3,3,3,
#'   2,2,1
#' ), ncol = 3, byrow = TRUE)
#' randolphKappa(x)
#'
#' # the raters could have chosen from five categories, not just the three
#' # they used
#' randolphKappa(x, categories = 5)
#'
#' ## Long format with a formula
#' df <- data.frame(
#'   subject = rep(1:5, each = 3),
#'   rater   = rep(paste0("r", 1:3), times = 5),
#'   rating  = c(1,1,1, 2,2,2, 1,2,1, 3,3,3, 2,2,1)
#' )
#' randolphKappa(raterFrame(rating ~ subject | rater, 
#'                          data = df, dropSubj=TRUE))
#'
#' @rdname randolphKappa
#'
#' @family assoc.agreement  
#' @concept agreement  
#' @concept categorical-agreement
#'
#'
#' @export
randolphKappa <- function(x, categories = NULL, conf.level = NA, ...) {

  if (!(length(conf.level) == 1L && is.na(conf.level)))
    stop("confidence intervals are not implemented for randolphKappa(); ",
         "leave 'conf.level' at NA.")

  x <- .asRatingsMatrix(x)

  N <- nrow(x)

  # number of categories: observed by default, otherwise as supplied
  if (is.null(categories)) {
    k <- length(unique(as.vector(x[!is.na(x)])))
  } else if (length(categories) == 1L && is.numeric(categories)) {
    k <- as.integer(categories)
  } else {
    k <- length(unique(categories))
  }

  if (is.na(k) || k < 2L)
    stop("at least two categories are needed; kappa is undefined for k < 2.")

  if (!is.null(categories) &&
      k < length(unique(as.vector(x[!is.na(x)]))))
    stop("'categories' has fewer entries than there are distinct ratings in 'x'.")

  # observed agreement per subject: the proportion of agreeing rater PAIRS.
  # The previous version used max_j n_ij / m, the share of raters in the modal
  # category. That is a different quantity: by the pigeonhole principle
  # max_j n_ij >= m/k, so Po >= 1/k = Pe and the coefficient could never
  # become negative -- systematic disagreement was invisible. With two raters
  # it assigned 1/2 to a disagreeing subject instead of 0.
  poi <- apply(x, 1L, function(row) {
    v <- row[!is.na(row)]
    m <- length(v)
    if (m < 2L) return(NA_real_)
    tab <- table(v)
    sum(tab * (tab - 1)) / (m * (m - 1))
  })

  if (all(is.na(poi)))
    stop("no subject has two or more ratings.")

  if (anyNA(poi))
    warning(gettextf("%d subject(s) with fewer than two ratings removed.",
                     sum(is.na(poi))), domain = NA)

  Po <- mean(poi, na.rm = TRUE)

  Pe <- 1 / k

  (Po - Pe) / (1 - Pe)

}
