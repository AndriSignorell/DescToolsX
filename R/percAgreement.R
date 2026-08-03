
#' Percent Agreement with Design-Based SE and CI
#'
#' Computes the proportion of agreement (percent agreement) along with
#' design-based standard errors and confidence intervals following
#' Klein and Gwet.
#'
#' For a confusion matrix the agreement indicator of a subject is 1 on the
#' diagonal and 0 elsewhere. For a ratings matrix the subject-wise agreement
#' is the proportion of agreeing rater \emph{pairs},
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
#' @param input character string specifying the input format:
#'   \code{"auto"}, \code{"confusion"}, or \code{"ratings"}
#'
#' @param conf.level confidence level for the interval
#' @param fpc finite population correction, the sampling fraction \eqn{n/N}
#'   in \eqn{[0, 1)} (default \code{0})
#' @param verbose logical; whether to return detailed output
#' @param ... reserved for future extensions
#'
#' @return if \code{verbose = FALSE}, a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{proportion of agreement}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' if \code{verbose = TRUE}, a list with the estimate, standard error,
#' confidence interval, sample sizes, and method description.
#'
#'
#' @family assoc.agreement  
#' @concept agreement  
#' @concept categorical-agreement
#'
#'
#' @export
percAgreement <- function(x, y = NULL,
                          input = c("auto", "confusion", "ratings"),
                          conf.level = 0.95,
                          fpc = 0,
                          verbose = FALSE,
                          ...) {

  input <- match.arg(input)

  if (length(conf.level) != 1L || !is.numeric(conf.level) || is.na(conf.level) ||
      conf.level <= 0 || conf.level >= 1)
    stop("'conf.level' must be a single number in (0, 1).")

  if (length(fpc) != 1L || !is.numeric(fpc) || is.na(fpc) || fpc < 0 || fpc >= 1)
    stop("'fpc' must be a single number in [0, 1).")

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
  if (is.na(Po) || n <= 1) {
    se <- NA_real_
    ci <- c(NA_real_, NA_real_)
  } else {
    se <- sqrt(var_hat)
    alpha <- 1 - conf.level
    tcrit <- qt(1 - alpha/2, df = n - 1)
    ci <- c(
      max(0, Po - tcrit * se),
      min(1, Po + tcrit * se)
    )
  }
  
  if (!verbose) {
    return(c(est = Po, lci = ci[1], uci = ci[2]))
  }
  
  list(
    estimate = Po,
    se = se,
    conf.int = ci,
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
