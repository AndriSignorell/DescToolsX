
#' Polychoric Correlation
#'
#' Estimates the polychoric correlation between two ordinal variables based
#' on a contingency table. Both a two-step estimator and full maximum
#' likelihood (ML) estimation are supported.
#'
#' @param x A contingency table or an ordinal vector.
#' @param y Optional second ordinal vector. If supplied, a contingency table
#'   is constructed via \code{table(x, y)}.
#' @param method Character string specifying the estimation method:
#'   \describe{
#'     \item{\code{"two-step"}}{Two-step estimator (default, fast).}
#'     \item{\code{"ML"}}{Full maximum likelihood estimation.}
#'   }
#' @param se Logical; if \code{TRUE}, standard errors are computed via the
#'   Hessian matrix (ML estimation only).
#' @param control A list of control parameters passed to \code{\link[stats]{optim}}.
#' @param maxcor Numeric; maximum absolute correlation allowed (default
#'   \code{0.9999}) to avoid numerical issues near the boundary.
#'
#' @details
#' The polychoric correlation estimates the correlation between two latent
#' normally distributed variables underlying observed ordinal variables.
#'
#' The likelihood is based on a discretized bivariate normal distribution,
#' evaluated via \code{\link[mvtnorm]{pmvnorm}}.
#'
#' For numerical stability:
#' \itemize{
#'   \item The correlation parameter is internally transformed using
#'   \code{tanh()} to enforce \eqn{|\rho| < 1}.
#'   \item Cell probabilities are bounded away from zero to avoid
#'   \code{log(0)}.
#' }
#'
#' Empty rows or columns in the contingency table are removed with a warning.
#'
#' @return
#' If \code{se = FALSE}, a numeric value giving the estimated correlation.
#'
#' If \code{se = TRUE}, a list with components:
#' \describe{
#'   \item{\code{rho}}{Estimated polychoric correlation.}
#'   \item{\code{rowCuts}}{Estimated row thresholds.}
#'   \item{\code{colCuts}}{Estimated column thresholds.}
#'   \item{\code{var}}{Variance-covariance matrix of the estimates.}
#'   \item{\code{n}}{Total sample size.}
#'   \item{\code{chisq}}{Likelihood ratio test statistic.}
#'   \item{\code{df}}{Degrees of freedom.}
#'   \item{\code{method}}{Estimation method used.}
#' }
#' The returned object has class \code{"polychor"}.
#'
#' @references
#' Olsson, U. (1979). Maximum likelihood estimation of the polychoric
#' correlation coefficient. \emph{Psychometrika}, 44(4), 443–460.
#'
#' Fox, J. (2016). \emph{Applied Regression Analysis and Generalized Linear Models}.
#'
#' @seealso
#' \code{\link[mvtnorm]{pmvnorm}}, \code{\link[stats]{optim}}
#'
#' @examples
#' # Example with ordinal variables
#' x <- factor(sample(1:3, 100, replace = TRUE), ordered = TRUE)
#' y <- factor(sample(1:3, 100, replace = TRUE), ordered = TRUE)
#'
#' # Two-step estimate
#' corPolychor(x, y)
#'
#' # ML estimate
#' corPolychor(x, y, method = "ML")
#'
#' # With standard errors
#' res <- corPolychor(x, y, method = "ML", se = TRUE)
#' res$rho
#'

#' @family correlation
#' @concept correlation
#' @concept association-measures
#' @concept descriptive-statistics
#'
#'
#' @export
corPolychor <- function(x, y = NULL,
                        method = c("two-step", "ML"),
                        se = FALSE,
                        control = list(),
                        maxcor = 0.9999) {
  
  method <- match.arg(method)
  
  # --- build contingency table ------------------------------------------
  tab <- if (is.null(y)) x else table(x, y)
  
  # remove zero rows/cols
  zerorows <- rowSums(tab) == 0
  zerocols <- colSums(tab) == 0
  
  if (any(zerorows)) {
    warning(sprintf("%d empty row(s) removed", sum(zerorows)))
    tab <- tab[!zerorows, , drop = FALSE]
  }
  if (any(zerocols)) {
    warning(sprintf("%d empty column(s) removed", sum(zerocols)))
    tab <- tab[, !zerocols, drop = FALSE]
  }
  
  r <- nrow(tab)
  c <- ncol(tab)
  
  if (r < 2 || c < 2)
    stop("Need at least 2x2 table")
  
  n <- sum(tab)
  
  # --- thresholds --------------------------------------------------------
  rc <- qnorm(cumsum(rowSums(tab)) / n)[-r]
  cc <- qnorm(cumsum(colSums(tab)) / n)[-c]
  
  
  # --- log-likelihood ----------------------------------------------------
  logLikFun <- function(pars) {
    
    # tanh parametrization → guarantees |rho| < 1
    rho <- tanh(pars[1])
    rho <- max(min(rho, maxcor), -maxcor)
    
    if (length(pars) == 1) {
      rowCuts <- rc
      colCuts <- cc
    } else {
      rowCuts <- sort(pars[2:r])
      colCuts <- sort(pars[(r + 1):(r + c - 1)])
      
      if (any(diff(rowCuts) <= 0) || any(diff(colCuts) <= 0)) {
        return(1e10)
      }
    }
    
    P <- .binBvn(rho, rowCuts, colCuts)
    
    # numerical stability
    P <- pmax(P, 1e-12)
    
    -sum(tab * log(P))
  }
  
  
  
  # --- estimation --------------------------------------------------------
  if (method == "two-step" && !se) {
    rho <- optimise(logLikFun, interval = c(-2, 2))$minimum
    return(tanh(rho))
  }
  
  # ML estimation
  start <- c(0, rc, cc)
  
  fit <- optim(start,
               logLikFun,
               method = "BFGS",
               control = control,
               hessian = se)
  
  rho <- tanh(fit$par[1])
  rho <- max(min(rho, 1), -1)
  
  if (!se) return(rho)
  
  # --- standard errors ---------------------------------------------------
  chisq <- 2 * (fit$value + sum(tab * log((tab + 1e-12) / n)))
  df <- length(tab) - r - c
  
  res <- list(
    type = "polychoric",
    rho = rho,
    rowCuts = fit$par[2:r],
    colCuts = fit$par[(r + 1):(r + c - 1)],
    var = solve(fit$hessian),
    n = n,
    chisq = chisq,
    df = df,
    method = method
  )
  
  class(res) <- "polychor"
  
  res
}


# == internal helper functions ==========================================


# --- probability matrix ------------------------------------------------
.binBvn <- function(rho, rowCuts, colCuts) {
  
  rowCuts <- c(-Inf, rowCuts, Inf)
  colCuts <- c(-Inf, colCuts, Inf)
  
  P <- matrix(0, length(rowCuts) - 1, length(colCuts) - 1)
  
  R <- matrix(c(1, rho, rho, 1), 2, 2)
  
  for (i in seq_len(nrow(P))) {
    for (j in seq_len(ncol(P))) {
      P[i, j] <- as.numeric(
        mvtnorm::pmvnorm(
          lower = c(rowCuts[i], colCuts[j]),
          upper = c(rowCuts[i + 1], colCuts[j + 1]),
          corr  = R
        )
      )
    }
  }
  
  P
}


