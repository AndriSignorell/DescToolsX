
#' Polychoric Correlation
#'
#' Estimates the polychoric correlation between two ordinal variables based
#' on a contingency table. Both a two-step estimator and full maximum
#' likelihood (ml) estimation are supported.
#'
#' @param x a contingency table or an ordinal vector
#' @param y optional second ordinal vector. If supplied, a contingency table
#'   is constructed via `table(x, y, \dots)`.
#' @param method character string specifying the estimation method:
#'   \describe{
#'     \item{`"two-step"`}{two-step estimator (default, fast)}
#'     \item{`"ml"`}{full maximum likelihood estimation}
#'   }
#' @param se logical; if `TRUE`, standard errors are computed via the
#'   Hessian matrix. This requires ml estimation, so it is an error to
#'   combine it with `method = "two-step"`.
#' @param control a list of control parameters passed to [stats::optim()]
#' @param maxcor numeric; maximum absolute correlation allowed (default
#'   `0.9999`) to avoid numerical issues near the boundary
#' @param ... further arguments passed to [table()] when `y`
#'   is supplied, for example `useNA`
#'
#' @details
#' The polychoric correlation estimates the correlation between two latent
#' normally distributed variables underlying observed ordinal variables.
#'
#' The likelihood is based on a discretized bivariate normal distribution,
#' evaluated via [mvtnorm::pmvnorm()].
#'
#' For numerical stability:
#' \itemize{
#'   \item The correlation parameter is internally transformed using
#'   `tanh()` to enforce \eqn{|\rho| < 1}. The search range on that
#'   scale is derived from `maxcor`, so the estimate is free to
#'   approach the documented boundary.
#'   \item Cell probabilities are bounded away from zero to avoid
#'   `log(0)`.
#' }
#'
#' Empty rows or columns in the contingency table are removed with a warning.
#'
#' @return
#' if `se = FALSE`, a numeric value giving the estimated correlation.
#'
#' If `se = TRUE`, a list with components:
#' \describe{
#'   \item{`type`}{type of correlation}
#'   \item{`rho`}{estimated polychoric correlation}
#'   \item{`rowCuts`}{estimated row thresholds}
#'   \item{`colCuts`}{estimated column thresholds}
#'   \item{`var`}{variance-covariance matrix of the estimates, on the
#'     scale of `rho` and the thresholds. The optimiser works on
#'     `atanh(rho)`, so the leading row and column are transformed
#'     back with the delta method, factor \eqn{(1 - \rho^2)}.}
#'   \item{`n`}{total sample size}
#'   \item{`chisq`}{likelihood-ratio test statistic}
#'   \item{`df`}{degrees of freedom}
#'   \item{`method`}{estimation method actually used}
#' }
#' The returned object has class `"Polychor"`.
#'
#' @references
#' Olsson, U. (1979). Maximum likelihood estimation of the polychoric
#' correlation coefficient. *Psychometrika*, 44(4), 443--460.
#'
#' Fox, J. (2016). *Applied Regression Analysis and Generalized Linear Models*.
#'
#' @seealso
#' [mvtnorm::pmvnorm()], [stats::optim()]
#'
#' @examples
#' # Example with ordinal variables
#' set.seed(1)
#' z <- rnorm(200)
#' x <- factor(cut(z + rnorm(200, sd = 0.6), 3), ordered = TRUE)
#' y <- factor(cut(z + rnorm(200, sd = 0.6), 3), ordered = TRUE)
#'
#' # Two-step estimate
#' corPolychor(x, y)
#'
#' # ml estimate
#' corPolychor(x, y, method = "ml")
#'
#' # With standard errors
#' res <- corPolychor(x, y, method = "ml", se = TRUE)
#' res$rho
#'
#' @family assoc.continuous
#' @concept correlation
#' @concept latent-variable
#' @concept ordinal
#' @export
corPolychor <- function(x, y = NULL,
                        method = c("two-step", "ml"),
                        se = FALSE,
                        control = list(),
                        maxcor = 0.9999,
                        ...) {

  method <- match.arg(method)

  if (!is.logical(se) || length(se) != 1L || is.na(se))
    stop("'se' must be a single non-missing logical value")

  # Standard errors come from the ml Hessian. The former version quietly
  # ran the full ml optimisation for se = TRUE and then reported
  # method = "two-step" in the result, so the object described an
  # estimator that had not been used.
  if (se && method == "two-step")
    stop("standard errors require method = \"ml\"")

  if (!is.numeric(maxcor) || length(maxcor) != 1L || !is.finite(maxcor) ||
      maxcor <= 0 || maxcor >= 1)
    stop("'maxcor' must be a single number in (0, 1)")

  # --- build contingency table ------------------------------------------
  tab <- if (is.null(y)) as.table(as.matrix(x)) else table(x, y, ...)

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

  nr <- nrow(tab)
  nc <- ncol(tab)   # 'c' as a local name masked base::c()

  if (nr < 2 || nc < 2)
    stop("Need at least 2x2 table")

  n <- sum(tab)

  # --- thresholds --------------------------------------------------------
  rc <- qnorm(cumsum(rowSums(tab)) / n)[-nr]
  cc <- qnorm(cumsum(colSums(tab)) / n)[-nc]


  # --- log-likelihood ----------------------------------------------------
  logLikFun <- function(pars) {

    # tanh parametrization -> guarantees |rho| < 1
    rho <- tanh(pars[1])
    rho <- max(min(rho, maxcor), -maxcor)

    if (length(pars) == 1) {
      rowCuts <- rc
      colCuts <- cc
    } else {
      rowCuts <- sort(pars[2:nr])
      colCuts <- sort(pars[(nr + 1):(nr + nc - 1)])
    }

    P <- .binBvn(rho, rowCuts, colCuts)

    # numerical stability
    P <- pmax(P, 1e-12)

    -sum(tab * log(P))
  }


  # --- estimation --------------------------------------------------------
  # The search interval lives on the atanh scale. It used to be fixed at
  # c(-2, 2), i.e. |rho| <= tanh(2) = 0.964: any stronger association was
  # silently truncated there, well short of the documented maxcor.
  atanhLim <- atanh(maxcor)

  if (method == "two-step") {
    rho <- optimise(logLikFun, interval = c(-atanhLim, atanhLim))$minimum
    return(max(min(tanh(rho), maxcor), -maxcor))
  }

  # ml estimation
  start <- c(0, rc, cc)

  fit <- optim(start,
               logLikFun,
               method = "BFGS",
               control = control,
               hessian = se)

  rho <- tanh(fit$par[1])
  rho <- max(min(rho, maxcor), -maxcor)

  if (!se) return(rho)

  # --- standard errors ---------------------------------------------------
  chisq <- 2 * (fit$value + sum(tab * log((tab + 1e-12) / n)))
  df <- length(tab) - nr - nc

  vcov <- tryCatch(solve(fit$hessian),
                   error = function(e)
                     stop("the Hessian is singular; standard errors are not ",
                          "available for this fit", call. = FALSE))

  # The optimiser parameterises rho as atanh(rho), so the raw Hessian
  # gives the variance of the transformed parameter, not of rho.
  # d rho / d par = 1 - tanh(par)^2 = 1 - rho^2.
  jac <- 1 - rho^2
  vcov[1, ] <- vcov[1, ] * jac
  vcov[, 1] <- vcov[, 1] * jac

  res <- list(
    type = "polychoric",
    rho = rho,
    rowCuts = fit$par[2:nr],
    colCuts = fit$par[(nr + 1):(nr + nc - 1)],
    var = vcov,
    n = n,
    chisq = chisq,
    df = df,
    method = method
  )

  class(res) <- "Polychor"

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
