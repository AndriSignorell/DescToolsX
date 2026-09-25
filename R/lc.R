#' Lorenz Curve
#'
#' Computes the empirical Lorenz curve for a numeric vector, optionally with
#' weights and grouped via formula interface.  Returns an object of class
#' `"Lc"` (or `"LcList"` for grouped data) that can be visualized
#' with `plot()`, `lines()`, and `points()` from the
#' \pkg{pharos} package.
#'
#' @details
#' The Lorenz curve is defined as
#'
#' \deqn{L(p) = \frac{\sum_{i=1}^{k} w_i x_i}{\sum_{i=1}^{n} w_i x_i}}
#'
#' where observations are sorted in increasing order and \eqn{p} denotes
#' the cumulative proportion of weights up to rank \eqn{k}.
#'
#' The generalized Lorenz curve scales the standard curve by the weighted
#' mean \eqn{\mu}:
#'
#' \deqn{L_{\text{general}}(p) = L(p) \cdot \mu}
#'
#' For formula input of the form `y ~ group`, the data are split by
#' group and a separate Lorenz curve is computed for each level.  A single
#' `"Lc"` object is returned when there is only one group; otherwise
#' an `"LcList"`.
#'
#' Bootstrap confidence intervals in `predict.Lc()` are based on
#' resampling with replacement from the (weighted) empirical distribution,
#' followed by interpolation of each replicate at the requested population
#' shares and pointwise quantiles across bootstrap replicates.  The number
#' of replications is controlled by `R` passed via `...` and
#' extracted by `.extractBootArgs()` (default `R = 999`).
#'
#' @param x numeric vector of non-negative values
#' @param n numeric vector of non-negative weights of the same length as
#'   `x`. Defaults to equal weights (`rep(1, length(x))`). Bootstrap intervals
#'   draw `floor(sum(n))` observations with probabilities proportional to `n`;
#'   the sum must be finite and at least one. Thus rescaling the weights
#'   changes the bootstrap sample size; frequency weights are appropriate
#'   when the weights represent replicated observations.
#' @param na.rm logical. If `TRUE`, observations with `NA` in
#'   `x` or `n` are removed before computation.  Default is
#'   `FALSE`.
#' @param formula a formula of the form `y ~ group` specifying the
#'   response and grouping variable
#' @param data optional data frame in which to evaluate `formula`
#' @param subset optional expression indicating which rows of `data`
#'   to use
#' @param na.action function for handling missing values in the model frame.
#'   Default is [stats::na.pass()].
#' @param object object of class `"Lc"` as returned by `lc()`
#' @param newdata optional numeric vector of values in \eqn{[0, 1]} at
#'   which to evaluate the Lorenz curve via linear interpolation.  If
#'   omitted, the original grid points are returned.
#' @param conf.level numeric scalar in \eqn{(0, 1)}. If supplied,
#'   bootstrap confidence intervals at level `conf.level` are added
#'   as columns `lci` and `uci`.  Set to `NA` (default)
#'   to suppress intervals. For the standard curve, all-zero bootstrap
#'   samples have undefined income shares and are omitted pointwise; if no
#'   finite replicates remain, the corresponding limits are `NA`.
#' @param general logical. If `TRUE`, the generalized Lorenz curve
#'   is used.  Default is `FALSE`.
#' @param ... further arguments passed to `lc.default()` from
#'   `lc.formula()`. In `predict.Lc()`, the argument `R`
#'   (positive integer, default `999`) controls the number of bootstrap
#'   replications when `conf.level` is supplied; it is extracted via
#'   `.extractBootArgs()` and ignored otherwise. Only percentile intervals
#'   are implemented: `type = "perc"` may be supplied explicitly; other
#'   interval types are rejected.
#'
#' @return
#' \describe{
#'   \item{`lc.default()`}{an object of class `"Lc"`, a list
#'     with components:
#'     \describe{
#'       \item{`p`}{numeric vector of cumulative population shares
#'         starting at 0}
#'       \item{`L`}{numeric vector of Lorenz curve values at `p`}
#'       \item{`L.general`}{generalized Lorenz curve values}
#'       \item{`Gini`}{estimated Gini coefficient}
#'       \item{`x`}{unsorted data used after missing-value removal}
#'       \item{`n`}{corresponding weights after missing-value removal}
#'     }
#'   }
#'   \item{`lc.formula()`}{a single `"Lc"` object if the formula
#'     specifies one group, otherwise an object of class `"LcList"`
#'     (a named list of `"Lc"` objects, one per group level)}
#'   \item{`predict.Lc()`}{a data frame with columns `p` and
#'     `L` (interpolated curve values at `newdata`).  If
#'     `conf.level` is supplied, columns `lci` and `uci`
#'     are appended.}
#' }
#'
#' @examples
#' set.seed(1)
#' x <- rlnorm(100)
#'
#' # default method
#' lc_obj <- lc(x)
#' lc_obj$Gini
#'
#' # with weights
#' w <- runif(100, 0.5, 2)
#' lc(x, n = w)
#'
#' # formula interface: grouped Lorenz curves
#' g <- sample(letters[1:3], 100, replace = TRUE)
#' d <- data.frame(x = x, g = g)
#' lc_grp <- lc(x ~ g, data = d)
#'
#' # prediction on a regular grid
#' predict(lc_obj, newdata = seq(0, 1, by = 0.1))
#'
#' # with 95% bootstrap confidence intervals (R = 200 for speed)
#' predict(lc_obj, newdata = seq(0, 1, by = 0.25),
#'         conf.level = 0.95, R = 200)
#'         
#'         
#' # plotting routines from package pharos         
#' set.seed(1)
#' x <- rlnorm(100)
#' lc_obj <- lc(x)
#'
#' # basic plot
#' plot(lc_obj)
#'
#' # overlay confidence band
#' lines(lc_obj, cbandArgs = list(conf.level = 0.95))
#'
#' # add points
#' points(lc_obj, pch = 16)
#'
#' # generalized Lorenz curve
#' plot(lc_obj, general = TRUE)
#'
#' # grouped Lorenz curves
#' g <- sample(letters[1:3], 100, replace = TRUE)
#' lc_grp <- lc(x ~ g)
#' plot(lc_grp)
#' lines(lc_grp)
#' points(lc_grp, pch = 16)
#' 
#'
#' @references
#' Lorenz, M. O. (1905). Methods of measuring the concentration of wealth.
#' *Publications of the American Statistical Association*, **9**,
#' 209--219.
#'
#' @seealso  [pharos::plot.Lc] for visualization.
#'
#' @name lc
#' @family inequality
#' @concept descriptive-statistics
#' @concept inequality
#' 
NULL




#' @family inequality  
#' @concept inequality
#' @concept distribution-visualization
#' @export
lc <- function(x, ...)
  UseMethod("lc")


#' @rdname lc
#' @export
lc.formula <- function(formula, data, subset, na.action = na.pass, ...) {
  
  subset_expr <- if (!missing(subset)) substitute(subset) else NULL
  
  rf <- resolveFormula(
    formula,
    data       = data,
    subset     = subset_expr,
    na.action  = na.action,
    allowed    = c("one-sample", "n-sample-independent")
  )
  
  # --- one sample ---
  if (rf$type == "one-sample") {
    return(lc(rf$x, ...))
  }
  
  # --- grouped ---
  if (rf$type == "n-sample-independent") {
    
    split_data <- split(rf$x, rf$group)
    
    res <- lapply(split_data, function(x) lc(x, ...))
    
    class(res) <- c("LcList", "list")
    
    attr(res, "groups") <- levels(rf$group)
    attr(res, "data.name") <- rf$data.name
    
    return(res)
  }
  
  stop("unsupported formula type for lc")
}



#' @rdname lc
#' @export
lc.default <- function(x, n = rep(1, length(x)), na.rm = FALSE, ...) {
  
  if (na.rm) {
    keep <- !is.na(x) & !is.na(n)
    x <- x[keep]
    n <- n[keep]
  }
  
  if (length(x) == 0)
    stop("empty input")
  
  if (any(is.na(x)) || any(x < 0))
    stop("x must be non-negative and not contain NA values")
  
  # Retain the effective unsorted sample for subsequent bootstrap prediction.
  xx <- x
  nn <- n

  g <- gini(x, weights = n, na.rm = FALSE)
  
  o <- order(x)
  x <- x[o]
  n <- n[o]
  
  wx <- n * x
  
  p <- cumsum(n) / sum(n)
  L <- cumsum(wx) / sum(wx)
  
  p <- c(0, p)
  L <- c(0, L)
  
  L2 <- L * sum(wx) / sum(n)
  
  lc <- list(p = p, L = L, L.general = L2, Gini = g, x = xx, n = nn)
  class(lc) <- "Lc"
  
  lc
}



#' @rdname lc
#' @export
predict.Lc <- function(object, newdata, conf.level = NA, general = FALSE, ...) {
  
  if (!inherits(object, "Lc"))
    stop("object must be of class 'Lc'")
  
  # --- newdata validation ---
  if (!missing(newdata)) {
    if (!is.numeric(newdata))
      stop("newdata must be numeric")
    if (any(newdata < 0 | newdata > 1, na.rm = TRUE))
      stop("newdata must be in [0, 1]")
  }
  
  # --- select curve ---
  L <- if (!general) object$L else object$L.general
  
  # --- base prediction ---
  if (missing(newdata)) {
    newdata <- object$p
    res <- data.frame(p = object$p, L = L)
  } else {
    tmp <- approx(x = object$p, y = L, xout = newdata, rule = 2)
    res <- data.frame(p = tmp$x, L = tmp$y)
  }
  
  # --- helper: safe interpolation ---
  interp_safe <- function(x, y, xout) {
    ok <- is.finite(x) & is.finite(y)
    if (sum(ok) < 2)
      return(rep(NA_real_, length(xout)))
    approx(x[ok], y[ok], xout = xout, rule = 2)$y
  }
  
  # --- confidence interval ---
  if (length(conf.level) != 1L ||
      !(is.numeric(conf.level) || is.logical(conf.level)) ||
      is.nan(conf.level))
    stop("conf.level must be a single number in (0, 1), or NA")

  if (!is.na(conf.level)) {
    if (!is.numeric(conf.level) || !is.finite(conf.level) ||
        conf.level <= 0 || conf.level >= 1)
      stop("conf.level must be a single number in (0, 1)")

    # This implementation computes pointwise percentile intervals only.
    # Do not inherit the shared helper's BCa default or its BCa diagnostics.
    bootDots <- list(...)
    if (!is.null(bootDots$type) && !identical(bootDots$type, "perc"))
      stop("predict.Lc supports only type = 'perc' (percentile intervals)")
    bootDots$type <- "perc"
    bootArgs <- .extractBootArgs(bootDots)
    R <- bootArgs$R

    # Also accept objects made before lc() stored its cleaned sample.
    bx <- object$x
    bw <- object$n
    if (!is.numeric(bx) || !is.numeric(bw) || length(bx) != length(bw))
      stop("the stored data and weights must be numeric vectors of equal length")
    keep <- !is.na(bx) & !is.na(bw)
    bx <- bx[keep]
    bw <- bw[keep]
    if (any(!is.finite(bx)) || any(bx < 0) ||
        any(!is.finite(bw)) || any(bw < 0))
      stop("the stored data and weights must be finite and non-negative")

    wsum <- sum(bw)
    if (!length(bx) || wsum == 0) {
      return(data.frame(res, lci = rep(NA_real_, length(newdata)),
                       uci = rep(NA_real_, length(newdata))))
    }
    if (!is.finite(wsum) || wsum < 1)
      stop("the sum of weights must be finite and at least 1 for bootstrap intervals")

    # Preserve the existing frequency-weight resampling convention: the
    # draw size is sum(weights), truncated by sample.int() when fractional.
    # Sampling indices avoids sample(5, ...) interpreting one value as 1:5.
    curves <- lapply(seq_len(R), function(i) {
      idx <- sample.int(length(bx), size = wsum, replace = TRUE, prob = bw)
      sampleX <- sort(bx[idx])
      nSample <- length(sampleX)
      p <- c(0, seq_len(nSample) / nSample)
      income <- c(0, cumsum(sampleX))
      if (general) {
        values <- income / nSample
      } else {
        total <- income[length(income)]
        if (total == 0)
          return(rep(NA_real_, length(newdata)))
        values <- income / total
      }
      interp_safe(p, values, newdata)
    })

    # Quantiles are taken AT newdata, not interpolated after taking quantiles.
    mat <- do.call(rbind, curves)
    point_quantile <- function(j, prob) {
      values <- mat[, j]
      values <- values[is.finite(values)]
      if (!length(values)) return(NA_real_)
      unname(quantile(values, probs = prob))
    }
    lci <- vapply(seq_along(newdata), point_quantile, numeric(1),
                  prob = (1 - conf.level) / 2)
    uci <- vapply(seq_along(newdata), point_quantile, numeric(1),
                  prob = 1 - (1 - conf.level) / 2)
    res <- data.frame(res, lci = lci, uci = uci)
  }

  res
}
