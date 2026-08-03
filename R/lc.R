#' Lorenz Curve
#'
#' Computes the empirical Lorenz curve for a numeric vector, optionally with
#' weights and grouped via formula interface.  Returns an object of class
#' \code{"Lc"} (or \code{"LcList"} for grouped data) that can be visualized
#' with \code{plot()}, \code{lines()}, and \code{points()} from the
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
#' For formula input of the form \code{y ~ group}, the data are split by
#' group and a separate Lorenz curve is computed for each level.  A single
#' \code{"Lc"} object is returned when there is only one group; otherwise
#' an \code{"LcList"}.
#'
#' Bootstrap confidence intervals in \code{predict.Lc()} are based on
#' resampling with replacement from the (weighted) empirical distribution,
#' followed by pointwise quantiles across bootstrap replicates.  The number
#' of replications is controlled by \code{R} passed via \code{...} and
#' extracted by \code{.extractBootArgs()} (default \code{R = 999}).
#'
#' @param x numeric vector of non-negative values
#' @param n numeric vector of non-negative weights of the same length as
#'   \code{x}. Defaults to equal weights (\code{rep(1, length(x))}).
#' @param na.rm logical. If \code{TRUE}, observations with \code{NA} in
#'   \code{x} or \code{n} are removed before computation.  Default is
#'   \code{FALSE}.
#' @param formula a formula of the form \code{y ~ group} specifying the
#'   response and grouping variable
#' @param data optional data frame in which to evaluate \code{formula}
#' @param subset optional expression indicating which rows of \code{data}
#'   to use
#' @param na.action function for handling missing values in the model frame.
#'   Default is \code{\link[stats]{na.pass}}.
#' @param object object of class \code{"Lc"} as returned by \code{lc()}
#' @param newdata optional numeric vector of values in \eqn{[0, 1]} at
#'   which to evaluate the Lorenz curve via linear interpolation.  If
#'   omitted, the original grid points are returned.
#' @param conf.level numeric scalar in \eqn{(0, 1)}. If supplied,
#'   bootstrap confidence intervals at level \code{conf.level} are added
#'   as columns \code{lci} and \code{uci}.  Set to \code{NA} (default)
#'   to suppress intervals.
#' @param general logical. If \code{TRUE}, the generalized Lorenz curve
#'   is used.  Default is \code{FALSE}.
#' @param ... further arguments passed to \code{lc.default()} from
#'   \code{lc.formula()}. In \code{predict.Lc()}, the argument \code{R}
#'   (positive integer, default \code{999}) controls the number of bootstrap
#'   replications when \code{conf.level} is supplied; it is extracted via
#'   \code{.extractBootArgs()} and ignored otherwise.
#'
#' @return
#' \describe{
#'   \item{\code{lc.default()}}{an object of class \code{"Lc"}, a list
#'     with components:
#'     \describe{
#'       \item{\code{p}}{numeric vector of cumulative population shares
#'         starting at 0}
#'       \item{\code{L}}{numeric vector of Lorenz curve values at \code{p}}
#'       \item{\code{L.general}}{generalized Lorenz curve values}
#'       \item{\code{Gini}}{estimated Gini coefficient}
#'       \item{\code{x}}{original unsorted data vector}
#'       \item{\code{n}}{original weight vector}
#'     }
#'   }
#'   \item{\code{lc.formula()}}{a single \code{"Lc"} object if the formula
#'     specifies one group, otherwise an object of class \code{"LcList"}
#'     (a named list of \code{"Lc"} objects, one per group level)}
#'   \item{\code{predict.Lc()}}{a data frame with columns \code{p} and
#'     \code{L} (interpolated curve values at \code{newdata}).  If
#'     \code{conf.level} is supplied, columns \code{lci} and \code{uci}
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
#' \emph{Publications of the American Statistical Association}, \bold{9},
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
  
  xx <- x
  nn <- n
  
  if (na.rm) {
    keep <- !is.na(x) & !is.na(n)
    x <- x[keep]
    n <- n[keep]
  }
  
  if (length(x) == 0)
    stop("empty input")
  
  if (any(is.na(x)) || any(x < 0))
    stop("x must be non-negative and not contain NA values")
  
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
  if (!is.na(conf.level)) {
    
    if (!is.numeric(conf.level) || length(conf.level) != 1 ||
        conf.level <= 0 || conf.level >= 1)
      stop("conf.level must be a single number in (0, 1)")
    
    bootArgs <- .extractBootArgs(match.call(expand.dots = FALSE)$`...`)
    R <- bootArgs$R
    
    # --- reconstruct weighted sample ---
    wsum <- sum(object$n)
    
    if (wsum == 0 || length(object$x) == 0) {
      lci <- uci <- rep(NA_real_, length(newdata))
      return(data.frame(res, lci = lci, uci = uci))
    }
    
    x_full <- sample(
      object$x,
      size    = wsum,
      replace = TRUE,
      prob    = object$n
    )
    
    # --- bootstrap ---
    lst <- replicate(R, lc(x_full), simplify = FALSE)
    
    curve_name <- if (general) "L.general" else "L"
    
    mat <- do.call(
      rbind,
      lapply(lst, function(obj) obj[[curve_name]])
    )
    
    ci_x <- lst[[1]]$p
    
    # --- handle degenerate bootstrap ---
    if (is.null(mat) || nrow(mat) == 0) {
      lci <- rep(NA_real_, length(newdata))
      uci <- rep(NA_real_, length(newdata))
    } else {
      lci_raw <- apply(mat, 2, quantile,
                       probs = (1 - conf.level) / 2,
                       na.rm = TRUE)
      uci_raw <- apply(mat, 2, quantile,
                       probs = 1 - (1 - conf.level) / 2,
                       na.rm = TRUE)
      lci <- interp_safe(ci_x, lci_raw, newdata)
      uci <- interp_safe(ci_x, uci_raw, newdata)
    }
    
    res <- data.frame(res, lci = lci, uci = uci)
  }
  
  res
}
