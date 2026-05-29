
#' Lorenz Curve Estimation and Visualization
#'
#' \code{lc()} computes the empirical Lorenz curve for a numeric vector.
#' It returns an object of class \code{"lc"} which can be visualized using
#' \code{plot()}, \code{lines()}, and \code{points()}.
#'
#' The Lorenz curve represents the cumulative proportion of a variable
#' (e.g., income) as a function of the cumulative population share.
#'
#' @name lc
#'
#' @param x A numeric vector of non-negative values.
#' @param n Optional vector of non-negative weights of the same length as \code{x}.
#'   Defaults to equal weights.
#' @param na.rm Logical. Should missing values be removed?
#'
#' @param formula A formula of the form \code{y ~ group} for grouped Lorenz curves.
#' @param data Optional data frame.
#' @param subset Optional subset expression.
#' @param na.action Function to handle missing values.
#'
#' @param object Object of class \code{"lc"}.
#' @param newdata Optional numeric vector of values in \eqn{[0,1]} at which to
#'   evaluate the Lorenz curve.
#' @param conf.level Confidence level for bootstrap confidence intervals.
#' @param general Logical. If \code{TRUE}, the generalized Lorenz curve is used.
#' @param n Number of bootstrap replications.
#'
#' @param main,xlab,ylab Graphical parameters passed to \code{plot()}.
#' @param xlim,ylim Axis limits.
#' @param col,lwd,lty Graphical parameters for line drawing.
#' @param pch Plotting symbol for points.
#' @param grid Logical or list. Draw grid lines.
#' @param box Logical. Draw box around the plot.
#' @param cband Logical or list. Draw confidence band in \code{lines()}.
#' @param stamp Optional annotation handled by the graphics framework.
#' @param ... Further arguments passed to underlying methods.
#'
#' @return
#' \itemize{
#'   \item \code{lc()} returns an object of class \code{"lc"} with components:
#'     \describe{
#'       \item{p}{Cumulative population share}
#'       \item{L}{Lorenz curve}
#'       \item{L.general}{Generalized Lorenz curve}
#'       \item{Gini}{Gini coefficient}
#'       \item{x}{Original data}
#'       \item{n}{Weights}
#'     }
#'   \item \code{lc.formula()} returns either a single \code{"lc"} object or a
#'     list of such objects of class \code{"lclist"} for grouped data.
#'   \item \code{predict.lc()} returns a data frame with columns \code{p} and
#'     \code{L}, optionally including \code{lci} and \code{uci}.
#' }
#'
#' @details
#' The Lorenz curve is defined as
#'
#' \deqn{
#' L(p) = \frac{\sum_{i=1}^{k} w_i x_i}{\sum_{i=1}^{n} w_i x_i}
#' }
#'
#' where observations are ordered increasingly and \eqn{p} denotes the cumulative
#' proportion of weights.
#'
#' The generalized Lorenz curve scales the Lorenz curve by the mean:
#'
#' \deqn{
#' L_{general}(p) = L(p) \cdot \mu
#' }
#'
#' where \eqn{\mu} is the weighted mean of \code{x}.
#'
#' For formula input, data are split by group and separate Lorenz curves
#' are computed.
#'
#' Bootstrap confidence intervals in \code{predict()} and \code{lines()}
#' are based on resampling with replacement.
#'
#' @examples
#' set.seed(1)
#' x <- rlnorm(100)
#'
#' # basic Lorenz curve
#' lc_obj <- lc(x)
#' plot(lc_obj)
#'
#' # add points
#' points(lc_obj, pch = 16)
#'
#' # add confidence band
#' lines(lc_obj, conf.level = 0.95, cband = TRUE)
#'
#' # grouped Lorenz curves
#' g <- sample(letters[1:3], 100, replace = TRUE)
#' lc_grp <- lc(x ~ g)
#'
#' plot(lc_grp)
#' lines(lc_grp)
#'
#' @references
#' Lorenz, M. O. (1905).
#' Methods of measuring the concentration of wealth.
#' \emph{Publications of the American Statistical Association}.
#'
#' @seealso \code{\link{gini}}, \code{\link{predict.lc}}
#'
#' @family inequality
#' @concept descriptive-statistics
#' @concept inequality
NULL


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
    
    class(res) <- c("lclist", "list")
    
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
  class(lc) <- "lc"
  
  lc
}



#' @rdname lc
#' @export
predict.lc <- function(object, newdata, conf.level = NA, general = FALSE, n = 1000, ...) {
  
  if (!inherits(object, "lc"))
    stop("object must be of class 'lc'")
  
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
    if (sum(ok) < 2) {
      return(rep(NA_real_, length(xout)))
    }
    approx(x[ok], y[ok], xout = xout, rule = 2)$y
  }
  
  # --- confidence interval ---
  if (!is.na(conf.level)) {
    
    if (!is.numeric(conf.level) || length(conf.level) != 1 ||
        conf.level <= 0 || conf.level >= 1)
      stop("conf.level must be a single number in (0,1)")
    
    # --- reconstruct weighted sample ---
    wsum <- sum(object$n)
    
    if (wsum == 0 || length(object$x) == 0) {
      lci <- uci <- rep(NA_real_, length(newdata))
      return(data.frame(res, lci = lci, uci = uci))
    }
    
    x_full <- sample(
      object$x,
      size = wsum,
      replace = TRUE,
      prob = object$n
    )
    
    # --- bootstrap ---
    lst <- replicate(n, lc(x_full, ...), simplify = FALSE)
    
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
      
      # --- compute quantiles ---
      lci_raw <- apply(mat, 2, quantile,
                       probs = (1 - conf.level) / 2,
                       na.rm = TRUE)
      
      uci_raw <- apply(mat, 2, quantile,
                       probs = 1 - (1 - conf.level) / 2,
                       na.rm = TRUE)
      
      # --- interpolate safely ---
      lci <- interp_safe(ci_x, lci_raw, newdata)
      uci <- interp_safe(ci_x, uci_raw, newdata)
    }
    
    res <- data.frame(res, lci = lci, uci = uci)
  }
  
  res
}



#' @rdname lc
#' @export
plot.lc <- function(
    
  # DATA
  x,
  
  # LABELS
  main = "Lorenz curve",
  xlab = "p",
  ylab = "L(p)",
  
  # AXES
  xlim = NULL,
  ylim = NULL,
  
  # STRUCTURE
  general = FALSE,
  
  # STYLE
  col = NULL,
  lwd = 2,
  lty = 1,
  pch = NULL,
  grid = FALSE,
  box = TRUE,
  
  # FRAMEWORK
  stamp = NULL,
  
  ...
) {
  
  if (!inherits(x, "lc"))
    stop("x must be of class 'lc'")
  
  .withGraphicsState({
    
    .applyParFromDots(...)
    
    # --- data selection ---
    L <- if (!general) x$L else x$L.general
    p <- x$p
    
    # --- axis limits ---
    if (is.null(xlim)) xlim <- c(0, 1)
    if (is.null(ylim)) ylim <- c(0, 1)
    
    # --- base plot ---
    plot(
      p, L,
      type = "n",
      main = main,
      xlab = xlab,
      ylab = ylab,
      xlim = xlim,
      ylim = ylim
    )
    
    # --- grid ---
    callIf(
      graphics::grid,
      grid,
      defaults = list(col = "grey90", lty = 1)
    )

    # --- Lorenz curve ---
    lines(p, L, col = col, lwd = lwd, lty = lty)
    
    # --- equality line ---
    abline(0, 1, col = "grey50", lty = 2)
    
    # --- points ---
    if (!is.null(pch)) {
      points(p, L, pch = pch, col = col)
    }
    
    # --- box ---
    if (isTRUE(box)) box()
    
  }, stamp = stamp)
}


#' @rdname lc
#' @export
lines.lc <- function(
    
  # DATA
  x,
  
  # STRUCTURE
  general = FALSE,
  
  # STYLE
  col = NULL,
  lwd = 2,
  lty = 1,
  
  # FEATURES
  conf.level = NA,
  cband = FALSE,
  
  ...
) {
  
  if (!inherits(x, "lc"))
    stop("x must be of class 'lc'")
  
  # --- select curve ---
  L <- if (!general) x$L else x$L.general
  
  # --- confidence band ---
  if (!is.na(conf.level)) {
    
    if (!is.numeric(conf.level) || length(conf.level) != 1 ||
        conf.level <= 0 || conf.level >= 1)
      stop("conf.level must be a number in (0,1)")
    
    ci <- predict(x, conf.level = conf.level, general = general)
    
    defaults <- list(
      col = grDevices::adjustcolor("black", alpha.f = 0.12),
      border = NA
    )
    
    band_args <- list(
      x = c(ci$p, rev(ci$p)),
      y = c(ci$lci, rev(ci$uci))
    )

    callIf(
        drawBand,
        cband,
        defaults = modifyList(defaults, band_args),
        forbidden = c("x", "y")
      )

  }
  
  # --- draw line ---
  lines(x$p, L, col = col, lwd = lwd, lty = lty, ...)
  
  invisible(NULL)
}


#' @rdname lc
#' @export
points.lc <- function(
    
  # DATA
  x,
  
  # STRUCTURE
  general = FALSE,
  
  # STYLE
  pch = 16,
  col = NULL,
  
  ...
) {
  
  if (!inherits(x, "lc"))
    stop("x must be of class 'lc'")
  
  # --- select curve ---
  L <- if (!general) x$L else x$L.general
  
  # --- draw points ---
  points(x$p, L, pch = pch, col = col, ...)
  
  invisible(NULL)
}



#' @rdname lc
#' @export
lines.lclist <- function(x, col = NULL, ...) {
  
  k <- length(x)
  
  if (is.null(col))
    col <- seq_len(k)
  
  for (i in seq_along(x)) {
    lines(x[[i]], col = col[i], ...)
  }
  
  invisible(NULL)
}



#' @rdname lc
#' @export
points.lclist <- function(x, col = NULL, ...) {
  
  k <- length(x)
  
  if (is.null(col))
    col <- seq_len(k)
  
  for (i in seq_along(x)) {
    points(x[[i]], col = col[i], ...)
  }
  
  invisible(NULL)
}



#' @rdname lc
#' @export
plot.lclist <- function(x, col = NULL, ...) {
  
  k <- length(x)
  
  if (k == 0)
    stop("empty lclist")
  
  if (is.null(col))
    col <- seq_len(k)
  
  plot(x[[1]], col = col[1], ...)
  
  if (k > 1) {
    for (i in 2:k) {
      lines(x[[i]], col = col[i], ...)
    }
  }
  
  invisible(NULL)
}

