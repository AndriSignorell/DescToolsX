
# == shared infrastructure for skew() and kurt() ===============================
#
# Both functions are the same machine with a different moment: argument
# checks, data preparation, the rule for undefined estimates, the Wald
# interval, the bootstrap and `sides` live here once. Each function only
# supplies its kernel FUN(x, w, n, estimator) -> c(est, se), see .i.skew()
# and .i.kurt().
#
# Weight semantics (documented in both Rd pages): weights are FREQUENCIES.
# For integer weights a weighted call equals the unweighted call on
# rep(x, weights); sum(weights) is the sample size for the bias corrections
# and the normal-theory SE, and the bootstrap resamples the expanded sample.
# Non-integer weights are a computational extension of the same formulas
# for the point estimate and the classic interval (no exact sampling
# interpretation), and rejected for the bootstrap.


# bootstrap arguments accepted through `...`; anything else is an error
# rather than being dropped: the statistic relies on ordinary index
# resampling, so e.g. sim = "parametric" or strata cannot simply be passed on
.shapeBootArgs <- c("R", "type", "parallel", "ncpus")

# boot.ci() component per type; "stud" is deliberately absent (see .shapeBoot)
.shapeBootTypes <- c(norm = "normal", basic = "basic",
                     perc = "percent", bca = "bca")


.shape <- function(FUN, x, conf.level, sides, method, estimator,
                   weights, na.rm, ...) {

  .shapeCheckArgs(x, weights, estimator, na.rm, conf.level)

  ci <- !is.na(conf.level)

  if (ci && sides != "two.sided") {
    # a one-sided level p is the two-sided level 2p - 1, which must be > 0
    if (conf.level <= 0.5)
      stop("one-sided intervals require 'conf.level' > 0.5")
    conf.level <- 2 * conf.level - 1
  }

  undef <- if (ci) c(est = NA_real_, lci = NA_real_, uci = NA_real_)
           else NA_real_

  d <- .shapeData(x, weights, na.rm)

  # NAs present and na.rm = FALSE: NA, silently, as mean(c(1, NA))
  if (is.null(d))
    return(undef)

  r <- FUN(d$x, d$w, d$n, estimator)

  if (is.na(r[["est"]])) {
    warning(attr(r, "reason"), "; returning NA", call. = FALSE)
    return(undef)
  }

  if (!ci)
    return(r[["est"]])

  res <- switch(method,

    classic = {
      if (is.na(r[["se"]]))
        warning("normal-theory standard error undefined for n = ",
                format(d$n), "; interval is NA", call. = FALSE)
      z <- qnorm(1 - (1 - conf.level) / 2)
      c(est = r[["est"]],
        lci = r[["est"]] - z * r[["se"]],
        uci = r[["est"]] + z * r[["se"]])
    },

    boot = .shapeBoot(
      d,
      stat = function(x) FUN(x, NULL, length(x), estimator)[["est"]],
      est = r[["est"]],
      conf.level = conf.level,
      ...
    )
  )

  # both moments are unbounded, so the open side is +-Inf
  if (sides == "left")
    res[["uci"]] <- Inf
  else if (sides == "right")
    res[["lci"]] <- -Inf

  res
}


.shapeCheckArgs <- function(x, weights, estimator, na.rm, conf.level) {

  # an all-NA vector is logical (rep(NA, n)) and must give NA, not an error
  if (!is.numeric(x) && !(is.logical(x) && all(is.na(x))))
    stop("'x' must be numeric")

  if (!is.null(weights)) {
    if (!is.numeric(weights))
      stop("'weights' must be numeric")
    if (length(weights) != length(x))
      stop("'weights' must have same length as 'x'")
  }

  # numeric, because the kernels dispatch with a positional switch():
  # estimator = "2" would look for a branch NAMED "2" and return NULL.
  # Length first: %in% is vectorised.
  if (!is.numeric(estimator) || length(estimator) != 1L ||
      is.na(estimator) || !estimator %in% c(1, 2, 3))
    stop("'estimator' must be one of 1, 2, or 3")

  if (!is.logical(na.rm) || length(na.rm) != 1L || is.na(na.rm))
    stop("'na.rm' must be a single non-missing logical value")

  # conf.level switches between the two return shapes, so it must be a
  # single value before is.na() can be asked
  if (length(conf.level) != 1L ||
      (!is.na(conf.level) &&
       (!is.numeric(conf.level) || conf.level <= 0 || conf.level >= 1)))
    stop("'conf.level' must be a single number in (0, 1), or NA")

  invisible(NULL)
}


# Returns list(x, w, n), or NULL if missing values are present and
# na.rm = FALSE. x and w are cleaned TOGETHER and once, so the point
# estimate, the classic interval and every bootstrap replicate see the
# same data.
.shapeData <- function(x, weights, na.rm) {

  # a matrix contributes all its elements, as documented. Without this the
  # point path flattened it while boot() resampled ROWS and x[d] indexed
  # linearly; na.omit() would also have dropped whole rows.
  x <- as.vector(x)

  if (!is.null(weights)) {

    weights <- as.vector(weights)
    known <- !is.na(weights)

    if (any(!is.finite(weights[known])) || any(weights[known] < 0))
      stop("'weights' must be finite and non-negative")

    if (any(known) && all(weights[known] == 0))
      stop("'weights' must have a positive sum")

    # a zero frequency is an absent observation, so it goes FIRST: its x may
    # be NA or Inf without effect, it cannot be drawn by the bootstrap and
    # does not count in the constancy check. A missing weight stays a
    # missing value, handled below.
    drop <- known & weights == 0
    x <- x[!drop]
    weights <- weights[!drop]
  }

  miss <- is.na(x)
  if (!is.null(weights))
    miss <- miss | is.na(weights)

  if (any(miss)) {
    if (!na.rm)
      return(NULL)
    x <- x[!miss]
    if (!is.null(weights))
      weights <- weights[!miss]
  }

  if (any(!is.finite(x)))
    stop("'x' must not contain infinite values")

  n <- if (is.null(weights)) length(x) else sum(weights)

  # finite weights need not have a finite sum
  if (!is.finite(n))
    stop("the sum of 'weights' is not finite")

  list(x = x, w = weights, n = n)
}


# undefined estimate: NA with the reason .shape() turns into the warning
.shapeUndefined <- function(reason)
  structure(c(est = NA_real_, se = NA_real_), reason = reason)


# Bootstrap interval. d comes from .shapeData(), stat maps an unweighted
# sample to the estimate, est is the already computed point estimate.
.shapeBoot <- function(d, stat, est, conf.level, ...) {

  dots <- list(...)
  bad <- setdiff(names(dots), .shapeBootArgs)
  if (length(bad) || (length(dots) && is.null(names(dots))))
    stop("unsupported argument(s) in '...': ",
         paste(if (length(bad)) bad else "<unnamed>", collapse = ", "),
         ". Supported: ", paste(.shapeBootArgs, collapse = ", "))

  args <- .extractBootArgs(dots)

  # "stud" needs a variance estimate for every replicate, and no suitable
  # one is implemented: the normal-theory variance would smuggle back the
  # assumption the bootstrap is meant to avoid.
  if (!args$type %in% names(.shapeBootTypes))
    stop("bootstrap type \"", args$type, "\" is not supported; use one of ",
         paste0("\"", names(.shapeBootTypes), "\"", collapse = ", "))

  # frequency weights: resample the expanded sample, so that the bootstrap
  # draws sum(weights) units, not length(x) observation-weight pairs
  x <- d$x
  if (!is.null(d$w)) {
    if (any(abs(d$w - round(d$w)) > sqrt(.Machine$double.eps)))
      stop("bootstrap intervals require integer frequency weights ",
           "(see the Details on non-integer weights)")
    x <- rep.int(x, round(d$w))
  }

  na <- c(est = est, lci = NA_real_, uci = NA_real_)

  b <- boot::boot(x, function(x, i) stat(x[i]),
                  R        = args$R,
                  parallel = args$parallel,
                  ncpus    = args$ncpus)

  nbad <- sum(!is.finite(b$t[, 1L]))
  if (nbad) {
    warning(nbad, " of ", length(b$t[, 1L]), " bootstrap replicates are ",
            "undefined (constant resample); interval is NA", call. = FALSE)
    return(na)
  }

  # errors become NA with a warning; boot.ci()'s own warnings (e.g. extreme
  # order statistics used as endpoints) pass through untouched
  ci <- tryCatch(
    boot::boot.ci(b, conf = conf.level, type = args$type),
    error = function(e) {
      warning("bootstrap interval could not be computed: ",
              conditionMessage(e), call. = FALSE)
      NULL
    }
  )

  if (is.null(ci))
    return(na)

  # normal: conf, lower, upper; others: conf, 2 order stats, lower, upper
  m <- ci[[.shapeBootTypes[[args$type]]]]
  if (!is.matrix(m) || !ncol(m) %in% c(3L, 5L)) {
    warning("boot.ci() returned no \"", args$type, "\" interval; ",
            "interval is NA", call. = FALSE)
    return(na)
  }

  c(est = est,
    lci = unname(m[1L, ncol(m) - 1L]),
    uci = unname(m[1L, ncol(m)]))
}
