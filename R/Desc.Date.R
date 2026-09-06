#' Descriptive statistics for calendar date variables
#'
#' Computes a structured descriptive summary for objects of class
#' `"Date"`. The description focuses on time-axis characteristics
#' (range, span, coverage, quantiles) and distributional structure over
#' weekdays and months.
#'
#' In addition to core time-axis statistics, observed and expected
#' frequencies for weekdays and months are calculated together with
#' standardized residuals and chi-square p-values. The function also
#' performs heuristic detection of suspicious sentinel dates (e.g.,
#' extreme future or implausibly early values) to highlight potential
#' data-quality issues.
#'
#' @inheritParams desc
#'
#' @param wprobs numeric vector of length 7 specifying expected
#' probabilities for weekdays (Monday to Sunday). The default is a
#' uniform distribution `rep(1/7, 7)`.
#'
#' @param mprobs numeric vector of length 12 specifying expected
#' probabilities for months (January to December). If `NULL`
#' (default), probabilities proportional to the number of days per month
#' in a non-leap year are used.
#'
#' @details
#' The core time-axis summary includes:
#' \itemize{
#'   \item Number of observations and missing values
#'   \item Minimum and maximum date
#'   \item Span in days (`max - min`)
#'   \item Number of unique observed days
#'   \item Coverage: proportion of observed days relative to the total
#'         number of calendar days within the observed range
#'   \item Fundamental quantiles (5\%, 25\%, 50\%, 75\%, 95\%)
#'   \item Interquartile range (IQR) in days
#' }
#'
#' Weekday and month distributions are compared to their expected
#' probabilities using chi-square goodness-of-fit tests.
#'
#' Standardized residuals are defined as
#' \deqn{(Observed - Expected) / sqrt(Expected)}.
#' They describe the magnitude and direction of deviation from the
#' expected distribution.
#'
#' Sentinel detection is based on simple heuristics such as extremely
#' large future dates or implausibly early calendar dates. It is meant
#' as a diagnostic aid rather than a formal validation procedure.
#'
#' @return an object of class `c("Desc.Date", "Desc")` with components:
#' \describe{
#'   \item{`core`}{time-axis statistics}
#'   \item{`weekday`}{observed and expected weekday counts, standardized
#'     residuals, and p-value}
#'   \item{`month`}{observed and expected month counts, standardized
#'     residuals, and p-value}
#'   \item{`sentinel`}{heuristic data-quality diagnostics}
#'   \item{`meta`}{metadata}
#' }
#'
#' @seealso [desc()], [print.Desc.Date()]
#'
#' @method desc Date
#' @family desc
#' @concept data-description
#' @concept date-handling
#' @concept descriptive-statistics
#'
#' @export
desc.Date <- function(x,
                      main    = NULL,
                      plotit  = NULL,
                      verbose = NULL,
                      wprobs  = rep(1/7, 7),
                      mprobs  = rep(1/12, 12),
                      ...) {

  # ------------------------------
  # Basic handling

  nTotal <- length(x)
  ok     <- !is.na(x)
  xOk    <- x[ok]
  n      <- length(xOk)

  if (n == 0L)
    return(.descAllNA(x, deparse(substitute(x)), main, plotit, verbose))

  if (is.null(main))
    main <- deparse(substitute(x))

  # ------------------------------
  # Core statistics

  minD       <- min(xOk)
  maxD       <- max(xOk)
  span       <- as.numeric(maxD - minD)
  uniqueDays <- length(unique(xOk))
  coverage   <- uniqueDays / (span + 1)

  qProbs <- c(.05, .25, .50, .75, .95)
  qVals  <- as.Date(quantile(as.numeric(xOk), probs = qProbs, names = FALSE),
                    origin = "1970-01-01")
  names(qVals) <- c("q05", "q25", "median", "q75", "q95")

  iqrDays <- as.numeric(qVals["q75"] - qVals["q25"])

  # ------------------------------
  # Weekday distribution

  wdNum    <- as.integer(strftime(xOk, "%u"))   # 1 = Monday ... 7 = Sunday
  wdObs    <- tabulate(wdNum, nbins = 7)
  wdLevels <- weekdays(as.Date("2023-01-02") + 0:6)
  names(wdObs) <- wdLevels

  if (length(wprobs) != 7)
    stop("wprobs must have length 7.")
  wprobs <- wprobs / sum(wprobs)

  wdChi    <- suppressWarnings(chisq.test(wdObs, p = wprobs))
  wdExp    <- wdChi$expected
  wdStdres <- wdChi$stdres

  # ------------------------------
  # Month distribution

  moNum    <- as.integer(format(xOk, "%m"))
  moObs    <- tabulate(moNum, nbins = 12)
  refMonths <- as.Date(paste0("2023-", sprintf("%02d", 1:12), "-01"))
  moLevels  <- months(refMonths)
  names(moObs) <- moLevels

  if (is.null(mprobs)) {
    monthDays <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    mprobs    <- monthDays / sum(monthDays)
  }

  if (length(mprobs) != 12)
    stop("mprobs must have length 12.")
  mprobs <- mprobs / sum(mprobs)

  moChi    <- suppressWarnings(chisq.test(moObs, p = mprobs))
  moExp    <- moChi$expected
  moStdres <- moChi$stdres

  # ------------------------------
  # Sentinel detection

  sentinelFlag   <- FALSE
  sentinelReason <- NULL

  if (maxD > Sys.Date() + 365) {
    sentinelFlag   <- TRUE
    sentinelReason <- "Date far in future (possible open-ended coding)"
  }

  if (minD < as.Date("1900-01-01")) {
    sentinelFlag   <- TRUE
    sentinelReason <- "Very early date (possible default value)"
  }

  yrs <- as.integer(format(xOk, "%Y"))
  if (any(yrs %in% c(2099, 2999, 3000, 9999))) {
    sentinelFlag   <- TRUE
    sentinelReason <- "Suspicious sentinel year detected"
  }

  sentinelInfo <- list(
    flag   = sentinelFlag,
    reason = sentinelReason,
    lowest  = minD,
    highest = maxD
  )

  # ------------------------------
  # Assemble result

  res <- list(

    meta = .descMeta(x, deparse(substitute(x)), main, plotit, verbose),

    core = list(
      length     = nTotal,
      n          = n,
      nMissing   = nTotal - n,
      min        = minD,
      max        = maxD,
      span       = span,
      uniqueDays = uniqueDays,
      coverage   = coverage,
      quantiles  = qVals,
      iqrDays    = iqrDays
    ),

    weekday = list(
      observed = wdObs,
      expected = wdExp,
      stdres   = wdStdres,
      p.value  = wdChi$p.value
    ),

    month = list(
      observed = moObs,
      expected = moExp,
      stdres   = moStdres,
      p.value  = moChi$p.value
    ),

    sentinel = sentinelInfo
  )

  class(res) <- c("Desc.Date", "Desc")
  return(res)
}


#' Print method for `"Desc.Date"` objects
#'
#' Prints a structured summary of a `"Desc.Date"` object as created
#' by [desc()]. The output includes core time-axis statistics
#' such as range, span, coverage and fundamental quantiles.
#'
#' Depending on the selected verbosity level, weekday and month
#' distributions are shown including observed and expected counts,
#' standardized residuals and chi-square p-values.
#'
#' Standardized residuals are displayed together with a directional
#' deviation marker using "+" and "-" symbols. These markers reflect
#' the magnitude and direction of deviation from expectation but do not
#' represent separate hypothesis tests.
#'
#' @param x an object of class `"Desc.Date"`
#'
#' @param verbose integer controlling the amount of printed detail.
#' If `NULL`, the verbosity stored in the object metadata is used.
#'
#' \describe{
#'   \item{0--1}{core statistics only (range, span, coverage,
#'               quantiles, and sentinel detection)}
#'   \item{2}{additionally prints weekday distribution with observed
#'            counts, expected counts, standardized residuals and
#'            chi-square p-value}
#'   \item{3}{additionally prints month distribution}
#' }
#'
#' @param ... further arguments passed to underlying print methods
#'
#' @details
#' Weekday and month distributions are compared to their expected
#' probabilities as defined in [desc()].
#'
#' Standardized residuals are computed as
#' \deqn{(Observed - Expected) / sqrt(Expected)}.
#' Larger absolute values indicate stronger deviations from the expected
#' distribution.
#'
#' Sentinel values (e.g., extreme future dates such as 3000-01-01 or
#' implausibly early dates) are reported as potential data-quality issues.
#'
#' @method print Desc.Date
#' @export
print.Desc.Date <- function(x, verbose = NULL, ...) {

  .printHeader(x$meta)

  verbose <- verbose %||% x$meta$verbose %||% 2
  core    <- x$core

  # -------------------------
  # Core (always)

  m <- with(core, .printCore(length, n, nMissing, uniqueDays))
  m <- cbind(m,
             c("coverage", fm(core$coverage, fmt = "%", digits = 1), ""),
             c("span", gettextf("%s days", fm(core$span, fmt = "abs.sty")), ""))
  m <- rbind(m,
             rep("", ncol(m)),
             cbind(
               c("median", fm(core$quantiles["median"], fmt = "yyyy-MM-dd")),
               c("iqr",    gettextf("%s days", core$iqrDays)),
               c("range     ", format(core$min)),
               c("", paste0("- ", format(core$max))),
               rep("", 2),
               rep("", 2)
             ))

  printCharMatrix(m, align = "right", sep = 2, showRownames = FALSE, useCliStyle = TRUE)
  cat("\n")

  if (x$sentinel$flag)
    cat("\n\u26A0 Sentinel detected:", x$sentinel$reason, "\n")

  if (verbose < 2)
    return(invisible(x))

  # -------------------------
  # Weekday block

  cat("\nWeekday distribution:\n\n")

  wd  <- x$weekday
  out <- data.frame(
    level  = names(wd$observed),
    obs    = wd$observed,
    perc   = fm(proportions(wd$observed), fmt = "%", digits = 1),
    stdres = round(wd$stdres, 2),
    dev    = .residMark(wd$stdres),
    row.names = NULL
  )
  out <- cbind(out[, 1:4], " ", out[, 5])
  colnames(out) <- c("level", "obs", "perc", "stdres", "", "dev")
  printCharMatrix(out, align = c(rep("right", 5), "left"), showRownames = FALSE, useCliStyle = TRUE)
  .printFootnote(gettextf("Chi-squared p-value: %s", fm(wd$p.value, fmt = "p")))

  if (verbose < 3)
    return(invisible(x))

  # -------------------------
  # Month block

  cat("\nMonth distribution:\n\n")

  mo   <- x$month
  out2 <- data.frame(
    level  = names(mo$observed),
    obs    = as.numeric(mo$observed),
    perc   = fm(proportions(mo$observed), fmt = "%", digits = 1),
    stdres = round(mo$stdres, 2),
    dev    = .residMark(mo$stdres),
    row.names = NULL
  )
  out2 <- cbind(out2[, 1:4], " ", out2[, 5])
  colnames(out2) <- c("level", "obs", "perc", "stdres", "", "dev")
  printCharMatrix(out2, align = c(rep("right", 5), "left"), showRownames = FALSE, useCliStyle = TRUE)
  .printFootnote(gettextf("Chi-squared p-value: %s", fm(mo$p.value, fmt = "p")))

  invisible(x)
}


# == internal helper functions ================================================

.residMark <- function(z) {
  cut(z,
      breaks = c(-Inf, -3, -2, -1.5, 1.5, 2, 3, Inf),
      labels = c("---", "--", "-", "", "+", "++", "+++"),
      right  = FALSE)
}

.printCore <- function(nTotal, n, nMissing, uniqueN) {
  m <- rbind(
    c("length", "n", "NAs", "unique"),
    fm(c(nTotal, n, nMissing, uniqueN), fmt = style("abs.sty")),
    c("",
      fm(n        / nTotal, fmt = "%", digits = 1),
      fm(nMissing / nTotal, fmt = "%", digits = 1),
      "")
  )
  return(m)
}


# .printMatrix <- function(m, justify = "left", padding = 2) {
# 
#   m      <- as.matrix(m)
#   mChar  <- apply(m, 2, as.character)
#   ncols  <- ncol(mChar)
# 
#   if (length(justify) == 1)
#     justify <- rep(justify, ncols)
#   if (length(justify) != ncols)
#     stop("Length of 'justify' must be 1 or equal to number of columns.")
# 
#   colWidths <- sapply(seq_len(ncols), function(j) {
#     max(nchar(c(colnames(mChar)[j], mChar[, j])))
#   })
# 
#   if (ncols > 1)
#     colWidths[1:ncols] <- colWidths[1:ncols] + padding
# 
#   header <- mapply(function(x, w, j) format(x, width = w, justify = j),
#                    colnames(mChar), colWidths, justify)
#   cat(cli::style_bold(paste(header, collapse = ""), "\n"))
# 
#   for (i in seq_len(nrow(mChar))) {
#     row <- mapply(function(x, w, j) format(x, width = w, justify = j),
#                   mChar[i, ], colWidths, justify)
#     cat(paste(row, collapse = ""), "\n")
#   }
# 
#   invisible(NULL)
# }



.printFootnote <- function(x, len = 20, ...) {
  cat(gettextf("\n%s\n%s \n\n", strrep("\u2500", 20), x))
}
