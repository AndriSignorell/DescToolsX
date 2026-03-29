
#' Descriptive statistics for calendar date variables
#'
#' Computes a structured descriptive summary for objects of class
#' \code{"Date"}. The description focuses on time-axis characteristics
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
#' @inheritParams Desc
#'
#' @param wprobs Numeric vector of length 7 specifying expected
#' probabilities for weekdays (Monday to Sunday). The default is a
#' uniform distribution \code{rep(1/7, 7)}.
#'
#' @param mprobs Numeric vector of length 12 specifying expected
#' probabilities for months (January to December). If \code{NULL}
#' (default), probabilities proportional to the number of days per month
#' in a non-leap year are used.
#'
#' @details
#' The core time-axis summary includes:
#' \itemize{
#'   \item Number of observations and missing values
#'   \item Minimum and maximum date
#'   \item Span in days (\code{max - min})
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
#' @return
#' An object of class \code{"Desc.Date"} and \code{"Desc"} containing:
#' \itemize{
#'   \item \code{core}: time-axis statistics
#'   \item \code{weekday}: observed counts, expected counts,
#'         standardized residuals and p-value
#'   \item \code{month}: observed counts, expected counts,
#'         standardized residuals and p-value
#'   \item \code{sentinel}: heuristic data-quality diagnostics
#'   \item \code{meta}: metadata information
#' }
#'
#' @seealso \code{\link{Desc}}, \code{\link{print.Desc.Date}},
#'   \code{\link{plot.Desc}}
#'


#' @method Desc Date
#' @export
Desc.Date <- function(x,
                      main = NULL,
                      plotit = NULL,
                      verbose = NULL,
                      wprobs = rep(1/7, 7),
                      mprobs = rep(1/12, 12),
                      ...) {
  
  # ------------------------------
  # Basic handling
  
  total_n <- length(x)
  ok <- !is.na(x)
  x_ok <- x[ok]
  n <- length(x_ok)
  
  if(n == 0)
    stop("No valid (non-NA) dates available.")
  
  if(is.null(main))
    main <- deparse(substitute(x))
  
  # ------------------------------
  # Core statistics
  
  min_d <- min(x_ok)
  max_d <- max(x_ok)
  span <- as.numeric(max_d - min_d)
  
  unique_days <- length(unique(x_ok))
  coverage <- unique_days / (span + 1)
  
  q_probs <- c(.05, .25, .50, .75, .95)
  q_vals <- as.Date(quantile(as.numeric(x_ok), probs = q_probs, names = FALSE),
                    origin = "1970-01-01")
  
  names(q_vals) <- c("q05","q25","median","q75","q95")
  
  iqr_days <- as.numeric(q_vals["q75"] - q_vals["q25"])
  
  # ------------------------------
  # Weekday distribution
  
  wd_num <- as.integer(strftime(x_ok, "%u"))  # 1 = Montag ... 7 = Sonntag
  wd_obs <- tabulate(wd_num, nbins = 7)
  wd_levels <- weekdays(as.Date("2023-01-02") + 0:6)
  names(wd_obs) <- wd_levels

  if(length(wprobs) != 7)
    stop("wprobs must have length 7.")
  
  wprobs <- wprobs / sum(wprobs)
  
  wd_chi <- suppressWarnings(chisq.test(wd_obs, p = wprobs))
  wd_exp <- wd_chi$expected
  wd_stdres <- wd_chi$stdres
  
  # ------------------------------
  # month distribution
  
  mo_num <- as.integer(format(x_ok, "%m"))
  mo_obs <- tabulate(mo_num, nbins = 12)
  ref_months <- as.Date(paste0("2023-", sprintf("%02d", 1:12), "-01"))
  mo_levels <- months(ref_months)
  names(mo_obs) <- mo_levels
  
  if(is.null(mprobs)) {
    month_days <- c(31,28,31,30,31,30,31,31,30,31,30,31)
    mprobs <- month_days / sum(month_days)
  }
  
  if(length(mprobs) != 12)
    stop("mprobs must have length 12.")
  
  mprobs <- mprobs / sum(mprobs)
  
  mo_chi <- suppressWarnings(chisq.test(mo_obs, p = mprobs))
  mo_exp <- mo_chi$expected
  mo_stdres <- mo_chi$stdres
  
  # ------------------------------
  # Sentinel detection
  
  sentinel_flag <- FALSE
  sentinel_reason <- NULL
  
  if(max_d > Sys.Date() + 365) {
    sentinel_flag <- TRUE
    sentinel_reason <- "Date far in future (possible open-ended coding)"
  }
  
  if(min_d < as.Date("1900-01-01")) {
    sentinel_flag <- TRUE
    sentinel_reason <- "Very early date (possible default value)"
  }
  
  # explicit suspicious years
  yrs <- as.integer(format(x_ok, "%Y"))
  if(any(yrs %in% c(2099, 2999, 3000, 9999))) {
    sentinel_flag <- TRUE
    sentinel_reason <- "Suspicious sentinel year detected"
  }
  
  sentinel_info <- list(
    flag = sentinel_flag,
    reason = sentinel_reason,
    lowest = min_d,
    highest = max_d
  )
  
  # ------------------------------
  # Assemble result
  
  res <- list(
    
    meta = .descMeta(x, deparse(substitute(x)), main, plotit, verbose),
    
    core = list(
      length = total_n,
      n = n,
      NAs = total_n - n,
      min = min_d,
      max = max_d,
      span = span,
      unique_days = unique_days,
      coverage = coverage,
      quantiles = q_vals,
      IQR_days = iqr_days
    ),
    
    weekday = list(
      observed = wd_obs,
      expected = wd_exp,
      stdres = wd_stdres,
      p.value = wd_chi$p.value
    ),
    
    month = list(
      observed = mo_obs,
      expected = mo_exp,
      stdres = mo_stdres,
      p.value = mo_chi$p.value
    ),
    
    sentinel = sentinel_info

  )
  
  class(res) <- c("Desc.Date","Desc")
  
  return(res)
}



#' Print method for \code{"Desc.Date"} objects
#'
#' Prints a structured summary of a \code{"Desc.Date"} object as created
#' by \code{\link{Desc}}. The output includes core time-axis statistics
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
#' @param x An object of class \code{"Desc.Date"}.
#'
#' @param verbose Integer controlling the amount of printed detail.
#' If \code{NULL}, the verbosity stored in the object metadata is used.
#'
#' \describe{
#'   \item{0--1}{Core statistics only (range, span, coverage,
#'               quantiles, sentinel detection).}
#'   \item{2}{Additionally prints weekday distribution with observed
#'            counts, expected counts, standardized residuals and
#'            chi-square p-value.}
#'   \item{3}{Additionally prints month distribution.}
#' }
#'
#' @param ... Further arguments passed to underlying print methods.
#'
#' @details
#' Weekday and month distributions are compared to their expected
#' probabilities as defined in \code{\link{Desc}}.
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
  
  core <- x$core
  
  # -------------------------
  # Core (immer)
  
  m <- with(core, .printCore(length, n, NAs, unique_days))
  m <- cbind(m, 
             c("coverage", fm(core$coverage, fmt="%", digits=1),""),
             c("span", gettextf("%s days", fm(core$span, fmt="abs.sty")),""))
  m <- rbind(m,
             rep("", ncol(m)),
            cbind(c("median", fm(core$quantiles["median"], fmt="yyyy-MM-dd")), 
             c("iqr", gettextf("%s days", core$IQR_days)),
             c("range     ",format(core$min)),c("", paste0("- ", format(core$max))),
             rep("", 2),rep("", 2)))
 
  .printMatrix(m, justify="right", padding = 2)
  
  cat("\n")
  
  # Sentinel warning
  if(x$sentinel$flag) {
    cat("\n\u26A0 Sentinel detected:",
        x$sentinel$reason, "\n")
  }
  
  if(verbose < 2)
    return(invisible(x))
  
  # -------------------------
  # Weekday block
  
  cat("\nWeekday distribution:\n\n")
  
  wd <- x$weekday

  out <- data.frame(
    level = names(wd$observed),
    obs = wd$observed,
    perc = fm(proportions(wd$observed), fmt="%", digits=1),
    stdres = round(wd$stdres, 2),
    dev = .residMark(wd$stdres),
    row.names = NULL
  )
  
  out <- cbind(out[, 1:4], " ", out[, 5])
  colnames(out) <- c("level","obs","perc","stdres", "", "dev")
  .printMatrix(out, justify = c(rep("right", 5), "left"))
  
  .printFootnote(gettextf("Chi-squared p-value: %s", 
                          fm(wd$p.value, fmt="p")))

  if(verbose < 3)
    return(invisible(x))
  
  # -------------------------
  # month block
  
  cat("\nMonth distribution:\n\n")
  
  mo <- x$month
  
  out2 <- data.frame(
    level = names(mo$observed),
    obs = as.numeric(mo$observed),
    perc = fm(proportions(mo$observed), fmt="%", digits=1),
    stdres = round(mo$stdres, 2),
    dev = .residMark(mo$stdres),
    row.names = NULL
  )
  
  out2 <- cbind(out2[, 1:4], " ", out2[, 5])
  colnames(out2) <- c("level","obs","perc","stdres", "", "dev")
  .printMatrix(out2, justify = c(rep("right", 5), "left"))
  
  .printFootnote(gettextf("Chi-squared p-value: %s", 
                          fm(mo$p.value, fmt="p")))

  invisible(x)
  

}


# == internal helper functions =======================================

.residMark <- function(z) {
  cut(z,
      breaks=c(-Inf, -3,-2,-1.5,1.5,2,3,Inf),
      labels = c("---","--","-","", "+", "++", "+++"),
      right = FALSE)
}



.printCore <- function(tot_n, valid_n, na_n, unique_n){
  
  m <- rbind(
    c("length", "n", "NAs", "unique"),
    fm(c(tot_n, valid_n, na_n, unique_n), fmt = style("abs.sty")),
    c(
      "", fm(valid_n / tot_n, fmt = "%", digits = 1), 
      fm(na_n / tot_n, fmt = "%", digits = 1), ""
    ))

  return(m)
  
}


.printMatrix <- function(m,
                        justify = "left",
                        padding = 2) {
  
  m <- as.matrix(m)
  m_char <- apply(m, 2, as.character)
  
  ncols <- ncol(m_char)
  
  # justify-Recycling
  if(length(justify) == 1)
    justify <- rep(justify, ncols)
  
  if(length(justify) != ncols)
    stop("Length of 'justify' must be 1 or equal to number of columns.")
  
  # Spaltenbreiten berechnen
  col_widths <- sapply(seq_len(ncols), function(j) {
    max(nchar(c(colnames(m_char)[j], m_char[, j])))
  })
  
  # add padding (nicht bei letzter Spalte)
  if(ncols > 1)
    col_widths[1:(ncols)] <- col_widths[1:(ncols)] + padding
  
  # Header drucken
  header <- mapply(function(x, w, j)
    format(x, width = w, justify = j),
    colnames(m_char), col_widths, justify)
  
  cat(cli::style_bold(paste(header, collapse = ""), "\n"))
  
  
  # Zeilen drucken
  for(i in seq_len(nrow(m_char))) {
    
    row <- mapply(function(x, w, j)
      format(x, width = w, justify = j),
      m_char[i, ], col_widths, justify)
    
    cat(paste(row, collapse = ""), "\n")
  }
  
  invisible(NULL)
}


.printFootnote <- function(x, len=20, ...){
  
  cat(gettextf("\n%s\n%s \n\n", 
               strrep("\u2500", 20), x))
  
}

