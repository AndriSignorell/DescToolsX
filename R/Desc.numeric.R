
#' Describe a Numeric Variable
#'
#' Compute descriptive statistics for a numeric vector. The plot method may
#' display a histogram, density curve, box plot, and empirical distribution.
#'
#' 
#' 
#' @name desc.numeric
#' @inheritParams desc 
#' 
#' @param x numeric vector to describe, or an object of class
#' `"Desc.numeric"` for the print and plot methods
#' 
#' @param ... further arguments passed to methods
#'   
#' @param maxrows numeric; defines the maximum number of rows in a frequency
#' table to be reported. For factors with many levels it is often not
#' interesting to see all of them. Default is set to 12 most frequent ones
#' (resp. the first ones if `ord` is set to `"levels"` or
#' `"names"`).
#' 
#' @param digits number of digits used to format numeric values
#' 
#' @param include_x logical; if `TRUE`, the original vector is retained
#' in the result
#' @param conf.level confidence level for interval estimates (default 0.95)
#' 
#' @details
#' This function is an S3 method for [graphics::plot()].
#' It is automatically dispatched when calling `plot(x)` on a
#' `Desc.numeric` object.
#' 
#' For a numeric argument x `maxrows` is the minimum number of unique
#' values needed for a numeric variable to be treated as continuous. If left to
#' its default `NULL`, x will be regarded as continuous if it has more
#' than 12 single values. In this case the list of extreme values will be
#' displayed and the frequency table else.
#' 
#' If `maxrows` is < 1 it will be interpreted as percentage. In this case
#' just as many rows, as the `maxrows` most frequent levels will be shown.
#' Say, if `maxrows` is set to `0.8`, then the number of rows is
#' fixed so, that the highest cumulative relative frequency is the first one
#' going beyond 0.8.
#' 
#' Setting `maxrows` to `Inf` will unconditionally report all values
#' and also produce a plot with type "h" instead of a histogram.
#'
#' Named colors defined by `DescToolsX` (e.g. `"hred"`,
#' `"hblue"`) can be used directly.
#'
#' @return an object of class `c("Desc.numeric", "Desc")` containing
#' descriptive statistics, frequency information, and metadata
#'
#' @seealso [base::summary()],
#' [base::plot()]
#' 
#' Other Statistical summary functions: [abstract()]
#' @examples
#'
#' desc(Pizza$delivery_min)             # numeric
#'
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @rdname desc.numeric
#' @method desc numeric
#' @export
desc.numeric <- function(x, maxrows = NULL, conf.level = 0.95,
                         include_x = TRUE,
                         main = NULL, verbose = NULL, plotit = NULL,
                         digits = NULL,
                         ...) {
  
  xname   <- deparse(substitute(x))
  total_n <- length(x)
  ok      <- !is.na(x)
  n       <- sum(ok)
  
  if (is.null(main))
    main <- xname
  
  # Guard: all-NA or length == 0
  if (n == 0L)
    return(.descAllNA(x, xname, main, plotit, verbose))
  
  # 0/1 indicator: route to the dichotomous (logical) engine. Only literal
  # 0/1 values qualify - this is not a general "k=2" check. Two arbitrary
  # numeric values (e.g. c(3.2, 7.8)) remain two distinct measurements
  # with a meaningful mean/sd/skew; only 0/1 carries an inherent
  # "event occurred / did not occur" interpretation, for which the
  # proportion-with-CI view is the more honest description than
  # mean/skew/kurtosis of a Bernoulli variable.
  if (isTRUE(all(x[ok] %in% c(0, 1))) && length(unique(x[ok])) == 2L)
    return(.descLogicalCore(x, xname = xname, conf.level = conf.level,
                            include_x = include_x, main = main,
                            verbose = verbose, plotit = plotit,
                            digits = digits, ...))

  nstat <- .numStats(x[ok])
  
  # meanCI
  if (n > 1) {
    a <- qt(p = (1-conf.level) / 2, df = n-1) * nstat$sd / sqrt(n)
  } else {
    a <- NA
  }
  meanCI <- nstat$mean + c(1, -1) * a
  
  # check for remarkably frequent values in a numeric variable
  # say the most frequent value has significantly more than 5% from the total sample
  modefreq_crit <-
    binom.test(naReplace(nstat$modefreq, 0), n = n, p = 0.05, alternative = "greater")
  
  if (modefreq_crit$p.value < 0.05 && nstat$nu > 12) {
    modefreq_crit <- gettextf(
      "heap(?): remarkable frequency (%s) for the mode(s) (= %s)",
      fm(modefreq_crit$estimate, fmt = "%", digits = 1),
      paste(nstat$modex, collapse = ", ")
    )
  } else {
    modefreq_crit <- NA
  }
  
  # we display frequencies, when unique values <=12 else we set maxrows = 0
  # which will display extreme values as high-low list
  if (is.null(maxrows)) {
    maxrows <- ifelse(nstat$nu <= 12, 12, 0)
  }
  
  if (maxrows > 0) {
    freq <- freq(factor(x[ok]))
    colnames(freq)[1] <- "value"
    # use maxrows as percentage, when < 1
    if (maxrows < 1) {
      maxrows <- sum(freq[, 5] < maxrows) + 1
    }
  } else {
    freq <- NULL
  }
  
  # put together the results
  res <- list(
    
    # xname, not a second deparse(substitute(x)): the promise is still
    # intact here so both give the same string, but one source is enough
    meta = .descMeta(x, xname, main, plotit, verbose),
    
    length = total_n,
    n = n,
    NAs = total_n - n,
    main = main,
    unique = nstat$nu,
    "0s" = nstat$n0,
    mean = nstat$mean,
    meanSE = nstat$meanSE,
    conf.level = conf.level,
    meanCI = meanCI,
    quant = nstat$quant,
    range = nstat$range,
    meanAD = nstat$meanAD,
    sd = nstat$sd,
    var = nstat$var,
    vcoef = nstat$vcoef,
    mad = nstat$mad,
    iqr = nstat$iqr,
    skew = nstat$skew,
    kurt = nstat$kurt,
    small = nstat$small,
    large = nstat$large,
    mode = nstat$modex,
    modefreq_crit = modefreq_crit,
    freq = freq,
    maxrows = maxrows,
    plotit = plotit,
    digits = digits,
    x = if (include_x) x else NULL
  )
  
  class(res) <- c("Desc.numeric","Desc")
  return(res)
  
}



#' @rdname desc
#' @export
print.Desc.numeric <- function(x, digits = NULL, ...) {

  orig <- x

  .printHeader(x$meta)
  
  nlow <- 5
  nhigh <- 5
  
  if (is.null(digits) && !is.null(x$digits)) digits <- x$digits
  defdigits <- is.null(digits)
  
  x["nperc"] <- fm(x[["n"]] / x[["length"]], fmt = "%", digits = 1)
  x["naperc"] <- fm(x[["NAs"]] / x[["length"]], fmt = "%", digits = 1)
  x["zeroperc"] <- fm(x[["0s"]] / x[["length"]], fmt = "%", digits = 1)
  
  if (x[["n"]] > 1) {
    a <- qt(p = (1 - x[["conf.level"]]) / 2, df = x[["n"]] - 1, 
            lower.tail = FALSE) * x[["meanSE"]]
  } else {
    a <- NA
  }
  
  x["meanCI"] <- x[["mean"]] - a
  x["meanUCI"] <- x[["mean"]] + a
  
  x[c("length", "n", "NAs", "unique", "0s")] <-
    lapply(x[c("length", "n", "NAs", "unique", "0s")],
           fm,
           fmt = style("abs.sty")
    )
  if (defdigits) {
    # how many digits do we want to use?
    # we would use the same number as quantile does...
    out <- capture.output(x$quant)
    digits <- max(2, maxDec(strsplit(strTrim(out[[2]]), split = " ")[[1]][1]))
    # for counts the quants would tipically return 0 digits, mean and
    # sd deserve some though
    
  }
  
  x[["quant"]][] <- fm(x[["quant"]], fmt = style("num.sty", digits = digits))
  
  x[c("mean", "meanCI", "meanUCI", "range", "sd", "vcoef", "mad", "iqr", "skew", "kurt")] <-
    lapply(x[c("mean", "meanCI", "meanUCI", "range", "sd", "vcoef", "mad", "iqr", "skew", "kurt")],
           fm,
           fmt = style("num.sty", digits = digits)
    )
  
  lst <- list(
    l1 = unlist(x[c("length", "n", "NAs", "unique", "0s", "mean", "meanCI")]),
    l2 = c("", x[["nperc"]], x[["naperc"]], "", x[["zeroperc"]], "", x[["meanUCI"]]),
    l3 = x[["quant"]][-c(1, 9)],
    l4 = unlist(x[c("range", "sd", "vcoef", "mad", "iqr", "skew", "kurt")])
  )
  
  width <- max(c(
    unlist(lapply(lst, nchar)),
    unlist(lapply(lapply(lst, names), nchar))
  ), na.rm = TRUE)
  if (x$unique == x$n) {
    lst$l1["unique"] <- "= n"
  }
  
  
  # replaced by 0.99.19
  # cat(paste(lapply(lst, .txtline, width = width, ind = "  ",
  #                  space = "  "), collapse = "\n"), "\n")
  # clarify: print.gap can be set with space, which is set here to 2 spaces
  # should we make an argument out of that?
  
  m <- rbind(
    lst$l1, lst$l2, "",
    names(lst$l3), lst$l3, "",
    names(lst$l4), lst$l4, ""
  )
  out <- capture.output(printCharMatrix(m, showRownames = FALSE))
  out[1] <- paste0(out[1], .getOption("footnote")[1])
  cat(out, sep = "\n")
  
  # we need to do that even if highlow == FALSE, as Desc.integer
  # could need the result!!
  if (x$meta$class == "numeric") {
    vals <- fm(
      c(x$small$val, x$large$val),
      fmt = style("num.sty", digits = digits)
    )
  } else {
    vals <- fm(c(x$small$val, x$large$val), fmt = style("abs.sty"))
  }
  # we don't want too many digits but as well no trailing 0s by default
  if (defdigits) {
    vals <- gsub("\\.0+$", "\\.0", gsub("^(\\d+\\.\\d*?[1-9])0+$", "\\1",
                                        vals,
                                        perl = TRUE
    ))
  }
  
  if (is.null(x$freq)) {
    frq <- c(x$small$freq, x$large$freq)
    frqtxt <- paste(" (", fm(frq, fmt = style("abs.sty")), ")", sep = "")
    frqtxt[frq < 2] <- ""
    txt <- strTrim(paste(vals, frqtxt, sep = ""))
    x$lowtxt <-
      paste(head(txt, min(length(x$small$val), nlow)), collapse = ", ")
    x$hightxt <-
      paste(rev(tail(txt, min(length(x$large$val), nhigh))), collapse = ", ")
    
    cat(paste("lowest : ", x$lowtxt, "\n", "highest: ", x$hightxt, "\n\n",
              sep = ""
    ))
  } else {
    cat("\n")
    print(x$freq[seq_len(min(nrow(x$freq), x$maxrows)), , drop = FALSE])
    if (x$maxrows < nrow(x$freq)) {
      cat("... etc.\n [list output truncated]\n\n")
    } else {
      cat("\n")
    }
  }
  
  if (!is.na(x$modefreq_crit)) {
    cat(x$modefreq_crit)
    cat("\n\n")
  }
  
  if (.hasColor()) {
    cat(cli::col_silver(gettextf(
      "%s %s%s-CI (classic)\n\n",
      .getOption("footnote")[1], x$conf.level * 100, "%"
    )))
  } else {
    cat(gettextf(
      "%s %s%s-CI (classic)\n\n",
      .getOption("footnote")[1], x$conf.level * 100, "%"
    ))
  }
  
  # plot() gets the ORIGINAL object, not the one this function has been
  # rewriting: the block above replaces x$n, x$length and friends with
  # formatted strings, so plot.Desc.numeric()'s `x$n <= 1L` guard would
  # be a string comparison.
  if (x$meta$plotit)
    plot(orig, main = x$meta$main)

  invisible(orig)
}



#' @rdname desc
#' @export
plot.Desc.numeric <- function(x, main = x$meta$main, ...) {
  if (x$n <= 1L)
    return(plot.Desc.AllNA(x, ...))
  pharos::plotFdist(x$x, main = main, ...)
}



# ===========================================================================
# internal helper functions

.numStats <- function(x, ...){
  
  # superfast function to get most relevant set of statistics
  # for numeric values within one step
  
  # ATTENTION: x MUST NOT contain NAs!!
  #            (we don't want to lose time here to check for NAs)
  
  
  n <- length(x)
  probs <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
  
  # the quantiles, totally analogue to the core of stats::quantile:
  index <- 1 + (n - 1) * probs
  
  lo <- floor(index)
  hi <- ceiling(index)
  
  x <- sort(x, partial = unique(c(lo, hi)))
  # NOTE: sort(partial=) does NOT fully sort - only the elements at the
  # given positions are guaranteed to be final, which is exactly what the
  # quantiles below need. The former comment claimed the whole vector had
  # to be sorted for the extremes and the unique count; it does not, and
  # is not: n_pow_sum_cpp() builds an ordered map and derives both from
  # that, independently of the order it receives.
  
  qs <- x[lo]
  i <- which(index > lo)
  h <- (index - lo)[i]
  qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
  
  names(qs) <- c("min", ".05", ".10", ".25",
                 "median", ".75", ".90", ".95", "max")
  
  # ... here we go, all we need so far is in qs
  
  # proceed with the parameteric stuff...
  
  # we send the SORTED vector WITHOUT NAs to the C++ function to calc
  # the power sum(s), extreme values and the mode
  # NOTE: this is highly performance relevant!
  psum <- n_pow_sum_cpp(x)
  
  # this is method 3 in the usual functions Skew and Kurt
  # b1 = m3 / m2^1.5, then the type-3 adjustment
  skewx <- ((1 / n * psum$sum3) / (psum$sum2 / n)^1.5) * ((n - 1) / n)^(3 / 2)
  # b2 = m4 / m2^2, then the type-3 adjustment. The former version wrote
  # (((b2) - 3) + 3), which subtracts and re-adds 3 to no effect.
  kurtx <- ((1 / n * psum$sum4) / (psum$sum2 / n)^2) * (1 - 1 / n)^2 - 3
  
  # get std dev here
  varx <- psum$sum2 / (n - 1)
  sdx <- sqrt(varx)
  
  # get the mode
  modex <- modeX(x)
  
  # put together the results
  res <- list(
    n = n,
    nu = psum$unique,
    n0 = psum$zero,
    mean = psum$mean,
    meanSE = sdx / sqrt(n),
    quant = qs,
    range = unname(diff(qs[c(1, 9)])),
    meanAD = psum$sum1 / n,
    sd = sdx,
    var = varx,
    vcoef = sdx / psum$mean,
    mad = mad(x, center = qs[5]),
    iqr = unname(diff(qs[c(4, 6)])),
    skew = skewx,
    kurt = kurtx,
    small = data.frame(val  = psum$small_val,
                       freq = psum$small_freq),
    large = data.frame(val  = psum$large_val,
                       freq = psum$large_freq),
    modex = modex,
    modefreq = attr(modex, "freq")
  )
  
  return(res)
  
}
