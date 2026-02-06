
#' Desc.numeric 
#'
#' Displays a set of statistical measures describing a numeric data vector.
#' Visualizes the distribution of a numeric \code{Desc} object.
#' The plot may include a histogram, density curve, boxplot 
#' and empirical distribution.
#'
#' @param x An object of class \code{"Desc.numeric"}.
#' @param ... Further graphical parameters passed to the underlying
#'   base R plotting functions.
#' @param maxrows numeric; defines the maximum number of rows in a frequency
#' table to be reported. For factors with many levels it is often not
#' interesting to see all of them. Default is set to 12 most frequent ones
#' (resp. the first ones if \code{ord} is set to \code{"levels"} or
#' \code{"names"}).
#' @param digits integer. With how many digits should the real numbers
#' be formatted? Default is taken from \link[=setDescToolsXOption]{setDescToolsXOption(digits=x)}.
#' 
#' @param conf.level confidence level of the interval. If set to \code{NA} no
#' confidence interval will be calculated. Default is 0.95.
#' @param plotit logical. Should a plot be created? The plot type will be
#' chosen according to the classes of variables (roughly following a
#' numeric-numeric, numeric-categorical, categorical-categorical logic).
#' Default can be defined by option \code{plotit}, if it does not exist then
#' it's set to \code{FALSE}.
#' @param main (character|\code{NULL}|\code{NA}), the main title(s). 
#' \itemize{
#' \item If \code{NULL}, the title will be composed as: \itemize{ \item
#' variable name (class(es)), \item resp. number - variable name (class(es)) if
#' the \code{enum} option is set to \code{TRUE.} } \item Use \code{NA} if no
#' caption should be printed at all. }
#' @param include_x (logical) if \code{TRUE} (default) the original vector 
#' will be returned
#' in the result object. This is necessary for producing specific plot 
#' (e.g. the density, ecdf, etc.). However if no plots are required the result
#' object can be kept small and handy without the original data.
#' 
#' @name Desc.numeric
#' @aliases Desc.numeric plot.Desc.numeric print.Desc.numeric
#' 
#' @details
#' This function is an S3 method for \code{\link[graphics]{plot}}.
#' It is automatically dispatched when calling \code{plot(x)} on a
#' \code{Desc.numeric} object.
#' 
#' For a numeric argument x \code{maxrows} is the minimum number of unique
#' values needed for a numeric variable to be treated as continuous. If left to
#' its default \code{NULL}, x will be regarded as continuous if it has more
#' than 12 single values. In this case the list of extreme values will be
#' displayed and the frequency table else.
#' 
#' If \code{maxrows} is < 1 it will be interpreted as percentage. In this case
#' just as many rows, as the \code{maxrows} most frequent levels will be shown.
#' Say, if \code{maxrows} is set to \code{0.8}, then the number of rows is
#' fixed so, that the highest cumulative relative frequency is the first one
#' going beyond 0.8.
#' 
#' Setting \code{maxrows} to \code{Inf} will unconditionally report all values
#' and also produce a plot with type "h" instead of a histogram.
#'
#' Named colors defined by \code{DescToolsX} (e.g. \code{"hred"},
#' \code{"hblue"}) can be used directly.
#'
#' @return
#' Invisibly returns \code{NULL}.
#'
#' @author Andri Signorell
#' \href{mailto:andri@@signorell.net}{andri@@signorell.net}
#' @seealso \code{\link[base:summary]{base::summary()}},
#' \code{\link[base:plot]{base::plot()}}
#' 
#' Other Statistical summary functions: \code{\link{abstract}()}
#' @keywords multivariate print univar
#' @examples
#' 
#' Desc(d.pizza$delivery_min)             # numeric




#' @rdname Desc.numeric
#' @export
Desc.numeric <- function(x, maxrows = NULL, conf.level = 0.95,
                         include_x = TRUE, main = NULL, 
                         plotit=.getOption("plotit"), digits=NULL, 
                         ...) {
  
  total_n <- length(x)    # total n
  ok <- !is.na(x)         # non NAs
  n <- sum(ok)            # valid n
  
  if (is.null(main)) 
    main <- deparse(substitute(x))

  nstat <- .NumStats(x[ok])
  
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
    binom.test(zeroIfNA(nstat$modefreq), n = n, p = 0.05, alternative = "greater")
  
  if (modefreq_crit$p.value < 0.05 & nstat$nu > 12) {
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
    freq <- Freq(factor(x[ok]))
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
    xname = deparse(substitute(x)),
    label = Label(x),
    class = paste(class(x), collapse = ","),
    classlabel = paste(class(x), collapse = ","),
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
    IQR = nstat$IQR,
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



#' @rdname Desc.numeric
#' @export
print.Desc.numeric <- function(x, digits = NULL, ...) {

  .printHeader(main = x[["main"]], class=x[["class"]], label = x[["label"]])
  
  nlow <- 5
  nhigh <- 5
  
  # digits <- Coalesce(digits, x$digits, .getOption("digits"))
  
  if (is.null(digits) && !is.null(x$digits)) digits <- x$digits
  defdigits <- is.null(digits)
  
  x["nperc"] <- fm(x[["n"]] / x[["length"]], fmt = "%", digits = 1)
  x["naperc"] <- fm(x[["NAs"]] / x[["length"]], fmt = "%", digits = 1)
  x["zeroperc"] <- fm(x[["0s"]] / x[["length"]], fmt = "%", digits = 1)
  
  if (x[["n"]] > 1) {
    a <- qt(p = (1 - x[["conf.level"]]) / 2, df = x[["n"]] - 1) * x[["meanSE"]]
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
    digits <- max(2, maxDigits(strsplit(strTrim(out[[2]]), split = " ")[[1]][1]))
    # for counts the quants would tipically return 0 digits, mean and
    # ds deserve some though
    # if(digits==0) digits <- 1
  }
  
  x[["quant"]][] <- fm(x[["quant"]], fmt = style("num.sty", digits = digits))
  
  x[c("mean", "meanCI", "meanUCI", "range", "sd", "vcoef", "mad", "IQR", "skew", "kurt")] <-
    lapply(x[c("mean", "meanCI", "meanUCI", "range", "sd", "vcoef", "mad", "IQR", "skew", "kurt")],
           fm,
           fmt = style("num.sty", digits = digits)
    )
  
  lst <- list(
    l1 = unlist(x[c("length", "n", "NAs", "unique", "0s", "mean", "meanCI")]),
    l2 = c("", x[["nperc"]], x[["naperc"]], "", x[["zeroperc"]], "", x[["meanUCI"]]),
    l3 = x[["quant"]][-c(1, 9)],
    l4 = unlist(x[c("range", "sd", "vcoef", "mad", "IQR", "skew", "kurt")])
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
  out <- capture.output(.print.charmatrix(m))
  out[1] <- paste0(out[1], .getOption("footnote")[1])
  cat(out, sep = "\n")
  
  # we need to do that even if highlow == FALSE, as Desc.integer
  # could need the result!!
  if (x$class == "numeric") {
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
    print(x$freq[1:min(nrow(x$freq), x$maxrows), ])
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
  
  if (.has_color()) {
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
  
  if(x$plotit)
    plot(x, main=x$main)
  
  
}




#' @rdname Desc.numeric
#' @export
plot.Desc.numeric <- function(x, ...){
  DescToolsGraphics::plotFdist(x$x, na.rm=TRUE, ...)
}



# ===========================================================================
# internal helper functions

.NumStats <- function(x, ...){
  
  # superfast function to get most relevant set of statistics
  # for numeric values within one step
  
  # ATTENTION: x must not contain NAs!!
  #            (we don't want to lose time here to check for NAs)
  
  
  n <- length(x)
  probs <- c(0, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 1)
  
  # the quantiles, totally analogue to the core of stats::quantile:
  index <- 1 + (n - 1) * probs
  
  lo <- floor(index)
  hi <- ceiling(index)
  
  x <- sort(x, partial = unique(c(lo, hi)))
  # WHOLE x MUST be sorted in order to get the smallest and largest values,
  # as well as the number of unique values!!!
  
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
  psum <- n_pow_sum(x)
  
  # this is method 3 in the usual functions Skew and Kurt
  skewx <- ((1 / n * psum$sum3) / (psum$sum2 / n)^1.5) * ((n - 1) / n)^(3 / 2)
  kurtx <- ((((1 / n * psum$sum4) / (psum$sum2 / n)^2) - 3) + 3) * (1 - 1 / n)^2 - 3
  
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
    IQR = unname(diff(qs[c(4, 6)])),
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

