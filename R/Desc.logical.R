
#' Describe a Dichotomous Variable
#'
#' Compute descriptive statistics for a dichotomous variable. The plot method
#' displays absolute and relative frequencies in horizontal bar plots.
#' 
#' @aliases desc.logical
#' @details
#' Description of a \strong{dichotomous variable}. This can either be a logical
#' vector, a factor with two levels or a numeric variable with only two unique
#' values. The confidence levels for the relative frequencies are calculated by
#' \code{\link[lumen]{binomCI}()}, method \code{"Wilson"} on a confidence
#' level defined by \code{conf.level}. 
#' 
#' Dichotomous variables can be condensed into a compact graphical
#' representation. The method calculates frequencies and binomial confidence
#' intervals and can display them as a dot plot with error bars.
#' 
#' @param x a dichotomous numeric, integer, factor, character, or logical
#' vector
#' 
#' @param ... further arguments passed to methods
#'   
#' @param digits number of digits used to format relative frequencies; the
#' default can be set with \code{setDescToolsXOption(digits = x)}
#' @param ord order of the levels
#' @param include_x logical; if \code{TRUE}, the original vector is retained
#' in the result
#'
#' @return an object of class \code{c("Desc.logical", "Desc")} with components:
#' \describe{
#'   \item{\code{afrq}}{absolute frequencies}
#'   \item{\code{rfrq}}{matrix of binomial estimates with columns:
#'     \describe{
#'       \item{\code{est}}{point estimate of the binomial proportion}
#'       \item{\code{lci}}{lower confidence interval bound}
#'       \item{\code{uci}}{upper confidence interval bound}
#'     }}
#' }
#' 
#' 
#' @seealso \code{\link[pharos]{plotPropCI}} for graphical display


#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#'
#'



#' @rdname desc
#' @method desc logical
#' @export
desc.logical <- function(x, ord = "level", conf.level = 0.95,
                         include_x = TRUE,
                         main = NULL, verbose = NULL, plotit = NULL,
                         digits = NULL, ...) {
  .descLogicalCore(x, xname = deparse(substitute(x)), ord = ord,
                   conf.level = conf.level, include_x = include_x,
                   main = main, verbose = verbose, plotit = plotit,
                   digits = digits, ...)
}




# .descLogicalCore: shared engine for desc.logical() and for dichotomous
# routing from desc.numeric()/desc.factor() (0/1 numerics, 2-level
# factors/characters). xname is captured explicitly by the caller via
# deparse(substitute(x)) rather than re-derived here, since substitute()
# does not see through the extra call frame when this is invoked from
# desc.numeric()/desc.factor() rather than directly by the user.
.descLogicalCore <- function(x, xname, ord = "level", conf.level = 0.95,
                             include_x = TRUE,
                             main = NULL, verbose = NULL, plotit = NULL,
                             digits = NULL, ...) {
  
  total_n <- length(x)
  ok <- !is.na(x)
  n <- sum(ok)
  
  if (is.null(main))
    main <- xname
  
  if (n == 0L)
    return(.descAllNA(x, xname, main, plotit, verbose))
  
  ff <- table(x)
  
  switch(match.arg(ord, c("level", "desc", "asc", "name")),
         level = {  },
         name  = { ff <- ff[names(ff)] },
         asc   = { ff <- sort(ff) },
         desc  = { ff <- -sort(-ff) }
  )
  
  bf <- binomCI(ff, n, conf.level = conf.level)
  if (is.null(dim(bf))) {
    bf <- matrix(bf, nrow = 1, dimnames = list(names(ff), names(bf)))
  } else {
    bf <- as.matrix(bf[, c("est", "lci", "uci")])
  }
  
  res <- list(
    meta = .descMeta(x, xname, main, plotit, verbose),
    length = total_n,
    n = n,
    NAs = total_n - n,
    digits = digits,
    unique = length(ff),
    afrq = ff,
    rfrq = bf,
    conf.level = conf.level,
    x = if (include_x) x else NULL
  )
  
  class(res) <- c("Desc.logical", "Desc")
  res
}



#' @rdname desc
#' @export
print.Desc.logical <- function(x, digits = NULL, ...) {
  
  digits <- digits %||% x$digits
  
  .printHeader(x$meta)
  
  if (!is.null(digits)) {
    opt <- options(digits = digits)
    on.exit(options(opt))
  }
  
  m <- rbind(
    c("length", "n", "NAs", "unique"),
    c(fm(unlist(x[c("length", "n", "NAs", "unique")]), fmt = style("abs.sty"))),
    c(
      "",
      x["nperc"] <- fm(x[["n"]] / x[["length"]], fmt = "%", digits = 1),
      x["naperc"] <- fm(x[["NAs"]] / x[["length"]], fmt = "%", digits = 1),
      ""
    )
  )
  m[] <- strAlign(m, sep = "\\r")
  cat(paste(" ", apply(m, 1, paste, collapse = " ")), sep = "\n")
  cat("\n")
  
  if (!is.null(x$afrq)) {
    out <- cbind(
      freq = fm(x$afrq, fmt = style("abs.sty")),
      fm(x$rfrq, fmt = style("per.sty", digits = digits))
    )
    
    out <- apply(out, 2, strTrim)
    if (is.null(dim(out))) 
      out <- matrix(out, nrow = 1, dimnames = list(NULL, names(out)))

    rownames(out) <- rownames(x$afrq)
    colnames(out) <- c(
      "freq", "perc",
      gettextf(
        c("lci%s", "uci%s"),
        fm(x$conf.level, digits = 2, leadDigits = 0)
      )
    )
    
    txt <- capture.output(print(out,
                                quote = FALSE, right = TRUE,
                                print.gap = 2
    ))
    
    footer <- .getOption("footnote")[1]
    
    cat(paste(txt[1], footer, sep = ""), txt[-1], sep = "\n")
    
    if (.hasColor()) {
      cat(cli::col_silver(gettextf("\n%s %s%s-CI (Wilson)\n\n",
                                   footer, x$conf.level * 100, "%"
      )))
    } else {
      cat(gettextf(
        "\n%s %s%s-CI (Wilson)\n\n",
        footer, x$conf.level * 100, "%"
      ))
    }
  }
  
  if (!is.null(attr(x$x, "coding"))) {
    cod <- attr(x$x, "coding")
    cat(gettextf("coding (original x): '%s' = FALSE, '%s' = TRUE\n\n",
                 names(cod)[1], names(cod)[2]))
  }
  
  if (identical(x$noplot, TRUE)) {
    cat(gettextf("Nothing to plot in %s\n\n", x$xname))
  }
  
  if(x$meta$plotit)
    plot(x, main=x$meta$main)
  
}




#' @rdname desc
#' @export
plot.Desc.logical <- function(x, ...) {
  ff  <- x$afrq
  lbs <- names(ff)
  
  # Only one level observed (e.g. a constant logical vector, all-TRUE or
  # all-FALSE) - the missing complement is always unambiguous for a
  # logical variable. plotPropCI() always normalizes its matrix columns
  # to c("FALSE","TRUE") order regardless of which single value was
  # observed, so the labels must match that fixed order here too.
  if (length(lbs) == 1L)
    lbs <- c("FALSE", "TRUE")
  
  pharos::plotPropCI(as.matrix(t(ff)), labels = lbs, ...)
}
