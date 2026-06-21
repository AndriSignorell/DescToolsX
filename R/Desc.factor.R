
#' Desc.factor 
#'
#' Displays a set of statistical measures describing a factor.
#' Visualizes the distribution of a numeric \code{Desc} object.
#' The plot consists of a pair of horizontally organized barplots
#' displaying the absolute and relative frequencies.
#' 
#' @details
#' This function produces a rich description of a \strong{factor}, containing
#' length, number of NAs, number of levels and detailed frequencies of all
#' levels. The order of the frequency table can be chosen between
#' descending/ascending frequency, labels or levels. For ordered factors the
#' order default is \code{"level"}. Character vectors are treated as unordered
#' factors Desc.char converts x to a factor an processes x as factor.\cr
#' Desc.ordered does nothing more than changing the standard order for the
#' frequencies to it's intrinsic order, which means order \code{"level"}
#' instead of \code{"desc"} in the factor case.
#' 
#' @name desc.factor
#' @aliases desc.factor Desc.ordered Desc.character
#' 
#' @inheritParams desc 
#' 
#' @param x the object to be described. This can be a data.frame, a list, a
#' table or a vector of the classes: numeric, integer, factor, ordered factor,
#' logical.
#' 
#' @param maxrows numeric; defines the maximum number of rows in a frequency
#' table to be reported. For factors with many levels it is often not
#' interesting to see all of them. Default is set to 12 most frequent ones
#' (resp. the first ones if \code{ord} is set to \code{"levels"} or
#' \code{"names"}).
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
#' @param digits integer. With how many digits should the relative frequencies
#' be formatted? Default can be set by
#' \code{\link{setDescToolsXOption}(digits=x)}.
#' 
#' @param ord character out of \code{"name"} (alphabetical order),
#' \code{"level"}, \code{"asc"} (by frequencies ascending), \code{"desc"} (by
#' frequencies descending) defining the order for a frequency table as used for
#' factors, numerics with few unique values and logicals. Factors (and
#' character vectors) are by default ordered by their descending frequencies,
#' ordered factors by their natural order.
#' 
#' @seealso \code{\link[aurora]{plotCatDist}} for graphical display
#' 



#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept factor-handling
#'
#'



#' @rdname desc.factor
#' @method desc factor
#' @export
desc.factor <- function(x, maxrows = NULL, ord = NULL,
                        conf.level = 0.95,
                        main = NULL, verbose = NULL, plotit = NULL,
                        digits = NULL, ...) {
  
  xname   <- deparse(substitute(x))
  total_n <- length(x)
  ok      <- !is.na(x)
  n       <- sum(ok)
  
  # ── Guard: all-NA oder length == 0 ─────────────────────────────────────────
  if (n == 0L)
    return(.descAllNA(x, xname, main, plotit, verbose))
  
  if (is.null(main))
    main <- xname
  
  # k = 2: any 2-level factor/character is inherently yes/no-like (unlike
  # two arbitrary numeric measurements), so the dichotomous proportion
  # view applies unconditionally, not just for a specific value pair.
  if (length(unique(x[ok])) == 2L)
    return(.descLogicalCore(x, xname = xname,
                            ord = ord %||% "level",
                            conf.level = conf.level, include_x = TRUE,
                            main = main, verbose = verbose, plotit = plotit,
                            digits = digits, ...))
  
  if (is.null(ord)) {
    if (is.ordered(x)) ord <- "level" else ord <- "desc"
  }
  
  freq <- freq(x, ord = ord, ...)

  if (is.null(maxrows)) {
    maxrows <- 12
  }
  
  if (maxrows < 1) {
    maxrows <- sum(freq[, 5] < maxrows) + 1
  }
  
  res <- list(
    
    meta = .descMeta(x, deparse(substitute(x)), main, plotit, verbose),

    length = total_n,
    n = n,
    NAs = total_n - n,
    
    digits = digits,
    
    levels = nlevels(x),
    unique = sum(freq$freq > 0), dupes = any(freq$freq > 1), maxrows = maxrows,
    ord = ord, freq = freq
    
  )
  
  class(res) <- c("Desc.factor", "Desc")
  return(res)

}


# use the exactly same logic for characters...
#' @rdname desc.factor
#' @export
desc.character <- desc.factor  



#' @rdname desc.factor
#' @export
print.Desc.factor <- function(x, digits = NULL, ...) {
  
  .printHeader(x$meta)
  
  digits <- digits %||% x$digits
  
  m <- rbind(
    c("length", "n", "NAs", "unique", "levels", "dupes"),
    c(
      fm(unlist(x[c("length", "n", "NAs", "unique", "levels")]),
         fmt = style("abs.sty")
      ),
      c("n", "y")[x$dupes + 1]
    ),
    c(
      "", x["nperc"] <- fm(x[["n"]] / x[["length"]], fmt = "%", digits = 1),
      x["naperc"] <- fm(x[["NAs"]] / x[["length"]], fmt = "%", digits = 1),
      "", "", ""
    )
  )
  
  m[] <- strAlign(m[], sep = "\\r")
  cat(paste(" ", apply(m, 1, paste, collapse = " ")), sep = "\n")

  x$freq <- x$freq[1:min(nrow(x$freq), x$maxrows), ]
  txt.freq <- .captOut(print(x$freq, digits = digits))
  cat("\n")
  cat(txt.freq, sep = "\n")
  
  if (x$maxrows < x$levels) {
    cat("... etc.\n [list output truncated]\n\n")
  } else {
    cat("\n")
  }
  
  if(x$meta$plotit)
    plot(x, main=x$meta$main)
  
}




#' @rdname desc
#' @export
plot.Desc.factor <- function(x, ...){
  aurora::plotCatDist(setNamesX(x$freq[,2], x$freq[,1]), na.rm=TRUE, ...)
}

