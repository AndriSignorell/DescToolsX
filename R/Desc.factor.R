
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
#' @aliases Desc.factor Desc.ordered Desc.character
#' @param x the object to be described. This can be a data.frame, a list, a
#' table or a vector of the classes: numeric, integer, factor, ordered factor,
#' logical.
#' @param \dots further arguments to be passed to or from other methods
#' 
#' @param main (character|\code{NULL}|\code{NA}), the main title(s). \itemize{
#' \item If \code{NULL}, the title will be composed as: \itemize{ \item
#' variable name (class(es)), \item resp. number - variable name (class(es)) if
#' the \code{enum} option is set to \code{TRUE.} } \item Use \code{NA} if no
#' caption should be printed at all. }
#' @param plotit logical. Should a plot be created? The plot type will be
#' chosen according to the classes of variables (roughly following a
#' numeric-numeric, numeric-categorical, categorical-categorical logic).
#' Default can be defined by option \code{plotit}, if it does not exist then
#' it's set to \code{FALSE}.
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
#' @param digits integer. With how many digits should the relative frequencies
#' be formatted? Default can be set by
#' \link[=setDescToolsXOption]{setDescToolsXOption(digits=x)}.
#' @param ord character out of \code{"name"} (alphabetical order),
#' \code{"level"}, \code{"asc"} (by frequencies ascending), \code{"desc"} (by
#' frequencies descending) defining the order for a frequency table as used for
#' factors, numerics with few unique values and logicals. Factors (and
#' character vectors) are by default ordered by their descending frequencies,
#' ordered factors by their natural order.
#' 
#' @param maxlablen maximal character length for the labels 
#' @param type type of plot bar/dot
#' @param col the color of the bars
#' @param border the border color of the bars
#' @param xlim limitation for the x-axis
#' @param ecdf boolean, should the cumulative distribution function be 
#' included? (default \code{TRUE})



#' @rdname Desc.factor
#' @export
Desc.factor <- function(x, maxrows = NULL, 
                        ord=NULL, main = NULL, 
                        plotit=.getOption("plotit"), 
                        digits=NULL, ...) {
  
  # ----------------------------------------------------
  # general handling  
  
  total_n <- length(x)    # total n
  ok <- !is.na(x)         # non NAs
  n <- sum(ok)            # valid n
  
  if (is.null(main)) 
    main <- deparse(substitute(x))

  
  # ----------------------------------------------------
  # class specific handling  
  
  if(is.null(ord)){
    if(is.ordered(x))
      ord <- "level"
    else
      ord <- "desc"
  }
  
  freq <- Freq(x, ord = ord, ...)
  
  if (is.null(maxrows)) {
    maxrows <- 12
  }
  
  if (maxrows < 1) {
    maxrows <- sum(freq[, 5] < maxrows) + 1
  }
  
  res <- list(
    xname = deparse(substitute(x)),
    label = Label(x),
    class = paste(class(x), collapse = ","),
    classlabel = paste(class(x), collapse = ","),
    length = total_n,
    n = n,
    NAs = total_n - n,
    main = main,
    plotit = plotit,
    digits = digits,
    
    levels = nlevels(x),
    unique = sum(freq$freq > 0), dupes = any(freq$freq > 1), maxrows = maxrows,
    ord = ord, freq = freq
    
  )
  
  class(res) <- c("Desc.factor", "Desc")
  return(res)

}


# use the exactly same logic for characters...
#' @rdname Desc.factor
#' @export
Desc.character <- Desc.factor  



#' @rdname Desc.factor
#' @export
print.Desc.factor <- function(x, digits = NULL, ...) {
  
  .printHeader(main = x[["main"]], 
               class=x[["class"]], 
               label = x[["label"]])

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
  
  digits <- Coalesce(digits, x$digits, NULL)
  
  x$freq <- x$freq[1:min(nrow(x$freq), x$maxrows), ]
  txt.freq <- .CaptOut(print(x$freq, digits = digits))
  cat("\n")
  cat(txt.freq, sep = "\n")
  
  if (x$maxrows < x$levels) {
    cat("... etc.\n [list output truncated]\n\n")
  } else {
    cat("\n")
  }
  
  if(x$plotit)
    plot(x, main=x$main)
  
}




.plotFreq <- function(x, main = NULL, maxlablen = 25,
                     type = c("bar", "dot"),
                     col = NULL, border = NULL, xlim = NULL, 
                     ecdf = TRUE, ...) {
  
  
  DescToolsGraphics:::.withGraphicsState({
    
    # was cex in the dots-args? parse dots.arguments
    cex <- unlist(match.call(expand.dots = FALSE)$...["cex"])
    if (is.null(cex)) cex <- par("cex")
    
    tab <- as.table(x$freq$freq)
    names(tab) <- x$freq[[1]]
    ptab <- as.table(x$freq$perc)
    trunc_fg <- (nrow(tab) > x$maxrows)
    if (!is.na(x$maxrows) && x$maxrows < nrow(tab)) {
      tab <- tab[1:min(nrow(tab), x$maxrows)]
      ptab <- ptab[1:min(nrow(tab), x$maxrows)]
    }
    
    if (max(nchar(names(tab))) > maxlablen) {
      names(tab) <- strTrunc(names(tab), maxlablen)
    }
    wtxt <- max(strwidth(names(tab), "inch"))
    wplot <- (par("pin")[1] - wtxt) / 2
    layout(matrix(c(1, 2), nrow = 1), widths = c(wtxt + wplot, wplot) * 2.54)
    par(mai = c(1.2, max(strwidth(rev(names(tab)), "inch")) + .5, 0.2, .3) + .02)
    if (!is.na(x$main)) par(oma = c(0, 0, 3, 0))
    
    
    switch(match.arg(arg = type, choices = c("bar", "dot")),
           dot = {
             if (is.null(xlim)) {
               xlim <- range(pretty(tab)) + c(-1, 1) * diff(range(pretty(tab))) * 0.04
             }
             
             if (is.null(col)) col <- DescToolsGraphics::Pal()[1]
             if (is.null(border)) border <- "black"
             b <- barplot(rev(tab),
                          horiz = TRUE, border = NA, col = "white", las = 1,
                          xlim = xlim,
                          xpd = FALSE, xlab = "frequency",
                          cex.names = cex, cex.axis = cex, cex.lab = cex, tck = -0.04
             )
             abline(h = b, v = 0, col = "grey", lty = "dotted")
             segments(0, b, as.vector(rev(tab)), b)
             points(
               x = as.vector(rev(tab)), y = b, yaxt = "n",
               col = border, pch = 21, bg = col, cex = 1.3
             )
             box()
             
             par(mai = c(1.2, 0.1, 0.2, .3) + .02)
             b <- barplot(rev(ptab),
                          horiz = TRUE, border = NA, col = "white", las = 1, names = "",
                          xlim = c(-0.04, 1.04),
                          xlab = "percent", cex.names = cex, cex.axis = cex,
                          cex.lab = cex, tck = -0.04
             )
             abline(h = b, v = 0, col = "grey", lty = "dotted")
             segments(0, b, as.vector(rev(ptab)), b)
             points(
               x = as.vector(rev(ptab)), y = b, col = border, pch = 21,
               bg = col, cex = 1.3
             )
             box()
           },
           bar = { # type = "bar"
             
             if (is.null(xlim)) {
               xlim <- range(pretty(c(0.96 * min(tab), 1.04 * max(tab))))
             }
             
             if (is.null(col)) {
               col <- c(
                 rep("grey80 ", length.out = 2 * nrow(tab)),
                 rep(DescToolsGraphics::alpha("grey80", 0.4), length.out = nrow(tab))
               )
             } else {
               if (length(col) == 1) {
                 col <- c(
                   rep(col, length.out = 2 * nrow(tab)),
                   rep(DescToolsGraphics::alpha(col, 0.3), length.out = nrow(tab))
                 )
               } else {
                 col <- rep(col, length.out = 3 * nrow(tab))
               }
             }
             if (is.null(border)) border <- NA
             barplot(rev(tab),
                     horiz = TRUE, col = col[1:nrow(tab)],
                     border = border, las = 1, xlim = xlim,
                     xpd = FALSE, xlab = "frequency",
                     cex.names = cex, cex.axis = cex, cex.lab = cex, tck = -0.04
             )
             grid(ny = NA)
             
             par(mai = c(1.2, 0.15, 0.2, .3) + .02)
             if (ecdf) {
               barplot(rev(cumsum(ptab)),
                       horiz = TRUE, col = col[(2 * nrow(tab) + 1):(3 * nrow(tab))],
                       border = border, las = 1,
                       names = "", xlim = c(0, 1), xlab = "percent",
                       cex.names = cex, cex.axis = cex, cex.lab = cex, tck = -0.04
               )
               barplot(rev(ptab),
                       horiz = TRUE, col = col[(nrow(tab) + 1):(2 * nrow(tab))],
                       border = border, names = "", xlab = NA, ylab = NA,
                       add = TRUE, axes = FALSE
               )
             } else {
               barplot(rev(ptab),
                       horiz = TRUE, col = col[(nrow(tab) + 1):(2 * nrow(tab))],
                       border = border, las = 1, names = "",
                       xlim = c(0, 1), xlab = "percent", cex.names = cex,
                       cex.axis = cex, cex.lab = cex, tck = -0.04
               )
             }
             grid(ny = NA)
           }
    )
    
    
    if (is.null(main)) main <- x$main
    if (!is.na(main)) {
      title(main = Coalesce(main, x$main), outer = TRUE)
    }
    
    if (trunc_fg) {
      text(
        x = par()$usr[2], y = 0.4, labels = " ...[list output truncated]  ",
        cex = 0.6, adj = c(1, 0.5)
      )
    }
    
    if (!is.null(.getOption("stamp"))) {
      DescToolsGraphics::stamp()
    }
    
    # close .withGraphicsState
  })
  
  invisible()
}


#' @rdname Desc.factor
#' @export
plot.Desc.factor <- .plotFreq

