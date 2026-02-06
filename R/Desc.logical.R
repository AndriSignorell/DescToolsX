
#' Desc.logical 
#'
#' Displays a set of statistical measures describing a dichotomous variable.
#' Visualizes the distribution of a numeric \code{Desc} object.
#' The plot consists of a pair of horizontally organized barplots
#' displaying the absolute and relative frequencies.
#' 
#' @aliases Desc.logical
#' @details
#' Description of a \strong{dichotomous variable}. This can either be a logical
#' vector, a factor with two levels or a numeric variable with only two unique
#' values. The confidence levels for the relative frequencies are calculated by
#' \code{\link[=binomCI]{binomCI()}}, method \code{"Wilson"} on a confidence
#' level defined by \code{conf.level}. 
#' 
#' Dichotomous variables can easily be
#' condensed in one graphical representation. Desc for a set of flags
#' (=dichotomous variables) calculates the frequencies, a binomial confidence
#' interval and produces a kind of dotplot with error bars. Motivation for this
#' function is, that dichotomous variable in general do not contain intense
#' infmion. Therefore it makes sense to condense the description of sets of
#' dichotomous variables.

#' @param x a dichotomous vector of variable class, can be a \code{"numeric"}
#' \code{"integer"},  \code{"factor"},  \code{"character"} or  \code{"boolean"}, 
#' the only condition is, that there are only two unique values.
#' @param ... Further graphical parameters passed to the underlying
#'   base R plotting functions.
#' @param digits integer. With how many digits should the relative frequencies
#' be fmted? Default can be set by
#' \link[=setDescToolsXOption]{setDescToolsXOption(digits=x)}.
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
#' @param ord  order of the levels
#' @param xlab label for the x-axis
#' @param col colors for the bars
#' @param legend boolean, display a legend? (default \code{TRUE})
#' @param xlim limits for the x-axis
#' @param confint boolean, display confidence intervals? (default \code{TRUE})



#' @rdname Desc.logical
#' @export
Desc.logical <- function(x, ord = "level", conf.level = 0.95, 
                         main = NULL, 
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
  
    
  ff <- table(x)
  
  # how should the table be sorted, by name, level or frq? (NULL means "desc")
  switch(match.arg(ord, c("level", "desc", "asc", "name")),
         level = {  },
         name = {
           ff <- ff[names(ff)]
         },
         asc = {
           ff <- sort(ff)
         },
         desc = {
           ff <- -sort(-ff)
         }
  )
  
  bf <- binomCI(ff, n, conf.level = conf.level)
  rownames(bf) <- names(ff)
  
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
    
    unique = length(ff),
    afrq = ff, rfrq = bf, conf.level = conf.level
  )
  
  class(res) <- c("Desc.logical", "Desc")
  return(res)
  
}



#' @rdname Desc.logical
#' @export
print.Desc.logical <- function(x, digits = NULL, ...) {
  digits <- Coalesce(digits, x$digits, NULL)

  .printHeader(main = x[["main"]], 
               class=x[["class"]], 
               label = x[["label"]])
  
  
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
    
    rownames(out) <- rownames(x$afrq)
    colnames(out) <- c(
      "freq", "perc",
      gettextf(
        c("lci%s", "uci%s"),
        fm(x$conf.level, digits = 2, ldigits = 0)
      )
    )
    
    txt <- capture.output(print(strTrim(out),
                                quote = FALSE, right = TRUE,
                                print.gap = 2
    ))
    
    footer <- .getOption("footnote")[1]
    
    cat(paste(txt[1], footer, sep = ""), txt[-1], sep = "\n")
    
    if (.has_color()) {
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
  
  if (identical(x$noplot, TRUE)) {
    cat(gettextf("Nothing to plot in %s\n\n", x$xname))
  }
  
  if(x$plotit)
    plot(x, main=x$main)
  
}




.plotBoolean <- function(x, main = NULL, xlab = "", col = NULL,
                              legend = TRUE, xlim = c(0, 1), confint = TRUE, ...) {
  
  
  DescToolsGraphics:::.withGraphicsState({
    
    main <- Coalesce(main, x$main, deparse(substitute(x)))
    
    if (is.null(col)) {
      col <- c(DescToolsGraphics::Pal()[1:2], "grey80", "grey60", "grey40")
    } else {
      col <- rep(col, length.out = 5)
    }
    
    tab <- x$afrq
    ptab <- x$rfrq[, 1]
    if (nrow(x$rfrq) > 2) stop("!plot.Desc.logical! can only display 2 levels")
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    
    par(mar = c(4.1, 2.1, 0, 2.1))
    if (!is.na(main)) par(oma = c(0, 0, 3, 0))
    
    plot(
      x = ptab[1], y = 1, cex = 0.8, xlim = xlim, yaxt = "n", ylab = "",
      type = "n", bty = "n", xlab = xlab, main = NA
    )
    segments(x0 = 0, x1 = 1, y0 = 1, y1 = 1, col = "grey")
    segments(x0 = c(0, 1), x1 = c(0, 1), y0 = 0.8, y1 = 1.2, col = "grey")
    
    # insert grid
    segments(
      x0 = seq(0, 1, 0.1), x1 = seq(0, 1, 0.1), y0 = 0.8, y1 = 1.2,
      col = "grey", lty = "dotted"
    )
    rect(xleft = 0, ybottom = 0.95, xright = ptab[1], ytop = 1.05, col = col[1]) # greenyellow
    rect(xleft = ptab[1], ybottom = 0.95, xright = 1, ytop = 1.05, col = col[2]) # green4
    
    if (confint) {
      ci.99 <- binomCI(tab[1], sum(tab), conf.level = 0.99)[2:3]
      ci.95 <- binomCI(tab[1], sum(tab), conf.level = 0.95)[2:3]
      ci.90 <- binomCI(tab[1], sum(tab), conf.level = 0.90)[2:3]
      rect(xleft = ci.99[1], ybottom = 0.9, xright = ci.99[2], ytop = 1.1, col = col[3]) # olivedrab1
      rect(xleft = ci.95[1], ybottom = 0.9, xright = ci.95[2], ytop = 1.1, col = col[4]) # olivedrab3
      rect(xleft = ci.90[1], ybottom = 0.9, xright = ci.90[2], ytop = 1.1, col = col[5]) # olivedrab4
      segments(x0 = ptab[1], x1 = ptab[1], y0 = 0.7, y1 = 1.3)
    }
    
    if (legend) {
      legend(
        x = 0, y = 0.75, legend = c("ci.99     ", "ci.95     ", "ci.90     "),
        box.col = "white",
        fill = col[3:5], bg = "white", cex = 1, ncol = 3,
        text.width = c(0.2, 0.2, 0.2)
      )
    }
    if (length(rownames(tab)) == 1) {
      text(rownames(tab), x = ptab[1] / 2, y = 1.2)
    } else {
      text(rownames(tab), x = c(ptab[1], ptab[1] + 1) / 2, y = 1.2)
    }
    
    if (!is.na(main)) title(main = main, outer = TRUE)
    
    if (!is.null(.getOption("stamp"))) 
      DescToolsGraphics::stamp()
    
  })  
  
  invisible()
  
}

#' @rdname Desc.logical
#' @export
plot.Desc.logical <- .plotBoolean

