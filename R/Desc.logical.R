
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
#' \code{\link[lumen]{binomCI}()}, method \code{"Wilson"} on a confidence
#' level defined by \code{conf.level}. 
#' 
#' Dichotomous variables can easily be
#' condensed in one graphical representation. Desc for a set of flags
#' (=dichotomous variables) calculates the frequencies, a binomial confidence
#' interval and produces a kind of dotplot with error bars. Motivation for this
#' function is, that dichotomous variable in general do not contain intense
#' infmion. Therefore it makes sense to condense the description of sets of
#' dichotomous variables.
#' 
#' @param x a dichotomous vector of variable class, can be a \code{"numeric"}
#' \code{"integer"},  \code{"factor"},  \code{"character"} or  \code{"boolean"}, 
#' the only condition is, that there are only two unique values.
#' 
#' @param ... Further graphical parameters passed to the underlying
#'   base R plotting functions.
#'   
#' @param digits integer. With how many digits should the relative frequencies
#' be fmted? Default can be set by
#' \code{\link{setDescToolsXOption}(digits=x)}.
#' 
#' @param ord  order of the levels
#' 
#' @seealso \code{\link[aurora]{plot.Desc.logical}} for graphical display


#' @rdname Desc
#' @method Desc logical
#' @export
Desc.logical <- function(x, ord = "level", conf.level = 0.95, 
                         main = NULL, verbose = NULL, plotit = NULL,
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
  
  bf <- as.matrix(binomCI(ff, n, conf.level = conf.level)[, 1:3])
  rownames(bf) <- names(ff)
  
  res <- list(
    
    meta = .descMeta(x, deparse(substitute(x)), main, plotit, verbose),
    
    length = total_n,
    n = n,
    NAs = total_n - n,
    
    digits = digits,
    
    unique = length(ff),
    afrq = ff, 
    rfrq = bf, 
    conf.level = conf.level
    
  )
  
  class(res) <- c("Desc.logical", "Desc")
  return(res)
  
}



#' @rdname Desc
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
    
    rownames(out) <- rownames(x$afrq)
    colnames(out) <- c(
      "freq", "perc",
      gettextf(
        c("lci%s", "uci%s"),
        fm(x$conf.level, digits = 2, ldigits = 0)
      )
    )
    
    txt <- capture.output(print(out,
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
  
  if(x$meta$plotit)
    plot(x, main=x$meta$main)
  
}


# Note:
# plot.Desc.logical is part of aurora


