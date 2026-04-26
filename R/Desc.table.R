
#' Describe a Contingency Table
#'
#' Computes and displays a comprehensive set of descriptive statistics and
#' association measures for a contingency table (r x c or 2 x 2). The function
#' is also dispatched for \code{matrix} and cross-classified factor pairs via
#' \code{Desc.qq} and \code{Desc.matrix}.
#'
#' @param x a \code{table} or \code{matrix} object. For the formula interface
#'   use \code{Desc(y ~ x, data)} which dispatches to this function
#'   automatically.
#' @param prop character string controlling which proportions are shown in the
#'   cross-tabulation. One of \code{"rows"} (default), \code{"cols"},
#'   \code{"total"}, or \code{"no"} (frequencies only). At \code{verbose = 3}
#'   all three proportions are shown regardless of this argument.
#' @param verbose integer controlling the amount of output (1, 2, or 3).
#'   \code{NULL} (default) falls back to
#'   \code{getOption("DescTools.verbose", 2)}. If set explicitly in the
#'   function call, that value takes priority over the global option.
#'   See Details for what each level produces.
#' @param conf.level numeric, confidence level for all confidence intervals.
#'   Default is \code{0.95}.
#' @param \dots further arguments passed to or from other methods.
#'
#' @details
#' The \code{verbose} argument controls which statistics are computed and
#' displayed. The following table gives an overview; items marked with
#' \emph{2x2} are only shown for 2 x 2 tables.
#'
#' \strong{verbose = 1 — essential output:}
#' \itemize{
#'   \item Summary: n, rows, columns, missings
#'   \item Cross-tabulation: frequencies
#'   \item Pearson chi-squared test
#'   \item Chi-squared with Yates continuity correction \emph{(2x2)}
#'   \item Fisher's exact test \emph{(2x2)}
#'   \item McNemar's test \emph{(2x2)}
#'   \item Cramér's V with confidence interval and effect size label
#'   \item Odds ratio with confidence interval \emph{(2x2)}
#' }
#'
#' \strong{verbose = 2 — standard output (default):}
#'
#' All of the above, plus:
#' \itemize{
#'   \item Cross-tabulation: row proportions (or as set by \code{prop})
#'   \item G-test (log likelihood ratio test of independence)
#'   \item Mantel-Haenszel chi-squared test
#'   \item Contingency coefficient
#'   \item Kendall's tau-b with confidence interval
#'   \item Relative risk col1/col2 and row1/row2 with confidence intervals
#'     \emph{(2x2)}
#'   \item Proportions difference with confidence interval \emph{(2x2)}
#' }
#'
#' \strong{verbose = 3 — full output:}
#'
#' All of the above, plus:
#' \itemize{
#'   \item Cross-tabulation: row, column, and total proportions
#'   \item Lambda C|R, R|C, symmetric
#'   \item Uncertainty coefficient C|R, R|C, symmetric
#'   \item Mutual information
#'   \item Goodman-Kruskal gamma with confidence interval
#'   \item Stuart's tau-c with confidence interval
#'   \item Somers' D C|R and R|C with confidence intervals
#'   \item Pearson and Spearman correlation with confidence intervals
#' }
#'
#' \strong{Table types:}
#'
#' For \strong{r x c tables} (arbitrary number of rows and columns) all
#' nominal and ordinal association measures listed above are available.
#' For \strong{2 x 2 tables} the output additionally includes tests and
#' measures specific to the 2 x 2 case (Fisher's exact, McNemar, odds ratio,
#' relative risk, proportions difference).
#'
#' \strong{Dispatching:}
#'
#' \code{Desc.matrix} and \code{Desc.qq} both redirect to \code{Desc.table}.
#' When called via the formula interface \code{Desc(y ~ x, data)}, the type
#' of \code{y} and \code{x} is known and ordinal-specific measures
#' (tau-b and above) are activated automatically when both variables are
#' \code{ordered} factors.
#'
#' @return An object of class \code{c("Desc.table", "Desc")}, invisibly.
#'   The object is a list containing all computed statistics and is intended
#'   to be used via its \code{print} and \code{plot} methods.
#'
#' @seealso
#'   \code{\link{Desc}} for the generic function and formula interface,
#'   \code{\link{Desc.numeric}} for univariate numeric descriptions,
#'   \code{\link{Desc.factor}} for univariate factor descriptions,
#'   \code{\link[stats]{chisq.test}}, \code{\link[stats]{fisher.test}},
#'   \code{\link{CramerV}}, \code{\link{OddsRatio}}
#'
#' @family plotdesc
#' @concept contingency table cross-tabulation association measures
#' @concept chi-squared cramér fisher odds-ratio relative-risk
#' @concept nominal ordinal tau kendall gamma somers lambda
#'
#' @examples
#' # from an existing table
#' tab <- table(d.pizza$driver, d.pizza$area)
#' Desc(tab)
#' Desc(tab, prop = "rows", verbose = 3)
#'
#' # 2x2 table — additional measures are shown automatically
#' tab2 <- tab[1:2, 1:2]
#' Desc(tab2)
#'
#' # formula interface — dispatches to Desc.table internally
#' Desc(driver ~ class, data = d.pizza)
#'
#' # from a matrix
#' m <- matrix(c(153, 153, 167, 123, 108, 109, 89, 122, 167),
#'             nrow = 3, byrow = TRUE,
#'             dimnames = list(c("Brent","Camden","Westminster"),
#'                             c("Allanah","Maria","Rhonda")))
#' Desc(m, verbose = 2)
#'


#' 
#' #' @export
#' Desc.table <- function(x, prop = "rows", verbose = NULL,
#'                        abs.sty = NULL, per.sty = NULL,
#'                        conf.level = 0.95, ...) {
#'   
#'   # resolve format styles: function arg > global option > package default
#'   if (is.null(abs.sty))
#'     abs.sty <- getOption("DescTools.abs.sty", default = .default_abs.sty)
#'   if (is.null(per.sty))
#'     per.sty <- getOption("DescTools.per.sty", default = .default_per.sty)
#'   
#' }


#' @rdname Desc
#' @method Desc table
#' @export
Desc.table <- function(x, conf.level = 0.95, prop = "rows",
                       main = NULL, verbose = NULL, plotit = NULL,
                       ...) {

  
  .chisq_independence <- function(x, correct = FALSE) {
    
    d <- dim(x)
    n <- sum(x)
    
    # expected values under complete independence
    marg_probs <- lapply(seq_along(d), function(j) {
      prop.table(margin.table(x, j))
    })
    E <- Reduce(function(a, b) outer(a, b), marg_probs) * n
    
    O <- x
    df <- prod(d - 1)
    
    stat <- if (correct && identical(d, c(2L, 2L))) {
      sum((pmax(abs(O - E) - 0.5, 0))^2 / E)
    } else {
      sum((O - E)^2 / E)
    }
    
    structure(
      list(
        statistic = c("X-squared" = stat),
        parameter = c("df" = df),
        p.value   = pchisq(stat, df, lower.tail = FALSE),
        method    = if (correct && identical(d, c(2L, 2L)))
          "Pearson's Chi-squared test with Yates' continuity correction"
        else
          "Pearson's Chi-squared test for independence",
        expected  = E,
        approx.ok = !any(E < 5)
      ),
      class = "htest"
    )
  }

  # resolve verbose: function arg > global option > hardcoded default
  verbose <- verbose %||% getOption("DescTools.verbose", default = 2L)
  
  
  ttype <- if (identical(dim(x), c(2L, 2L))) {
    "t2x2"
  } else if (length(dim(x)) > 2) {
    "tndim"
  } else if (length(dim(x)) < 2) {
    "t1dim"
  } else {
    "trxc"
  }
  
  r.chisq <- .chisq_independence(x)
  
  res <- list(
    
    meta = .descMeta(x, deparse(substitute(x)), main, plotit, verbose),
    
    n = sum(x),
    dim = dim(x),
    # unique = NULL,
    ttype = ttype,
    verbose = verbose,
    conf.level = conf.level,
    chisq.test = r.chisq, 
    chisq.test.cont = if (ttype == "t2x2") {
                        .chisq_independence(x, correct = TRUE)
                      } else { NULL },
    loglik.chisq.test = if (ttype != "tndim") {
                          suppressWarnings(gTest(x))
                        } else { NULL },
    mh.test = if (ttype %in% c("t2x2", "trxc")) mhChisqTest(x) else NULL,
    fisher.test = if (ttype == "t2x2") fisher.test(x) else NULL,
    mcnemar.test = if (ttype == "t2x2") mcnemar.test(x),
    or = if (ttype == "t2x2") oddsRatio(x, conf.level = conf.level),
    relRisk1 = if (ttype == "t2x2") {
      relRisk(x, conf.level = conf.level, method = "wald", delta = 0)
    },
    relRisk2 = if (ttype == "t2x2") {
      relRisk(revX(x, margin = 2), conf.level = conf.level, method = "wald", delta = 0)
    },
    propdiff = if (ttype == "t2x2") {
      binomDiffCI(x[1,1], sum(x[1,]), x[2,1], sum(x[2,]), 
                  conf.level = conf.level, 
                  method = "miettinen-nurminen")
    },
    relRisk1r = if (ttype == "t2x2") {
      relRisk(t(x), conf.level = conf.level, method = "wald", 
              delta = 0)
    },
    relRisk2r = if (ttype == "t2x2") {
      relRisk(t(revX(x, margin = 1)), conf.level = conf.level, method = "wald", delta = 0)
    },
    assocs = if (ttype %in% c("t2x2", "trxc")) {
      assocs(x, conf.level = conf.level, out="ext") 
    } else {
      NULL
    },
    tab = x,
    pfreq = prop.table(x),
    pfreqr = if (ttype != "t1dim") prop.table(x, 1) else NULL,
    pfreqc = if (ttype != "t1dim") prop.table(x, 2),
    perctab = if (ttype == "t1dim") {
      freq(x)
    } else if (ttype == "tndim") {
      NULL
    } else {
      percTable(x, margins=c(1,2), prop=prop, ...)
    },
    approx.ok = if (ttype == "tndim") {
      r.chisq$approx.ok
    } else {
      !(any(r.chisq$expected < 5) && is.finite(r.chisq$parameter))
    }
  )

  
  class(res) <- c("Desc.table", "Desc")
  return(res)
  
}




#' @rdname Desc
#' @export
Desc.matrix <- Desc.table 
                       

#' @rdname Desc
#' @export
Desc.array <- Desc.table 


#' @rdname Desc
#' @export
print.Desc.table <- function(x, print_header=TRUE, ...) {
  
  if(print_header)
    .printHeader(x$meta)
  
  # x[c(6, 8)] <- NULL
  
  footnote <- .getOption("footnote")[1]
  
  if (x$ttype == "tndim") { # multdim table
    
    if(print_header)
      cat("Summary: \n",
          "n: ", fm(x$n, fmt = "abs.sty"), ", ",
          length(x$dim), "-dim table: ", paste(x$dim, collapse = " x "),
          "\n\n",
          sep = ""
        )
    
    print(ftable(addmargins(x$tab, c(1, length(x$dim)))))
    cat("\n")

    cat(gettextf(
      "%s\n  X-squared = %s, df = %s, p-value = %s",
      x[["chisq.test"]][["method"]],
      fm(x[["chisq.test"]][["statistic"]], digits = 3),
      x[["chisq.test"]][["parameter"]],
      fm(x[["chisq.test"]][["p.value"]], fmt = "p")
    ), "\n", sep = "")
    if (!x$approx.ok) {
      cat(cli::col_red("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n"))
    }
    
    cat("\n")
  } else { # <= 2-dimensional table
    
    
    if (x$ttype == "t1dim") { # 1-dim table ****
      cat("Summary: \n",
          "n: ", fm(x$n, fmt = "abs.sty"),
          ", rows: ", x$dim[1],
          "\n\n",
          sep = ""
      )
      cat("Pearson's Chi-squared test (1-dim uniform):\n  ",
          .captOut(x$chisq.test)[5], "\n\n",
          sep = ""
      )
      if (!x$approx.ok) {
        cat(cli::col_cyan("  Note: expected counts < 5 in some cells\n"))
      }
      
      # if (!x$approx.ok) {
      #   .ChisqWarning()
      # }
      
      print(x$perctab)
      
    } else { # 2-dim tabl *****
      
      if (!is.null(attr(x, "missings"))) {
        missn <- paste(",", attr(x, "missings"), paste = "")
      } else {
        missn <- ""
      }
      
      # 1. summary --------------------------------------------
      if(print_header)
        cat("Summary: \n",
            "n: ", fm(x$n, fmt = "abs.sty"),
            ", rows: ", fm(x$dim[1], fmt = "abs.sty"),
            ", columns: ", fm(x$dim[2], fmt = "abs.sty"),
            missn,
            "\n\n",
            sep = ""
        )
      
      # 2. data --------------------------------------------
      print(x$perctab)
      cat("\n\n")
      
      # 3. inference --------------------------------------------
      if (x$ttype == "t2x2") {
        if (x$meta$verbose == "3") {
          cat("Pearson's Chi-squared test:\n  ",
              .captOut(x$chisq.test)[5], "\n",
              sep = ""
          )
        }
        if (!x$approx.ok) {
          cat(cli::col_cyan("  Note: expected counts < 5 in some cells\n"))
        }
        cat("Pearson's Chi-squared test (cont. adj):\n  ",
            .captOut(x$chisq.test.cont)[5], "\n",
            sep = ""
        )
        cat("Fisher's exact test ",
            .captOut(x$fisher.test)[5], "\n",
            sep = ""
        )
        
        if (x$meta$verbose %in% c("2", "3")) { # print only with verbosity > 1
          cat("", .captOut(x$mcnemar.test)[5], "\n", sep = "")
        }
        
        # if (!x$approx.ok) {
        #   .ChisqWarning()
        # }
        
        if (x$meta$verbose %in% c("2", "3")) { # print only with verbosity > 1
          cat("\n")
          if (x$meta$verbose == "2") {
            m <- ftable(fm(rbind(
              "odds ratio    " = x$or,
              "rel. risk (col1)  " = x$relrisk1,
              "rel. risk (col2)  " = x$relrisk2,
              "prop. diff " = x$propdiff
            ), digits = 3, nsmall = 3))
          } else {
            m <- ftable(fm(rbind(
              "odds ratio    " = x$or,
              "rel. risk (col1)  " = x$relrisk1,
              "rel. risk (col2)  " = x$relrisk2,
              "rel. risk (row1)  " = x$relrisk1r,
              "rel. risk (row2)  " = x$relrisk2r,
              "prop. diff        " = x$propdiff
            ), digits = 3, nsmall = 3))
          }
          attr(m, "col.vars")[[1]][1] <- "estimate"
          txt <- capture.output(print(m))
          txt[1] <- paste(txt[1], footnote, sep = "")
          cat(txt, sep = "\n")
          cat("\n")
        }
      } else {
        # we report chisquare without cont-corr for rxc and with cont-corr for 2x2 by default
        cat("Pearson's Chi-squared test:\n  ",
            .captOut(x$chisq.test)[5], "\n",
            sep = ""
        )
        if (!x$approx.ok) {
          cat(cli::col_cyan("  Note: expected counts < 5 in some cells\n"))
        }
        
        if (x$meta$verbose == "3" &  x$ttype == "t2x2") {
          cat("Pearson's Chi-squared test (cont. adj):\n  ",
              .captOut(x$chisq.test.cont)[5], "\n",
              sep = ""
          )
        }
        
        if (x$meta$verbose > 1) { # print only with verbosity > 1
          
          # Log-likelihood chi-squared (G2) test of independence (homogeneity)
          cat("Log likelihood ratio (G-test) test of independence:\n  ",
              .captOut(x$loglik.chisq.test)[5], "\n",
              sep = ""
          )
          # Mantel-Haenszel ChiSquared (linear hypothesis)
          cat("Mantel-Haenszel Chi-squared:\n  ",
              .captOut(x$mh.test)[5], "\n",
              sep = ""
          )
        }
        

      switch(x$meta$verbose,
             "1" = {
               cat("\n")
             },
             "2" = {
               cat(sprintf(
                 "\nContingency Coeff.     %.3f\nCramer's V             %.3f\nKendall Tau-b          %.3f\n",
                 x$assocs[1, 1],
                 x$assocs[2, 1],
                 x$assocs[3, 1]
               ))
               cat("\n")
             },
             "3" = {
               cat("\n")
               txt <- capture.output(x$assocs)
               txt[1] <- paste(txt[1], footnote, sep = "")
               cat(txt, sep = "\n")
               cat("\n")
             }
      )
      
      if ((x$meta$verbose == "3") || (x$ttype == "t2x2")) {
        
        out <- gettextf( "\n%s\n%s %s%s conf. level\n", 
                         strrep("\u2500", 20), 
                         footnote, x$conf.level * 100, "%" )
        if (.has_color()) {
          cat(cli::col_silver(out))
        } else {
          cat(out)
        }
      }
    }
    
    cat("\n")
  }
  }
  
  if(x$meta$plotit)
    plot(x, main=x$meta$main)
  
}


# no export here, all code in aurora 
# plot.Desc.table <- aurora::plot.Desc.table



