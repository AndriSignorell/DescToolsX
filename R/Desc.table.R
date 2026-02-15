
#' Descriptive analysis of contingency tables
#'
#' @description
#' Performs a comprehensive descriptive and inferential analysis of a
#' contingency table. Depending on the table dimension, appropriate
#' chi-squared tests, exact tests, measures of association, risk estimates,
#' and percentage tables are computed.
#'
#' For two-by-two tables, additional measures such as odds ratios,
#' relative risks, Fisher's exact test, McNemar's test, and differences
#' in proportions are provided.
#'
#' @name Desc.table
#' 
#' @aliases Desc.table Desc.matrix Desc.array
#' @details
#' The function automatically determines the table type (one-dimensional,
#' two-by-two, r-by-c, or multi-dimensional) and adapts the set of
#' statistical procedures accordingly.
#'
#' Percentage tables are computed using \code{percTable()} for applicable
#' table types. Graphical and fmting parameters for percentage tables
#' can be passed via \code{...}.
#'
#' @param x
#' A contingency table (matrix or array of non-negative counts).
#' @param ...
#' Additional arguments passed to \code{percTable()}.
#'
#' @return
#' A list containing descriptive statistics, test results, measures of
#' association, frequency and percentage tables, and diagnostic infmion
#' about the validity of chi-squared approximations.
#'
#' @seealso
#' \code{\link{chisq.test}},
#' \code{\link{fisher.test}},
#' \code{\link{oddsRatio}},
#' \code{\link{relRisk}},
#' \code{\link{assocs}},
#' \code{\link{percTable}}
#'
#' @examples
#' tab <- matrix(c(12, 5, 7, 9), nrow = 2)
#' Desc(tab)
#' Desc(tab, verbose=3)
#' 
#' Desc(marginSums(HairEyeColor, c(3,2)))
#' Desc(marginSums(HairEyeColor, c(3,2)), verbose=3)
#'


#' @rdname Desc
#' @method Desc table
#' @export
Desc.table <- function(x, conf.level = 0.95, 
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
                  conf.level = conf.level, method = "mn")[1,]
    },
    relRisk1r = if (ttype == "t2x2") {
      relRisk(t(x), conf.level = conf.level, method = "wald", delta = 0)
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
      percTable(x, margins=c(1,2), ...)
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
print.Desc.table <- function(x, ...) {
  
  .printHeader(x$meta)
  
  # x[c(6, 8)] <- NULL
  
  footnote <- .getOption("footnote")[1]
  
  if (x$ttype == "tndim") { # multdim table
    
    cat("Summary: \n",
        "n: ", fm(x$n, fmt = "abs.sty"), ", ",
        length(x$dim), "-dim table: ", paste(x$dim, collapse = " x "),
        "\n\n",
        sep = ""
    )
    
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
    print(ftable(addmargins(x$tab, c(1, length(x$dim)))))
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
          .CaptOut(x$chisq.test)[5], "\n\n",
          sep = ""
      )
      
      if (!x$approx.ok) {
        .ChisqWarning()
      }
      
      print(x$perctab)
    } else { # 2-dim tabl *****
      
      if (!is.null(attr(x, "missings"))) {
        missn <- paste(",", attr(x, "missings"), paste = "")
      } else {
        missn <- ""
      }
      
      cat("Summary: \n",
          "n: ", fm(x$n, fmt = "abs.sty"),
          ", rows: ", fm(x$dim[1], fmt = "abs.sty"),
          ", columns: ", fm(x$dim[2], fmt = "abs.sty"),
          missn,
          "\n\n",
          sep = ""
      )
      
      if (x$ttype == "t2x2") {
        if (x$meta$verbose == "3") {
          cat("Pearson's Chi-squared test:\n  ",
              .CaptOut(x$chisq.test)[5], "\n",
              sep = ""
          )
        }
        cat("Pearson's Chi-squared test (cont. adj):\n  ",
            .CaptOut(x$chisq.test.cont)[5], "\n",
            sep = ""
        )
        cat("Fisher's exact test ",
            .CaptOut(x$fisher.test)[5], "\n",
            sep = ""
        )
        
        if (x$meta$verbose %in% c("2", "3")) { # print only with verbosity > 1
          cat("", .CaptOut(x$mcnemar.test)[5], "\n", sep = "")
        }
        
        if (!x$approx.ok) {
          .ChisqWarning()
        }
        
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
            .CaptOut(x$chisq.test)[5], "\n",
            sep = ""
        )
        
        if (x$meta$verbose == "3") {
          cat("Pearson's Chi-squared test (cont. adj):\n  ",
              .CaptOut(x$chisq.test.cont)[5], "\n",
              sep = ""
          )
        }
        
        if (x$meta$verbose > 1) { # print only with verbosity > 1
          
          # Log-likelihood chi-squared (G2) test of independence (homogeneity)
          cat("Log likelihood ratio (G-test) test of independence:\n  ",
              .CaptOut(x$loglik.chisq.test)[5], "\n",
              sep = ""
          )
          # Mantel-Haenszel ChiSquared (linear hypothesis)
          cat("Mantel-Haenszel Chi-squared:\n  ",
              .CaptOut(x$mh.test)[5], "\n",
              sep = ""
          )
        }
        
        if (!x$approx.ok) {
          .ChisqWarning()
        }
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
      
      print(x$perctab)
      
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
  
  if(x$meta$plotit)
    plot(x, main=x$meta$main)
  
}


# no export here, all code in DescToolsViz 
# plot.Desc.table <- DescToolsViz::plot.Desc.table



