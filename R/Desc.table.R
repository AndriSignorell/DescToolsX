
#' Describe a Contingency Table
#'
#' Computes and displays a comprehensive set of descriptive statistics and
#' association measures for a contingency table (r x c or 2 x 2). The function
#' is also dispatched for `matrix` and cross-classified factor pairs via
#' `Desc.qq` and `Desc.matrix`.
#'
#' @param x a `table` or `matrix` object. For the formula interface,
#'   use `desc(y ~ x, data)` which dispatches to this function
#'   automatically.
#' @param prop character string controlling which proportions are shown in the
#'   cross-tabulation. One of `"rows"`, `"cols"`, `"total"`, or
#'   `"no"` (frequencies only). If `NULL` (default), `"rows"` is used,
#'   and all three proportions at `verbose = 3`; an explicit value is
#'   always respected.
#' @param verbose integer controlling the amount of output (1, 2, or 3).
#'   `NULL` (default) falls back to
#'   `getOption("DescTools.verbose", 2)`. If set explicitly in the
#'   function call, that value takes priority over the global option.
#'   See Details for what each level produces.
#' @param conf.level numeric, confidence level for all confidence intervals.
#'   Default is `0.95`.
#' @param \dots further arguments passed to or from other methods
#' @param which plots to produce
#' @param main main title for the plot
#' @param plotit whether a plot is produced automatically
#' @param digits number of digits for numerical output
#' @param print_header whether the header is printed
#'   
#' @name desc.table
#' @details
#' The `verbose` argument controls which statistics are displayed. All
#' of them are computed in any case; the lists below describe the printed
#' output. The cross-tabulation always shows the frequencies together with
#' the proportions selected by `prop`.
#'
#' **2 x 2 tables**
#' \itemize{
#'   \item `verbose = 1`: chi-squared test with Yates continuity
#'     correction, Fisher's exact test
#'   \item `verbose = 2` (default): additionally McNemar's test and a table
#'     of odds ratio, relative risks (col1, col2) and proportion difference,
#'     each with confidence interval
#'   \item `verbose = 3`: additionally the uncorrected Pearson chi-squared
#'     test, and relative risks (row1, row2), Cramér's V and Cohen's h in
#'     the table of estimates
#' }
#'
#' **r x c tables**
#' \itemize{
#'   \item `verbose = 1`: Pearson chi-squared test
#'   \item `verbose = 2` (default): additionally the G-test (log likelihood
#'     ratio) and the Mantel-Haenszel chi-squared test, and the point
#'     estimates of the first three association measures
#'   \item `verbose = 3`: the full table of nominal and ordinal association
#'     measures with confidence intervals (Cramér's V, contingency
#'     coefficient, lambda, uncertainty coefficient, mutual information,
#'     gamma, tau-b, tau-c, Somers' D, Pearson and Spearman correlation)
#' }
#'
#' **Table types:**
#'
#' For **r x c tables** (arbitrary number of rows and columns) all
#' nominal and ordinal association measures listed above are available.
#' For **2 x 2 tables** the output additionally includes tests and
#' measures specific to the 2 x 2 case (Fisher's exact, McNemar, odds ratio,
#' relative risk, proportions difference).
#'
#' **Dispatching:**
#'
#' `desc.matrix` and `desc.qq` both redirect to `desc.table`.
#' When called via the formula interface `desc(y ~ x, data)`, the type
#' of `y` and `x` is known and ordinal-specific measures
#' (tau-b and above) are activated automatically when both variables are
#' `ordered` factors.
#'
#' @return an object of class `c("Desc.table", "Desc")`.
#'   The object is a list containing all computed statistics and is intended
#'   to be used via its `print` and `plot` methods.
#'
#' @seealso
#'   [desc] for the generic function and formula interface,
#'   [desc.numeric] for univariate numeric descriptions,
#'   [desc.factor] for univariate factor descriptions,
#'   [pharos::plot.Desc.table] for different plotting options,
#'   [stats::chisq.test], [stats::fisher.test],
#'   [cramerV], [oddsRatio]
#'
#' @examples
#' # from an existing table
#' tab <- table(Pizza$driver, Pizza$area)
#' desc(tab)
#' desc(tab, prop = "rows", verbose = 3)
#'
#' # 2x2 table — additional measures are shown automatically
#' tab2 <- tab[1:2, 1:2]
#' desc(tab2)
#'
#' # formula interface — dispatches to desc.table internally
#' desc(driver ~ area, data = Pizza)
#'
#' # from a matrix
#' m <- matrix(c(153, 153, 167, 123, 108, 109, 89, 122, 167),
#'             nrow = 3, byrow = TRUE,
#'             dimnames = list(c("Brent","Camden","Westminster"),
#'                             c("Allanah","Maria","Rhonda")))
#' desc(m, verbose = 2)
#'

#' 
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept table-manipulation
#'



#' @method desc table
#' @export
desc.table <- function(x, conf.level = 0.95, prop = NULL,
                       main = NULL, verbose = NULL, plotit = NULL,
                       ...) {

  # resolve verbose: function arg > global option > hardcoded default
  verbose <- .checkVerbose(verbose)
  
  prop <- prop %||% (if(verbose>2) c("rows","cols","total") else "rows")
  
  ttype <- if (identical(dim(x), c(2L, 2L))) {
    "t2x2"
  } else if (length(dim(x)) > 2) {
    "tndim"
  } else if (length(dim(x)) < 2) {
    "t1dim"
  } else {
    "trxc"
  }
  
  r.chisq <- .chisqIndependence(x)
  
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
                        .chisqIndependence(x, correct = TRUE)
                      } else { NULL },
    loglik.chisq.test = if (ttype != "tndim") {
                          suppressWarnings(gTest(x))
                        } else { NULL },
    mh.test = if (ttype %in% c("t2x2", "trxc")) mantelTrendTest(x) else NULL,
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
    cohenH = if (ttype == "t2x2") {
      cohenH(x, conf.level = conf.level)
    },
    assocs = if (ttype %in% c("t2x2", "trxc")) {
      .assocsTab(x, conf.level = conf.level, verbose=verbose) 
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




#' @rdname desc.table
#' @export
desc.matrix <- desc.table 
                       

#' @rdname desc.table
#' @export
desc.array <- desc.table 


#' @rdname desc.table
#' @export
print.Desc.table <- function(x, print_header=TRUE, ...) {
  
  if(print_header)
    .printHeader(x$meta)
  
  # x[c(6, 8)] <- NULL
  
  footnote <- .getOption("footnote")[1]

  # legend for the footnote marker on a table of estimates with intervals;
  # printed wherever such a table is printed
  printCiLegend <- function() {
    out <- gettextf("\n%s\n%s %s%s conf. level\n",
                    strrep("\u2500", 20),
                    footnote, x$conf.level * 100, "%")
    cat(if (.hasColor()) cli::col_silver(out) else out)
  }
  
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

        if (x$meta$verbose %in% c("2", "3")) { # print only with verbosity > 1
          cat("\n")
          if (x$meta$verbose == "2") {
            m <- ftable(fm(rbind(
              "odds ratio    " = x$or,
              "rel. risk (col1)  " = x$relRisk1,
              "rel. risk (col2)  " = x$relRisk2,
              "prop. diff        " = x$propdiff
            ), digits = 3, align = "\\r"))
          } else {
            m <- ftable(fm(rbind(
              "odds ratio    " = x$or,
              "rel. risk (col1)  " = x$relRisk1,
              "rel. risk (col2)  " = x$relRisk2,
              "rel. risk (row1)  " = x$relRisk1r,
              "rel. risk (row2)  " = x$relRisk2r,
              "prop. diff        " = x$propdiff,
              "Cramer's V        " = x$assocs[1,],
              "Cohen's H         " = x$cohenH
            ), digits = 3, align = "\\r"))
          }
          attr(m, "col.vars")[[1]][1] <- "est"
          txt <- capture.output(print(m))
          txt[1] <- paste(txt[1], footnote, sep = "")
          cat(txt, sep = "\n")
          # the legend used to sit in the r x c branch behind a
          # `|| ttype == "t2x2"` that could never be true there, so the
          # marker on this table was never explained
          printCiLegend()
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
        

      # as.character(): switch() on a number selects by POSITION, which
      # only coincided with the labels because they happen to be 1, 2, 3
      switch(as.character(x$meta$verbose),
             "1" = {
               cat("\n")
             },
             "2" = {
               cat("\n")
               printCharMatrix(fm(x$assocs[1:3, 1], 3))
               cat("\n")
               
             },
             "3" = {
               cat("\n")
                 print(fm(as.data.frame(x$assocs),
                          fmt="num.sty", naForm = "-  "),
                       print.gap=3)
             }
      )
      
      if (x$meta$verbose == 3)
        printCiLegend()
    }
    
    cat("\n")
  }
  }
  
  .plotIfRequested(x)
}


# == internal helper functions =================================================

.chisqIndependence <- function(x, correct = FALSE) {
  
  d <- dim(x)
  n <- sum(x)
  
  O <- x

  if (length(d) == 1L) {
    # 1-dim: goodness of fit against the uniform distribution. The
    # "independence" expectation of a single margin is the margin itself,
    # which gave X-squared = 0 and p = 1 for every 1-dim table.
    E  <- rep(n / d, d)
    df <- d - 1L
  } else {
    # expected values under complete (mutual) independence
    marg_probs <- lapply(seq_along(d), function(j) {
      prop.table(margin.table(x, j))
    })
    E  <- Reduce(function(a, b) outer(a, b), marg_probs) * n
    # prod(d - 1) is right for 2 dimensions only; mutual independence
    # in k dimensions has prod(d) - 1 - sum(d - 1), cf. summary.table()
    df <- prod(d) - 1L - sum(d - 1L)
  }
  
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
