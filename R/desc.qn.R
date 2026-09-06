
#' @name desc.qn
#' @aliases .descQN
#'
#' @title Describe Relationship: Categorical y vs Numeric x
#'
#' @description
#' Computes descriptive statistics for the relationship between a categorical
#' variable `y` and a numeric variable `x`.
#'
#' @param y a categorical variable (factor or coercible to factor)
#' @param x a numeric variable
#' @param conf.level confidence level for interval estimates (default 0.95)
#' @param breaks numeric vector defining cut points for `x`.
#'   If not supplied, quartiles of `x` are used.
#' @param right logical; passed to `cut()`, defining interval closure
#' 
#' @param ... further arguments passed to methods
#' @param verbose amount of printed output
#'
#' @details
#' The function summarizes how a numeric variable `x` differs across
#' levels of a categorical variable `y`.
#'
#' **Computed statistics**
#' \itemize{
#'   \item Group-wise descriptive statistics (median, IQR, counts)
#'   \item Kruskal-Wallis test with effect size (\eqn{\eta^2})
#'   \item Levene's test for homogeneity of variance
#'   \item Kendall's Tau-b with confidence interval and p-value
#'   \item Spearman correlation (reported for higher verbosity levels)
#' }
#'
#' **Binary outcomes**
#' If `y` has two levels:
#' \itemize{
#'   \item Area under the curve (AUC)
#'   \item Prevalence across quantile groups of `x`
#'   \item Cochran-Armitage trend test
#' }
#'
#' **Quantile grouping**
#' The numeric variable `x` is optionally discretized using
#' `breaks`. By default, quartiles are used.
#'
#' @return an object of class `c("Desc.qn", "Desc")` with components:
#' \describe{
#'   \item{`grpTable`}{group-wise summary table}
#'   \item{`kw`}{result of the Kruskal-Wallis test}
#'   \item{`eta2`}{effect size}
#'   \item{`levene`}{result of Levene's test}
#'   \item{`tauB`}{estimate, confidence interval, and p-value for
#'     Kendall's tau-b}
#'   \item{`spearman`}{estimate, confidence interval, and p-value for
#'     Spearman's correlation}
#'   \item{`auc`}{area under the curve for a binary outcome}
#'   \item{`prevTable`}{prevalence table for a binary outcome with columns:
#'     \describe{
#'       \item{`quantile`}{quantile group}
#'       \item{`n`}{number of complete cases in the group}
#'       \item{`est`}{point estimate of the prevalence}
#'       \item{`lci`}{lower confidence interval bound}
#'       \item{`uci`}{upper confidence interval bound}
#'     }}
#'   \item{`caTest`}{result of the Cochran-Armitage test for a binary
#'     outcome}
#' }
#'
#' @seealso
#' [desc()], [desc.nn()], [desc.nq()],
#' [kruskal.test()], [lumen::leveneTest()]
#'
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept association-measures
#'
#' @rdname desc.qn
#' @usage .descQN(y, x, conf.level = 0.95, breaks, right)
NULL


.descQN <- function(y, x, conf.level = 0.95,
                     breaks = quantile(x, probs = c(0.25, 0.5, 0.75),
                                         na.rm = TRUE),
                     right=FALSE) {
  
  # ── 1. Basic counts (summary bereits in Hauptfunktion) ───────────────────────
  ok     <- complete.cases(x, y)
  xOk   <- x[ok]
  yOk   <- if (is.factor(y[ok])) y[ok] else factor(y[ok])
  nValid <- sum(ok)
  lvls  <- levels(yOk)
  k     <- nlevels(yOk)
  if (k < 2L)
    stop("'y' must contain at least two distinct levels after removing missing values")
  
  # ── 2. Gruppenweise Kennzahlen via .buildSummaryTable ──────────────────────
  grpTable <- .buildSummaryTable(
    tapply(xOk, yOk, desc, plotit = FALSE)
  )
  
  # ── 3. Kruskal-Wallis + eta² ─────────────────────────────────────────────────
  kw   <- kruskal.test(xOk ~ yOk)
  eta2   = .eta2Kruskal(H = kw$statistic, 
                         k = k,         # already computed
                         n = nValid)    # also there..
  
  # ── 4. Levene ────────────────────────────────────────────────────────────────
  levene <- leveneTest(xOk ~ yOk)
  
  # ── 5. AUC via cStat (nur binär) ─────────────────────────────────────────────
  auc <- if (k == 2L) cStat(xOk, yOk) else NULL
  
  # ── 6. Kendall Tau-b ─────────────────────────────────────────────────────────
  tauB   <- kendallTauB(as.integer(yOk), xOk, conf.level = conf.level)
  tauBCI <- tauB
  tauB <- tauB[1]
  tauBP  <- cor.test(as.integer(yOk), xOk,
                     method = "kendall", exact = FALSE)$p.value
  
  # ── 7. Spearman (verbose >= 3) ───────────────────────────────────────────────
  spearmanR  <- cor(as.integer(yOk), xOk, method = "spearman")
  spearmanCI <- corCI(spearmanR, n = nValid, conf.level = conf.level)
  spearmanP  <- cor.test(as.integer(yOk), xOk,
                         method = "spearman", exact = FALSE)$p.value
  
  # ── 8. Prävalenz-Tabelle mit Wilson-CI + Cochran-Armitage ────────────────────
  bNorm <- .normalizeBreaks(breaks, xOk, right = right)
  
  xCut  <- cut(xOk,
               breaks         = bNorm,
               labels         = attr(bNorm, "labels"),
               include.lowest = TRUE,
               right          = right)
  
  
  # Prävalenz pro Quantil-Klasse — nur für binäres y sinnvoll
  prevTable <- if (k == 2L) {
    qLevels <- levels(xCut)
    do.call(rbind, lapply(qLevels, function(ql) {
      idx  <- xCut == ql
      ni   <- sum(idx)
      xi   <- sum(yOk[idx] == lvls[2L])   # positive class = second level
      ci   <- binomCI(xi, ni,
                      conf.level = conf.level, method = "wilson")
      data.frame(
        quantile = ql,
        n        = ni,
        est      = ci[1L],
        lci      = ci[2L],
        uci      = ci[3L]
      )
    }))
  } else {
    # multinomial: Häufigkeitstabelle y x xCut
    prop.table(table(yOk, xCut), margin = 2L)
  }
  
  # Cochran-Armitage Trend (nur binär)
  caTest <- if (k == 2L) {
    cochranArmitageTest(table(yOk, xCut))
  } else {
    NULL
  }
  
  # ── Assemble ─────────────────────────────────────────────────────────────────
  structure(
    list(
      conf.level = conf.level,
      k          = k,
      lvls       = lvls,
      breaks     = breaks,
      grpTable   = grpTable,
      kw         = kw,
      eta2       = eta2,
      levene     = levene,
      auc        = auc,
      tauB       = list(r = tauB,      ci = tauBCI, p = tauBP),
      spearman   = list(r = spearmanR, ci = spearmanCI, p = spearmanP),
      prevTable  = prevTable,
      caTest     = caTest,
      xOk        = xOk,
      yOk        = yOk
    ),
    class = c("Desc.qn", "Desc")
  )
}



# ── Print ─────────────────────────────────────────────────────────────────────
  
#' @rdname desc.qn
#' @export
print.Desc.qn <- function(x, verbose = NULL, ...) {

  verbose <- verbose %||% x$meta$verbose %||% getOption("DescTools.verbose", default = 2L)
  
  .printHeader(x$meta)
  
  cat(x$pair$strOut)
  
  # ── Gruppenweise Kennzahlen ───────────────────────────────────────────────────
  
  # Zeilenauswahl nach verbose
  rows <- switch(as.character(verbose),
                 "1" = c("median", "n", "np"),
                 "2" = c("median", "iqr", "n", "np"),
                 x$res$grpTable   # verbose = 3: alles, direkt zurückgeben
  )
  
  if (verbose <= 2L) {
    printCharMatrix(x$res$grpTable[rows, , drop = FALSE], sep = 3)
  } else {
    printCharMatrix(x$res$grpTable, sep=3)
  }
  
  
  fmtR <- function(r, ci, p){
    sprintf("%6.3f  (%6.3f, %6.3f)  %s  %s",
            r, ci[2], ci[3], .stars(p), .rLabel(r))
  }

  # ── Kruskal-Wallis + eta² ────────────────────────────────────────────────────
  cat("Kruskal-Wallis rank sum test:\n")
  cat(sprintf("  H = %s,  df = %d,  p = %s\n",
              fm(unname(x$res$kw$statistic), fmt = "abs.sty"),
              unname(x$res$kw$parameter),
              format.pval(x$res$kw$p.value, digits = 3, eps = 0.001)))
  cat(sprintf("  \u03b7\u00b2 = %.3f (%s)\n",
              x$res$eta2, attr(x$res$eta2, "label")))
  
  # ── Levene ───────────────────────────────────────────────────────────────────
  if (verbose >= 2L) {
    cat("\nLevene's test for homogeneity of variance (center = median):\n")
    cat(sprintf("  F = %.4f,  df1 = %d,  df2 = %d,  p = %s\n",
                x$res$levene[[1L]][1L],
                as.integer(x$res$levene[[2L]][1L]),
                as.integer(x$res$levene[[2L]][2L]),
                format.pval(x$res$levene[[3L]][1L], digits = 3, eps = 0.001)))
  }
  
  # ── AUC ──────────────────────────────────────────────────────────────────────
  if (!is.null(x$res$auc)) {
    cat(sprintf("\nAUC = %s  (%s vs %s)\n",
                fm(x$res$auc, fmt = "per.sty"),
                x$res$lvls[1L], x$res$lvls[2L]))
  }
  
  # ── Tau-b ────────────────────────────────────────────────────────────────────
  if (verbose >= 2L) {
    cat(sprintf("\nKendall Tau-b:  %s\n",
                fmtR(x$res$tauB$r, x$res$tauB$ci, x$res$tauB$p)))
  }
  
  # ── Spearman ─────────────────────────────────────────────────────────────────
  if (verbose >= 3L) {
    cat(sprintf("Spearman r:     %s\n",
                fmtR(x$res$spearman$r, x$res$spearman$ci, x$res$spearman$p)))
  }
  
  # ── Prävalenz-Tabelle + Cochran-Armitage ─────────────────────────────────────
  if (!is.null(x$res$prevTable) && is.data.frame(x$res$prevTable)) {
    cat(sprintf("\nPrevalence of \"%s\" by groups (Wilson CI):\n",
                x$res$lvls[2L]))
    cat(sprintf("  %-18s  %6s  %7s  %7s  %7s\n",
                "", "n", "est", "lci", "uci"))
    cat(strrep("-", 52L), "\n")
    for (i in seq_len(nrow(x$res$prevTable))) {
      r <- x$res$prevTable[i, ]
      cat(sprintf("  %-18s  %6s  %7s  %7s  %7s\n",
                  r$quantile,
                  fm(r$n,    fmt = "abs.sty"),
                  fm(r$est,  fmt = "per.sty"),
                  fm(r$lci,  fmt = "per.sty"),
                  fm(r$uci,  fmt = "per.sty")))
    }
    if (!is.null(x$res$caTest)) {
      cat(sprintf("\nCochran-Armitage trend test:\n"))
      cat(sprintf("  Z = %.3f,  p = %s\n",
                  unname(x$res$caTest$statistic),
                  format.pval(x$res$caTest$p.value, digits = 3, eps = 0.001)))
    }
  } else if (!is.null(x$res$prevTable)) {
    # multinomial: einfache Häufigkeitstabelle
    cat("\nConditional distribution of y by x-quantile:\n")
    print(fm(x$res$prevTable, fmt = "%", digits = 1L))
    
  }
  
  cat("\n")
  
  
  
  invisible(x)
  
}

  
# ── Plot ──────────────────────────────────────────────────────────────────────
  
# ── plot.Desc.qn — qualitative y ~ quantitative x ────────────────────────────
#
# which:
#   1  Spineplot                          (default)
#   2  Conditional density plot (cdplot)
#   3  Overlapping density per group
#   4  Boxplot
#   5  Prevalence + Wilson-CI along x     (binary y only)
#
# Default by verbose:
#   verbose = 1  →  which = 1
#   verbose = 2  →  which = 1:2
#   verbose = 3  →  which = 1:2  (+5 if binary y)
#
# Design parameters (line width, colors, point size etc.) will be
# governed by DescToolsX design rules once defined.


# plot.Desc.qn is defined in pharos



# == internal helper functions =================================================

# breaks normalisieren — Inf/-Inf nur ergänzen wenn nötig
.normalizeBreaks <- function(breaks, x, right = FALSE, fmt = "abs.sty") {
  
  xMin <- min(x, na.rm = TRUE)
  xMax <- max(x, na.rm = TRUE)
  
  # Ränder ergänzen wenn nötig
  if (xMin < breaks[1L])
    breaks <- c(-Inf, breaks)
  if (xMax > breaks[length(breaks)])
    breaks <- c(breaks, Inf)
  
  # endliche Grenzwerte für Labels
  bLower <- ifelse(is.infinite(breaks[-length(breaks)]),
                   xMin, breaks[-length(breaks)])
  bUpper <- ifelse(is.infinite(breaks[-1L]),
                   xMax, breaks[-1L])
  
  nLev  <- length(breaks) - 1L
  
  if (right) {
    labs <- sprintf("(%s\u2013%s]",
                    fm(bLower, fmt = fmt),
                    fm(bUpper, fmt = fmt))
  } else {
    labs <- sprintf("[%s\u2013%s)",
                    fm(bLower, fmt = fmt),
                    fm(bUpper, fmt = fmt))
  }

  attr(breaks, "labels") <- labs
  breaks
}



  
