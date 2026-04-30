

# ── Desc.nn — quantitative ~ quantitative ────────────────────────────────────
# Two functions:
#   calcDescNn(x, y, conf.level)  →  list with all computed statistics
#   print.Desc.nn(x, ...)         →  formatted console output



#' Describe a Numeric-Numeric Relationship
#'
#' Computes, prints and plots a comprehensive bivariate description for two
#' quantitative variables. The function is dispatched automatically by
#' \code{desc(y ~ x, data)} when both \code{y} and \code{x} are numeric.
#'
#' @param x an object of class \code{"Desc.nn"} as returned by \code{desc()}.
#' @param verbose integer controlling the amount of output (1, 2, or 3).
#'   \code{NULL} (default) falls back to
#'   \code{x$meta$verbose \%||\% getOption("DescTools.verbose", 2)}.
#' @param which integer vector selecting which plots to draw. See Details.
#'   \code{NULL} (default) selects plots automatically based on \code{verbose}.
#' @param abs.sty format style for counts. \code{NULL} falls back to
#'   \code{getOption("DescTools.abs.sty")}.
#' @param per.sty format style for proportions. \code{NULL} falls back to
#'   \code{getOption("DescTools.per.sty")}.
#' @param \dots further arguments passed to the underlying plot functions.
#'
#' @name desc.nn
#' @details
#' \strong{Print output by verbose level:}
#'
#' \describe{
#'   \item{\code{verbose = 1}}{
#'     Summary (n, missings), Pearson r and Spearman r each with confidence
#'     interval and effect size label, linear regression coefficients
#'     (estimate, CI, significance) and R².}
#'   \item{\code{verbose = 2} (default)}{
#'     All of the above, plus residual standard error and Shapiro-Wilk test
#'     on residuals.}
#'   \item{\code{verbose = 3}}{
#'     All of the above, plus Breusch-Pagan test for heteroscedasticity
#'     and Cook's distance summary.}
#' }
#'
#' \strong{Confidence intervals} are reported throughout instead of standard
#' errors and t-values, using \code{confint()} for regression coefficients
#' and \code{corCI()} (Fisher z-transform) for correlations.
#'
#' \strong{Effect size labels} for correlations follow Cohen (1988):
#' \tabular{ll}{
#'   \code{negligible} \tab |r| < 0.10 \cr
#'   \code{small}      \tab 0.10 \eqn{\le} |r| < 0.30 \cr
#'   \code{moderate}   \tab 0.30 \eqn{\le} |r| < 0.50 \cr
#'   \code{large}      \tab |r| \eqn{\ge} 0.50 \cr
#' }
#'
#' \strong{Plot options via \code{which}:}
#' \describe{
#'   \item{\code{which = 1}}{Scatterplot with linear regression line and
#'     confidence band.}
#'   \item{\code{which = 2}}{Scatterplot with Loess smoother and confidence
#'     band (via \code{lines.loess()}).}
#'   \item{\code{which = 3}}{Residual plot: residuals vs. fitted values.}
#'   \item{\code{which = 4}}{Q-Q plot of residuals.}
#' }
#'
#' Default \code{which} by verbose level:
#' \itemize{
#'   \item \code{verbose = 1}: \code{which = 1}
#'   \item \code{verbose = 2}: \code{which = 1:2}
#'   \item \code{verbose = 3}: \code{which = 1:4}
#' }
#'
#' @return Both functions return \code{x} invisibly.
#'
#' @references
#'   Cohen, J. (1988). \emph{Statistical Power Analysis for the Behavioral
#'   Sciences} (2nd ed.). Lawrence Erlbaum Associates.
#'
#'   Breusch, T.S. and Pagan, A.R. (1979). A simple test for
#'   heteroscedasticity and random coefficient variation.
#'   \emph{Econometrica}, 47, 1287--1294.
#'
#' @seealso
#'   \code{\link{desc}} for the generic entry point,
#'   \code{\link{print.Desc.nq}} for numeric ~ categorical,
#'   \code{\link{print.Desc.qn}} for categorical ~ numeric,
#'   \code{\link{print.Desc.qq}} for categorical ~ categorical,
#'   \code{\link{corCI}}, \code{\link[lumen]{bpTest}},
#'   \code{\link[stats]{lm}}, \code{\link[stats]{cor.test}}
#'
#' @family desc
#' @concept bivariate numeric regression correlation scatterplot
#' @concept pearson spearman r-squared residuals heteroscedasticity
#'
#' @examples
#' # basic usage via desc()
#' desc(mpg ~ wt, mtcars)
#'
#' # more detail
#' desc(mpg ~ wt, mtcars, verbose = 3)
#'
#' # store result and plot separately
#' d <- desc(mpg ~ wt, mtcars, plotit = FALSE)
#' print(d, verbose = 1)
#' plot(d, which = 1:2)
#'
#' # pipe
#' desc(mpg ~ wt, mtcars) |> plot(which = 3)
#'
NULL


# ── Helpers ───────────────────────────────────────────────────────────────────

# Significance stars
.stars <- function(p) {
  if (is.na(p))   return("")
  if (p < 0.001)  return("***")
  if (p < 0.01)   return(" **")
  if (p < 0.05)   return("  *")
  if (p < 0.1)    return("  .")
  return("   ")
}

# Effect size label for |r| (Cohen 1988)
.rLabel <- function(r) {
  r <- abs(r)
  if (is.na(r))   return("")
  if (r >= 0.50)  return("large")
  if (r >= 0.30)  return("moderate")
  if (r >= 0.10)  return("small")
  return("negligible")
}


# ── Calc ──────────────────────────────────────────────────────────────────────

#' @keywords internal
.desc_nn <- function(x, y, conf.level = 0.95) {
  
  # ── 1. Basic counts ──────────────────────────────────────────────────────────
  nTotal  <- length(x)
  ok      <- complete.cases(x, y)
  nValid  <- sum(ok)
  nMiss   <- nTotal - nValid
  
  xOk <- x[ok]
  yOk <- y[ok]
  
  # ── 2. Pearson r ─────────────────────────────────────────────────────────────
  pearsonR  <- cor(xOk, yOk, method = "pearson")
  pearsonCI <- corCI(pearsonR, n = nValid, conf.level = conf.level)
  pearsonP  <- cor.test(xOk, yOk, method = "pearson")$p.value
  
  # ── 3. Spearman r ────────────────────────────────────────────────────────────
  spearmanR  <- cor(xOk, yOk, method = "spearman")
  spearmanCI <- corCI(spearmanR, n = nValid, conf.level = conf.level)
  spearmanP  <- cor.test(xOk, yOk, method = "spearman", exact = FALSE)$p.value
  
  # ── 4. Linear regression ─────────────────────────────────────────────────────
  lmFit  <- lm(yOk ~ xOk)
  lmSum  <- summary(lmFit)
  lmCI   <- confint(lmFit, level = conf.level)
  lmCoef <- coef(lmFit)
  lmP    <- pf(lmSum$fstatistic[1],
               lmSum$fstatistic[2],
               lmSum$fstatistic[3],
               lower.tail = FALSE)
  
  # ── 5. Residual diagnostics (verbose >= 2) ───────────────────────────────────
  residSe <- lmSum$sigma
  residDf <- lmFit$df.residual
  
  sw <- if (nValid >= 3L && nValid <= 5000L) {
    shapiro.test(residuals(lmFit))
  } else {
    NULL
  }
  
  # ── 6. Breusch-Pagan + Cook's distance (verbose >= 3) ───────────────────────
  bp      <- bpTest(lmFit)
  cook    <- cooks.distance(lmFit)
  cookMax <- max(cook, na.rm = TRUE)
  cookN4  <- sum(cook > 4 / nValid, na.rm = TRUE)   # common 4/n threshold
  
  # ── Assemble result ──────────────────────────────────────────────────────────
  structure(
    list(
      # meta
      nTotal     = nTotal,
      nValid     = nValid,
      nMiss      = nMiss,
      conf.level = conf.level,
      
      # correlation
      pearson  = list(r  = pearsonR,
                      ci = pearsonCI,
                      p  = pearsonP),
      spearman = list(r  = spearmanR,
                      ci = spearmanCI,
                      p  = spearmanP),
      
      # regression
      lm = list(
        intercept = list(est = unname(lmCoef[1]),
                         lci = lmCI[1, 1],
                         uci = lmCI[1, 2],
                         p   = lmSum$coefficients[1, 4]),
        slope     = list(est = unname(lmCoef[2]),
                         lci = lmCI[2, 1],
                         uci = lmCI[2, 2],
                         p   = lmSum$coefficients[2, 4]),
        r2        = lmSum$r.squared,
        r2Adj     = lmSum$adj.r.squared,
        p         = lmP,
        residSe   = residSe,
        residDf   = residDf
      ),
      
      # diagnostics
      shapiro = sw,
      bp      = bp,
      cookMax = cookMax,
      cookN4  = cookN4
    ),
    class = c("Desc.nn", "Desc")
  )
}



# ── Print ─────────────────────────────────────────────────────────────────────



#' @exportS3Method
#' @rdname desc.nn
print.Desc.nn <- function(x, verbose = NULL, abs.sty = NULL,
                          per.sty = NULL, ...) {

  .printHeader(x$meta)
  
  cat(x$pair$strOut)

  .printNN(x$res)

}




.printNN <- function(x, verbose = NULL, abs.sty = NULL,
                          per.sty = NULL, ...) {

  verbose <- verbose %||% getOption("DescTools.verbose", default = 2L)

  # ── local format helpers ─────────────────────────────────────────────────────
  fmtEst <- function(est, lci, uci, p)
    sprintf("%8.4f  (%8.4f, %8.4f)  %s", est, lci, uci, .stars(p))

  fmtR <- function(r, ci, p)
    sprintf("%6.3f  (%6.3f, %6.3f)  %s  %s",
            r, ci["lwr.ci"], ci["upr.ci"], .stars(p), .rLabel(r))

  cat("\n")

  # ── correlations ─────────────────────────────────────────────────────────────
  cat(sprintf("Pearson  r:  %s\n", fmtR(x$pearson$r,  x$pearson$ci,  x$pearson$p)))
  cat(sprintf("Spearman r:  %s\n", fmtR(x$spearman$r, x$spearman$ci, x$spearman$p)))
  cat("\n")

  # ── linear regression ────────────────────────────────────────────────────────
  cat("Linear regression:\n")
  cat(sprintf("  Intercept:  %s\n",
              fmtEst(x$lm$intercept$est,
                     x$lm$intercept$lci,
                     x$lm$intercept$uci,
                     x$lm$intercept$p)))
  cat(sprintf("  Slope:      %s\n",
              fmtEst(x$lm$slope$est,
                     x$lm$slope$lci,
                     x$lm$slope$uci,
                     x$lm$slope$p)))
  cat(sprintf("  R\u00b2: %.3f   adj. R\u00b2: %.3f   p: %s\n",
              x$lm$r2, x$lm$r2Adj,
              format.pval(x$lm$p, digits = 3, eps = 0.001)))

  # ── verbose >= 2: residual diagnostics ───────────────────────────────────────
  if (verbose >= 2L) {
    cat(sprintf("  Residual SE: %.4f on %d df\n",
                x$lm$residSe, x$lm$residDf))
    if (!is.null(x$shapiro)) {
      cat(sprintf("  Shapiro-Wilk on residuals: W = %.3f,  p = %s\n",
                  x$shapiro$statistic,
                  format.pval(x$shapiro$p.value, digits = 3, eps = 0.001)))
    } else {
      cat("  Shapiro-Wilk: n outside valid range (3-5000), skipped\n")
    }
  }

  # ── verbose >= 3: Breusch-Pagan + Cook ───────────────────────────────────────
  if (verbose >= 3L) {
    cat("\n")
    cat(sprintf("  Breusch-Pagan test: BP = %.4f,  df = %d,  p = %s\n",
                unname(x$bp$statistic),
                unname(x$bp$parameter),
                format.pval(x$bp$p.value, digits = 3, eps = 0.001)))
    cat(sprintf("  Cook's distance: max = %.4f,  n > 4/n threshold: %d\n",
                x$cookMax, x$cookN4))
  }

  cat("\n")
  invisible(x)
}




#' @exportS3Method
#' @rdname desc.nn
plot.Desc.nn <- function(x, which = 1, verbose = NULL, ...) {
  
  for(j in which){
    
    switch(as.character(j %||% "1"),
           "1" = {
             plot(x$data$y ~ x$data$x, ...,type="n")
             
             points(x=x$data$x, y=x$data$y, ...)
             
             lines(loess(y ~ x, x$data))
             abline(lm(y ~ x, x$data), lwd = 1.5, col = "darkgray")
             
           },
           "2" ={
             
             zz <- as.data.frame(x$data)
             zz <- zz[complete.cases(zz), ]
             
             plotDens2D(x=zz$x, y=zz$y, ...)
             
           },
           "3" ={
             plotBag(x=cbind(x$data$x, y=x$data$y), ...)
             
           },
           "4" ={
             plotHexbin(x=x$data$x, y=x$data$y, ...)
             
           },
    )
  }
  
}


