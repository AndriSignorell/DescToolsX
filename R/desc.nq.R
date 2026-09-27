
#' @name desc.nq
#' @aliases .descNQ
#'
#' @title Describe Relationship: Numeric x by Categorical g
#'
#' @description
#' Computes, prints and plots descriptive statistics for a numeric variable
#' `x` grouped by a categorical variable `g`. The function is dispatched
#' automatically by `desc(y ~ g, data)` when `y` is numeric and `g`
#' categorical.
#'
#' @param x a numeric variable for `.descNQ()`, or an object of class
#'   `"Desc.nq"` for the print and plot methods
#' @param g a categorical grouping variable (factor or coercible to factor)
#' @param which integer vector selecting the plots to draw, one plot per
#'   element, see section **Plots**. Default `1`.
#' @param digits currently unused
#' @param \dots further arguments. In `.descNQ()` unused, in `print()`
#'   passed to [bedrock::printCharMatrix()]. In `plot()` they are passed on
#'   to the plot function selected by `which` (see section **Plots**, where
#'   each function is linked) and go unchanged to *every* selected plot.
#'
#' @details
#' The function summarizes the distribution of `x` across levels of
#' `g` and performs nonparametric tests of group differences.
#'
#' **Computed statistics**
#' \itemize{
#'   \item Group-wise descriptive statistics (mean, median, SD, IQR, counts)
#'   \item Kruskal-Wallis test
#'   \item Effect size (\eqn{\eta^2}) based on the Kruskal-Wallis statistic
#'   \item Levene's test for homogeneity of variance
#' }
#'
#' **Interpretation**
#' The Kruskal-Wallis test evaluates whether the distribution of `x`
#' differs between groups defined by `g`. The effect size \eqn{\eta^2}
#' provides a standardized measure of group differences.
#'
#' @section Plots:
#' `plot()` draws one of three displays of `x` by group, selected by
#' `which`. All arguments in `...` go straight to the underlying
#' function; its help page lists what can be set.
#'
#' \describe{
#'   \item{`which = 1`}{Boxplots by group, drawn by [pharos::plotBox()].
#'     Axes are labelled with the variable names.}
#'   \item{`which = 2`}{Overlaid kernel density estimates, one per group,
#'     drawn by [pharos::plotDens()].}
#'   \item{`which = 3`}{Density and boxplot combined per group, drawn by
#'     [pharos::plotDensBox()].}
#' }
#'
#' `main` defaults to the title stored in the object and is passed to
#' every plot.
#'
#' @return `.descNQ()` returns an object of class `c("Desc.nq", "Desc")`
#' with components:
#' \describe{
#'   \item{`tab`}{group-wise summary table}
#'   \item{`test`}{result of the Kruskal-Wallis test}
#'   \item{`vtest`}{result of Levene's test}
#'   \item{`eta`}{effect size}
#' }
#' The plot method returns `x` invisibly.
#'
#' @seealso
#' [desc()], [desc.qn()], [desc.nn()], [desc.qq()],
#' [kruskal.test()], [lumen::leveneTest()]
#'
#' Plot functions: [pharos::plotBox()], [pharos::plotDens()],
#' [pharos::plotDensBox()]
#'
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept hypothesis-testing
#' @concept boxplot density group comparison
#'
#' @examples
#' # basic usage via desc()
#' desc(temperature ~ area, Pizza)
#'
#' # store result, print and plot separately
#' d <- desc(temperature ~ area, Pizza, plotit = FALSE)
#' d
#'
#' # the three plots
#' plot(d, which = 1)                     # boxplots          -> plotBox()
#' plot(d, which = 2)                     # densities         -> plotDens()
#' plot(d, which = 3)                     # density + boxplot -> plotDensBox()
#'
#' # pipe
#' desc(temperature ~ area, Pizza) |> plot(which = 2)
#'
#' @rdname desc.nq
#' @usage .descNQ(x, g, ...)
NULL


.descNQ <- function(x, g, ... ) {

  g <- droplevels(factor(g))
  kw <- kruskal.test(x~g)

  # eta squared needs the n and k the test was computed on: kruskal.test()
  # drops incomplete pairs, length(x) and unique(g) did not (NA counted
  # as a group of its own)
  ok <- complete.cases(x, g)
  
  res <- list(
          tab = .buildSummaryTable(
                   tapply(x, g, desc, plotit=FALSE)   # groupwise numeric description
                  ),
          test  = kw,
          vtest = leveneTest(x~g),
          eta   = .eta2Kruskal(H = kw$statistic, 
                                k = nlevels(droplevels(g[ok])), 
                                n = sum(ok))
        )
          
}  




#' @rdname desc.nq
#' @export
print.Desc.nq <- function(x, digits = NULL, ...) {

  .printHeader(x$meta)
  
  cat(x$pair$strOut)
  printCharMatrix(x$res$tab, sep = 3, ...)
  
  out <- strTrim(capture.output(x$res$test)[c(2,5)])
  cat(gettextf("\n%s:\n  %s\n", out[1], out[2]))
  cat(gettextf("  \u03b7\u00b2 = %.3f (%s)\n\n", x$res$eta, attr(x$res$eta, "label")))
  
  out <- strTrim(capture.output(x$res$vtest)[c(2,5)])
  cat(gettextf("%s:\n  %s\n\n", out[1], out[2]))
  
  if (x$pair$nMissingGroups > 0)
    .printWarning(gettextf("Grouping variable contains %s NAs (%s).",
                           x$pair$nMissingGroups,
                           fm(x$pair$pctMissingGroups, fmt = "per.sty")))
  
  .plotIfRequested(x)
}



#' @param main main title for the plot; defaults to the title stored in
#' `x$meta$main`
#' @rdname desc.nq
#' @export
plot.Desc.nq <- function(x, main = x$meta$main, which = 1, ...) {

  # local names for the formulas: `x$data$y ~ x$data$x` is evaluated in
  # functions whose first argument is called x as well (see plot.Desc.nn)
  response <- x$data$y
  group    <- x$data$x

  # loop instead of a bare switch(): switch() takes a single value, so
  # which = 1:3 failed with "EXPR must be a length 1 vector"
  for (j in which) {

    switch(as.character(j),
           "1" = {
             plotBox(response, g = group,
                     main = main,
                     xlab = x$meta$xname,
                     ylab = x$meta$yname, ...)
           },
           "2" = {
             plotDens(response ~ group, main = main, ...)
           },
           "3" = {
             plotDensBox(response ~ group, main = main, ...)
           },
           warning(gettextf("No plot defined for which = %s (valid: 1-3).", j))
    )
  }

  invisible(x)
}


# == internal helper functions ===============================================


.extractNqSummary <- function(x) {
  
  if (inherits(x, "Desc.AllNA"))
    return(c(mean = NA_real_, median = NA_real_, sd = NA_real_,
             iqr  = NA_real_, n = 0L, np = NA_real_,
             NAs  = x$NAs,   zeros = 0L))
  
  c(
    mean   = x$mean,
    median = unname(x$quant["median"]),
    sd     = x$sd,
    iqr    = x$iqr,
    n      = x$n,
    np     = x$n / x$length,
    NAs    = x$NAs,
    zeros  = x$`0s`
  )
}


.buildSummaryTable <- function(x) {
  
  # x = Liste von Desc-Resultaten (benannt!)
  
  mat <- sapply(x, .extractNqSummary)
  
  # calc percentages of valid cases
  mat[6,] <- mat[5,] / sum(mat[5,], na.rm = TRUE)
  
  # sicherstellen, dass Matrix
  mat <- as.matrix(mat)

  res <- rbind(
    fm(mat[c(1:4),  , drop = FALSE], fmt = style("num.sty")),
    fm(mat[c(5),    , drop = FALSE], fmt = style("abs.sty")),
    fm(mat[c(6),    , drop = FALSE], fmt = style("per.sty")),
    fm(mat[c(7:8),  , drop = FALSE], fmt = style("abs.sty"))
  )
  
  return(res)
  
}



# Eta² aus Kruskal-Wallis (Tomczak & Tomczak 2014)
# H = Kruskal-Wallis Statistik, k = Anzahl Gruppen, n = Gesamtn
.eta2Kruskal <- function(H, k, n) {
  eta2 <- (H - k + 1) / (n - k)
  eta2 <- max(0, eta2)   # kann bei kleinen n leicht negativ werden
  
  label <- cut(eta2,
               breaks = c(-Inf, 0.01, 0.06, 0.14, Inf),
               labels = c("negligible", "small", "moderate", "large"),
               right  = FALSE)
  
  structure(eta2, label = as.character(label))
}
