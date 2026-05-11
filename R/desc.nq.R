
#' @name desc.nq
#' @aliases .descNQ
#'
#' @title Describe Relationship: Numeric x by Categorical g
#'
#' @description
#' Computes descriptive statistics for a numeric variable \code{x}
#' grouped by a categorical variable \code{g}.
#'
#' @param x A numeric variable.
#' @param g A categorical grouping variable (factor or coercible to factor).
#' @param ... Further arguments (currently not used).
#' @param which integer vector selecting which plots to draw. See Details.
#'   \code{NULL} (default) selects plots automatically based on \code{verbose}.
#' @param digits integer. With how many digits should the relative frequencies
#' be formatted? Default can be set by
#' \code{\link{setDescToolsXOption}(digits=x)}.
#'
#' @details
#' The function summarizes the distribution of \code{x} across levels of
#' \code{g} and performs nonparametric tests of group differences.
#'
#' \strong{Computed statistics}
#' \itemize{
#'   \item Group-wise descriptive statistics (mean, median, SD, IQR, counts)
#'   \item Kruskal-Wallis test
#'   \item Effect size (\eqn{\eta^2}) based on the Kruskal-Wallis statistic
#'   \item Levene's test for homogeneity of variance
#' }
#'
#' \strong{Interpretation}
#' The Kruskal-Wallis test evaluates whether the distribution of \code{x}
#' differs between groups defined by \code{g}. The effect size \eqn{\eta^2}
#' provides a standardized measure of group differences.
#'
#' @return
#' An object of class \code{"Desc.nq"} inheriting from \code{"Desc"}.
#'
#' @section Output components:
#' \itemize{
#'   \item \code{tab}: group-wise summary table
#'   \item \code{test}: Kruskal-Wallis test
#'   \item \code{vtest}: Levene test
#'   \item \code{eta}: effect size
#' }
#'
#' @seealso
#' \code{\link{desc}}, \code{\link{desc.qn}}, \code{\link{desc.nn}},
#' \code{\link{kruskal.test}}, \code{\link[lumen]{leveneTest}}
#'
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
#' @concept hypothesis-testing
#'
#' @rdname desc.nq
#' @usage .descNQ(x, g, ...)
NULL


#' @keywords internal
.descNQ <- function(x, g, ... ) {

  kw <- kruskal.test(x~g)
  
  res <- list(
          tab = .buildSummaryTable(
                   tapply(x, g, desc, plotit=FALSE)   # groupwise numeric description
                  ),
          test  = kw,
          vtest = leveneTest(x~g),
          eta   = .eta2Kruskal(H = kw$statistic, 
                                k = length(unique(g)), 
                                n = length(x))
          
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
  
  if(x$pair$missing_groups > 0){
    warning(gettextf("  Grouping variable contains %s NAs (%s).", 
            x$pair$missing_groups, fm(x$pair$missing_groups_p, fmt="per.sty")), 
            call. = FALSE)
  }
  
}



#' @rdname desc.nq
#' @export
plot.Desc.nq <- function(x, which = NULL, ...){
  
  switch(as.character(which %||% "1"),
         "1" = {
           boxplot(x$data$y ~ x$data$x, 
                   xlab=x$meta$xname, ylab=x$meta$yname, ...)
           
           grid(nx=NA, ny=NULL)
           
           abline(h=mean(x$data$y, na.rm=TRUE), col="darkblue", lty="dashed", lwd=1)
           
           points(x=seq(ncol(x$res$tab)), 
                  y=tapply(x$data$y, x$data$x, mean, na.rm=TRUE),
                  pch=4, col="darkblue", cex=1.5)
           
           mtext(text=gettextf("n = %s", 
                               fm(tapply(x$data$y, x$data$x, 
                                      function(z) sum(!is.na(z))), fmt="abs.sty")), 
                 side = 3, at=seq(ncol(x$res$tab)), cex=0.8, line = 1)
           
         },
         "2" ={
           plotDens(x$data$y ~ x$data$x, ...)
         },
         
         "3" ={
           plotDensBox(x$data$y ~ x$data$x, ...)
           
         }
         
  )
  
}



# == internal helper functions ===============================================


.extractNqSummary <- function(x) {
  
  out <- c(
    mean   = x$mean,
    median = unname(x$quant["median"]),
    sd     = x$sd,
    IQR    = x$IQR,
    n      = x$n,
    np     = x$n /x$length,
    NAs    = x$NAs,
    zeros  = x$`0s`
  )
  
  return(out)
}


.buildSummaryTable <- function(x) {
  
  # dd = Liste von Resultaten (benannt!)
  
  mat <- sapply(x, .extractNqSummary)
  
  # calc percentages of valid cases
  mat[6,] <- mat[5,] / sum(mat[5,])
  
  # sicherstellen, dass Matrix
  mat <- as.matrix(mat)
  
  res <- rbind(
    fm(mat[c(1:4),], fmt=style("num.sty")),
    fm(mat[c(5),, drop=FALSE], fmt=style("abs.sty")),
    fm(mat[c(6),, drop=FALSE], fmt=style("per.sty")),
    mat[c(7:8),] <- fm(mat[c(7:8),], fmt=style("abs.sty"))
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

