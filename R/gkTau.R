
#' Goodman Kruskal's Tau
#' 
#' Calculate Goodman Kruskal's tau statistic, a measure of association for
#' ordinal factors in a two-way table.\cr The function has interfaces for a
#' table (matrix) and for single vectors. 
#' 
#' Goodman-Kruskal tau measures association for cross tabulations of nominal
#' level variables. Goodman-Kruskal tau is based on random category assignment.
#' It measures the percentage improvement in predictability of the dependent
#' variable (column or row variable) given the value of other variables (row or
#' column variables). Goodman-Kruskal tau is the same as Goodman-Kruskal lambda
#' except the calculations of the tau statistic are based on assignment
#' probabilities specified by marginal or conditional proportions.
#' Misclassification probabilities are based on random category assignment with
#' probabilities specified by marginal or conditional proportion.
#' 
#' Goodman Kruskal tau reduces to \eqn{\phi^2} (see: \code{\link{phi}}) in the
#' 2x2-table case.\cr
#' 
#' @name gkTau
#' @param x a numeric vector or a table. A matrix will be treated as table. 
#' @param y NULL (default) or a vector with compatible dimensions to \code{x}.
#' If y is provided, \code{table(x, y, \dots)} is calculated. 
#' @param direction direction of the calculation. Can be \code{"row"} (default)
#' or \code{"column"}, where \code{"row"} calculates Goodman Kruskal's tau-a
#' (R|C) ("column dependent"). 
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence interval will be calculated. 
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing i.e. to set useNA. This refers only to the
#' vector interface. 
#' 
#' @return a single numeric value if no confidence intervals are requested,\cr
#' and otherwise a numeric vector with 3 elements for the estimate, the lower
#' and the upper confidence interval
#' 
#' @note
#' Based on code by Antti Arppe.
#' 
#' @references Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley
#' & Sons, pp. 57-59.
#' 
#' Goodman, L. A., & Kruskal, W. H. (1954) Measures of association for cross
#' classifications. \emph{Journal of the American Statistical Association}, 49,
#' 732-764.
#' 
#' Somers, R. H. (1962) A New Asymmetric Measure of Association for Ordinal
#' Variables, \emph{American Sociological Review}, 27, 799-811.
#' 
#' Goodman, L. A., & Kruskal, W. H. (1963) Measures of association for cross
#' classifications III: Approximate sampling theory. \emph{Journal of the
#' American Statistical Association}, 58, 310-364.
#' 
#' Liebetrau, A. M. (1983) \emph{Measures of Association}, Sage University
#' Papers Series on Quantitative Applications in the Social Sciences, 07-004.
#' Newbury Park, CA: Sage, pp. 24--30
#' 
#' @seealso There's another implementation of gamma in \pkg{vcdExtra}
#' \code{\link[vcdExtra]{GKgamma}} \code{\link{Association}}
#' 
#' @examples
#' # example in:
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. S. 1821
#' 
#' tab <- as.table(rbind(c(26,26,23,18,9),c(6,7,9,14,23)))
#' 
#' # Goodman Kruskal's tau C|R
#' gkTau(tab, direction="column", conf.level=0.95)
#' # Goodman Kruskal's tau R|C
#' gkTau(tab, direction="row", conf.level=0.95)
#' 
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # pp. 1814 (143)
#' tab <- as.table(cbind(c(11,2),c(4,6)))
#' 
#' gkTau(tab, direction="row", conf.level=0.95)
#' gkTau(tab, direction="column", conf.level=0.95)
#' # reduce both to:
#' phi(tab)^2
#' 
#' 
#' # example 1 in Liebetrau (1983)
#' 
#' tt <- matrix(c(549,93,233,119,225,455,402,  
#'                212,124,78,42,41,12,132,
#'                54,54,33,13,46,7,153), ncol=3,
#'              dimnames=list(rownames=c("Gov", "Mil", "Edu", "Eco", "Intel", "Rel", "For"), 
#'                            colnames=c("One", "Two", "Multi")))
#' 
#' gkTau(tt, direction = "row", conf.level = 0.95)
#' gkTau(tt, direction = "column", conf.level = 0.95)
#' 
#' 
#' # SPSS
#' ttt <- matrix(c(225,53,206,3,1,12), nrow=3,
#'               dimnames=list(rownames=c("right","center", "left"), 
#'                             colnames=c("us","ussr")))
#' 
#' round(gkTau(ttt, direction = "r", con=0.95), d=3)
#' round(gkTau(ttt, direction = "c"), d=3)
#' 

#' @rdname gkTau

#' @family assoc.ordinal  
#' @concept association-measure  
#' @concept ordinal
#'
#'
#' @export
gkTau <- function(x, y = NULL, direction = c("row", "column"), conf.level = NA, ...){
  
  if(!is.null(y)) x <- table(x, y, ...)
  
  n <- sum(x)
  n.err.unconditional <- n^2
  sum.row <- rowSums(x)
  sum.col <- colSums(x)
  
  switch( match.arg( arg = direction, choices = c("row", "column") )
          , "column" = {             # Tau Column|Row
            
            for(i in 1:nrow(x))
              n.err.unconditional <- n.err.unconditional-n*sum(x[i,]^2/sum.row[i])
            n.err.conditional <- n^2-sum(sum.col^2)
            tau.CR <- 1-(n.err.unconditional/n.err.conditional)
            v <- n.err.unconditional/(n^2)
            d <- n.err.conditional/(n^2)
            f <- d*(v+1)-2*v
            var.tau.CR <- 0
            for(i in 1:nrow(x))
              for(j in 1:ncol(x))
                var.tau.CR <- var.tau.CR + x[i,j]*(-2*v*(sum.col[j]/n)+d*((2*x[i,j]/sum.row[i])-sum((x[i,]/sum.row[i])^2))-f)^2/(n^2*d^4)
            ASE.tau.CR <- sqrt(var.tau.CR)
            est <- tau.CR
            sigma2 <- ASE.tau.CR^2
          }
          , "row" = {             # Tau Row|Column
            
            for(j in 1:ncol(x))
              n.err.unconditional <- n.err.unconditional-n*sum(x[,j]^2/sum.col[j])
            n.err.conditional <- n^2-sum(sum.row^2)
            tau.RC <- 1-(n.err.unconditional/n.err.conditional)
            v <- n.err.unconditional/(n^2)
            d <- n.err.conditional/(n^2)
            f <- d*(v+1)-2*v
            var.tau.RC <- 0
            for(i in 1:nrow(x))
              for(j in 1:ncol(x))
                var.tau.RC <- var.tau.RC + x[i,j]*(-2*v*(sum.row[i]/n)+d*((2*x[i,j]/sum.col[j])-sum((x[,j]/sum.col[j])^2))-f)^2/(n^2*d^4)
            ASE.tau.RC <- sqrt(var.tau.RC)
            est <- tau.RC
            sigma2 <- ASE.tau.RC^2
          }
  )
  
  if(is.na(conf.level)){
    res <- est
  } else {
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + est
    res <- c(est=est, lci=ci[1], uci=ci[2])
  }
  
  return(res)
}
