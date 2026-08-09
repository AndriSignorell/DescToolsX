
#' Goodman Kruskal's Tau
#' 
#' Calculate Goodman-Kruskal's tau, a measure of association for nominal
#' variables in a two-way table. The function accepts either a contingency
#' table or two vectors.
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
#' @param x numeric vector or contingency table. A matrix is treated as a table.
#' @param y \code{NULL} (default) or a vector with compatible dimensions to
#' \code{x}. If supplied, \code{table(x, y, \dots)} is calculated.
#' @param direction direction of the calculation. Must be \code{"row"}
#' (default) or \code{"column"}. \code{"row"} gives tau (R|C), i.e. the row
#' variable is the dependent one and is predicted from the column variable;
#' \code{"column"} gives tau (C|R).
#' 
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See \code{\link{ConfidenceIntervals}}.
#'
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing i.e. to set useNA. This refers only to the
#' vector interface. 
#' 
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Goodman-Kruskal's tau}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#' 
#' @note Based on code by Antti Arppe, adapted to conform to package standards.
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
#' @seealso \code{\link{lambda}}, \code{\link{cramerV}},
#' \code{\link{Association}}
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
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @export
gkTau <- function(x, y = NULL, 
                  conf.level = NA,
                  sides = c("two.sided", "left", "right"), 
                  direction = c("row", "column"),
                  ...){

  # matched up front, not inside switch(): a misspelled 'direction' used
  # to be caught only where the branch was taken, and 'sides' did not
  # exist at all
  direction <- match.arg(direction)
  sides     <- match.arg(sides)

  conf.level <- checkConfLevel(conf.level)

  # A one-sided bound at level gamma is the corresponding end of the
  # two-sided interval at level 2*gamma - 1. At or below 0.5 that level is
  # not positive and the normal quantile turns negative, which hands back
  # the two bounds in reverse order.
  if(sides != "two.sided" && !is.na(conf.level) && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)

  confAdj <- if(sides == "two.sided") conf.level else 2 * conf.level - 1

  if(!is.null(y)) x <- table(x, y, ...)
  
  x <- as.matrix(x)
  
  n <- sum(x)
  n.err.unconditional <- n^2
  sum.row <- rowSums(x)
  sum.col <- colSums(x)
  
  switch( direction
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
    pr2 <- 1 - (1 - confAdj)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + est
    # tau is a proportional-reduction-in-error measure and lives in
    # [0, 1] - .applySides() clamps to that range and closes the open
    # side of a one-sided interval there instead of at an infinity tau
    # cannot reach
    res <- c(est = est, .applySides(ci, sides, lo = 0, hi = 1))
  }
  
  return(res)
}
