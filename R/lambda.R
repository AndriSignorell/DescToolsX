
#' Goodman Kruskal Lambda 
#' 
#' Calculate symmetric and asymmetric Goodman Kruskal lambda and their
#' confidence intervals. Lambda is a measure of proportional reduction in error
#' in cross tabulation analysis. For any sample with a nominal independent
#' variable and dependent variable (or ones that can be treated nominally), it
#' indicates the extent to which the modal categories and frequencies for each
#' value of the independent variable differ from the overall modal category and
#' frequency, i.e. for all values of the independent variable together 
#' 
#' Asymmetric lambda is interpreted as the probable improvement in predicting
#' the column variable Y given knowledge of the row variable X.\cr The
#' nondirectional lambda is the average of the two asymmetric lambdas,
#' lambda(C|R) and lambda(R|C). lambda (asymmetric and symmetric) has a scale
#' ranging from 0 to 1. 
#' 
#' @inheritParams Association
#' 
#' @param direction type of lambda. Can be one out of \code{"symmetric"}
#' (default), \code{"row"}, \code{"column"} (abbreviations are allowed).  If
#' direction is set to \code{"row"} then lambda(R|C) (column dependent) will be
#' reported. See Details.
#' 
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#'   
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See details in \code{\link{ConfidenceIntervals}}.
#'   
#' @param method character string selecting the interval method; currently
#' only \code{"classic"} is implemented. It is validated but has no
#' further effect while there is a single choice.
#' 
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Goodman--Kruskal lambda}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#' 
#' @note Based on code by Antti Arppe and Nanina Anderegg (confidence 
#' interval symmetric lambda), adapted to conform to package standards.
#' 
#' @seealso 
#' \code{\link{Association}}
#' @references Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley
#' & Sons
#' 
#' Goodman, L. A., Kruskal W. H. (1979) Measures of Association for Cross
#' Classifications. New York: Springer-Verlag (contains articles appearing in
#' \emph{J. Amer. Statist. Assoc.} in 1954, 1959, 1963, 1972).\cr
#' http://www.nssl.noaa.gov/users/brooks/public_html/feda/papers/goodmankruskal1.pdf
#' (might be outdated)
#' 
#' Liebetrau, A. M. (1983) \emph{Measures of Association}, Sage University
#' Papers Series on Quantitative Applications in the Social Sciences, 07-004.
#' Newbury Park, CA: Sage, pp. 17--24 
#' 
#' @examples
#' 
#' # example from Goodman Kruskal (1954)
#' m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))
#' dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
#' m
#' 
#' # direction default is "symmetric"
#' lambda(m)
#' lambda(m, conf.level=0.95)
#' 
#' lambda(m, direction="row")
#' lambda(m, direction="column")
#' 
#'
#' @family assoc.nominal
#' @concept association-measure
#' @concept nominal
#' @export
lambda <- function(x, y = NULL,
                   direction = c("symmetric", "row", "column"),
                   conf.level = NA,
                   sides = c("two.sided", "left", "right"),
                   method = c("classic"),
                   ...){

  # good description
  # http://salises.mona.uwi.edu/sa63c/Crosstabs%20Measures%20for%20Nominal%20Data.htm

  # All three matched up front. 'sides' used to be matched only inside the
  # interval branch, so a misspelled value was accepted whenever
  # conf.level was NA; 'method' was never matched at all.
  direction <- match.arg(direction)
  sides     <- match.arg(sides)
  method    <- match.arg(method)

  # Length and type BEFORE is.na(): NA is logical, and is.na() on a vector
  # of length != 1 makes the `if` below the error message instead of this
  # one. conf.level = NULL aborted the same way, and NaN slipped through
  # into the point estimate.
  conf.level <- .checkConfLevel(conf.level)
  
  # normalizeToConfusion() rather than table(): the family convention, and
  # it fixes two things at once - `...` used to reach table() only when y
  # was given, so useNA was silently dropped for a table input, and a
  # data.frame or a non-table x was never validated at all.
  x <- normalizeToConfusion(x, y, mode = "association", ...)
  
  # Guttman'a lambda (1941), resp. Goodman Kruskal's lambda (1954)
  
  n <- sum(x)
  csum <- colSums(x)
  rsum <- rowSums(x)
  rmax <- apply(x, 1, max)
  cmax <- apply(x, 2, max)
  max.rsum <- max(rsum)
  max.csum <- max(csum)
  
  nr <- nrow(x)
  nc <- ncol(x)
  
  switch( direction
          , "symmetric" = { res <- 0.5*(sum(rmax, cmax) - (max.csum +  max.rsum)) / (n - 0.5*(max.csum +  max.rsum)) }
          , "column" = { res <- (sum(rmax) - max.csum) / (n - max.csum) }
          , "row" = { res <- (sum(cmax) - max.rsum) / (n - max.rsum) }
  )
  
  if(!is.na(conf.level)) {
    
    # A one-sided bound at level gamma is the corresponding end of the
    # two-sided interval at level 2*gamma - 1. At or below 0.5 that level
    # is not positive: pr2 drops below 0.5, qnorm() turns negative and the
    # two bounds come out in reverse order - pmin/pmax clamps them
    # elementwise and does not notice. Same refusal as cramerV and
    # tukeyBiweight.
    if (sides != "two.sided" && conf.level <= 0.5)
      stop(gettextf(
        "a one-sided interval needs 'conf.level' above 0.5, not %g",
        conf.level), domain = NA)

    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    
    # Lengths were SWAPPED. L.col is indexed by row in the "column"
    # branch below (for(i in 1:nr)) but was allocated with nc elements,
    # and L.row the other way round. A matrix does not grow on
    # out-of-range assignment, so a non-square table with nr > nc aborted
    # with "subscript out of bounds" as soon as conf.level was supplied.
    # The documented example is 3x4 and takes direction = "symmetric",
    # which avoids both branches.
    L.col <- rep(NA_integer_, nr)
    L.row <- rep(NA_integer_, nc)
    
    switch( direction
            , "symmetric" = {
              
              #     How to see:
              #     http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
              #     pp. 1744
              #     Author:   Nina
              
              l <- which.max(csum)
              k <- which.max(rsum)
              li <- apply(x,1,which.max)
              ki <- apply(x,2,which.max)
              
              w <- 2*n-max.csum-max.rsum
              v <- 2*n -sum(rmax,cmax)
              xx <- sum(rmax[li==l], cmax[ki==k], rmax[k], cmax[l])
              # 'yy', not 'y': the latter masks the function's own y
              # argument, same class as the 't' fixed above
              yy <- 8*n-w-v-2*xx
              
              # 'isPeak', not 't': the latter masks base::t(), which is
              # the fifth instance of this in the suite
              isPeak <- logical(length(li))
              for (i in seq_along(li)){
                isPeak[i] <- (ki[li[i]]==i & li[ki[li[i]]]==li[i])
              }
              
              sigma2 <- 1/w^4*(w*v*yy-2 *w^2*(n - sum(rmax[isPeak]))-2*v^2*(n-x[k,l]))
              
            }
            , "column" = {
              L.col.max <- min(which(csum == max.csum))
              for(i in seq_len(nr)) {
                if(length(which(x[i, intersect(which(x[i,] == max.csum), 
                                               which(x[i,] == max.rsum))] == n))>0)
                  L.col[i] <- min(which(x[i, intersect(which(x[i,] == max.csum), 
                                                       which(x[i,] == max.rsum))] == n))
                else
                  if(x[i, L.col.max] == max.csum)
                    L.col[i] <- L.col.max
                  else
                    L.col[i] <- min(which(x[i,] == rmax[i]))
              }
              sigma2 <- (n-sum(rmax))*(sum(rmax) + max.csum -
                           2*(sum(rmax[which(L.col == L.col.max)])))/
                                (n-max.csum)^3
            }
            , "row" = {
              L.row.max <- min(which(rsum == max.rsum))
              for(i in seq_len(nc)) {
                # The condition tests COLUMN i (x[..., i]) while the
                # assignment below read ROW i (x[i, ...]) - a verbatim
                # copy of the "column" branch that was never transposed
                # with the rest. Both now work on column i, mirroring the
                # "column" branch exactly.
                if(length(which(x[intersect(which(x[,i] == max.rsum), 
                                            which(x[,i] == max.csum)),i] == n))>0)
                  L.row[i] <- min(which(x[intersect(which(x[,i] == max.rsum),
                                                    which(x[,i] == max.csum)), i] == n))
                else
                  if(x[L.row.max,i] == max.rsum)
                    L.row[i] <- L.row.max
                  else
                    L.row[i] <- min(which(x[,i] == cmax[i]))
              }
              sigma2 <- (n-sum(cmax))*(sum(cmax) + max.rsum -
                               2*(sum(cmax[which(L.row == L.row.max)])))/
                                  (n-max.rsum)^3
            }
    )
    
    
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + res

    # Lambda lies in [0, 1], so the open side of a one-sided interval
    # belongs at that boundary and not at +/-Inf (design_rules.md 4.1, as
    # decided for cohenKappa). .applySides() also does the clamping the
    # pmin/pmax above used to do - one implementation for the whole
    # family instead of a hand-written copy per function.
    res <- c(est = res, .applySides(ci, sides, lo = 0, hi = 1))

  }
  
  return(res)
}
