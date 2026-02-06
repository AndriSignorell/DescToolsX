
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
#' @param direction type of lambda. Can be one out of \code{"symmetric"}
#' (default), \code{"row"}, \code{"column"} (abbreviations are allowed).  If
#' direction is set to \code{"row"} then lambda(R|C) (column dependent) will be
#' reported. See details. 
#' @param method a character string, defining the type of intervals required.
#' The value should be one out of \code{"classic"}, \code{"boot"} (default).
#' 
#' @return if no confidence intervals are requested: the estimate as numeric
#' value\cr\cr else a named numeric vector with 3 elements
#' \item{est}{estimate} \item{lci}{lower confidence interval}
#' \item{uci}{upper confidence interval}
#' 
#' @author Andri Signorell <andri@@signorell.net> based on code from Antti
#' Arppe <antti.arppe@@helsinki.fi>, \cr Nanina Anderegg (confidence interval
#' symmetric lambda)
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
#' @keywords univar
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


#' @export
lambda <- function(x, y = NULL,  
                   conf.level = NA, 
                   sides=c("two.sided", "left", "right"), 
                   method = c("classic"), 
                   direction = c("symmetric", "row", "column"), ...){
  
  # good description
  # http://salises.mona.uwi.edu/sa63c/Crosstabs%20Measures%20for%20Nominal%20Data.htm

  if(!is.null(y)) x <- table(x, y, ...)
  
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
  
  switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
          , "symmetric" = { res <- 0.5*(sum(rmax, cmax) - (max.csum +  max.rsum)) / (n - 0.5*(max.csum +  max.rsum)) }
          , "column" = { res <- (sum(rmax) - max.csum) / (n - max.csum) }
          , "row" = { res <- (sum(cmax) - max.rsum) / (n - max.rsum) }
  )
  
  if(is.na(conf.level)){
    res <- res
  } else {
    
    sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                       several.ok = FALSE)
    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    
    L.col <- matrix(,nc)
    L.row <- matrix(,nr)
    
    switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
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
              y <- 8*n-w-v-2*xx
              
              t <- rep(NA, length(li))
              for (i in 1:length(li)){
                t[i] <- (ki[li[i]]==i & li[ki[li[i]]]==li[i])
              }
              
              sigma2 <- 1/w^4*(w*v*y-2 *w^2*(n - sum(rmax[t]))-2*v^2*(n-x[k,l]))
              
            }
            , "column" = {
              L.col.max <- min(which(csum == max.csum))
              for(i in 1:nr) {
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
              for(i in 1:nc) {
                if(length(which(x[intersect(which(x[,i] == max.rsum), 
                                            which(x[,i] == max.csum)),i] == n))>0)
                  L.row[i] <- min(which(x[i,intersect(which(x[i,] == max.csum), 
                                                      which(x[i,] == max.rsum))] == n))
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
    ci <- pmin(1, pmax(0, qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + res))
    res <- c(est = res,  lci=ci[1], uci=ci[2])
    
    if(sides=="left")
      res[3] <- Inf
    else if(sides=="right")
      res[2] <- -Inf

  }
  
  return(res)
}

