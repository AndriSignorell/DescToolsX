
#' Uncertainty Coefficient 
#' 
#' Computes directional or symmetric uncertainty coefficients. The
#' directional coefficient U(C|R) measures the proportion of uncertainty
#' (entropy) in the column variable Y that is explained by the row variable X.
#' The function has interfaces for a table, a matrix, a data frame, and single
#' vectors.
#' 
#' The uncertainty coefficient is computed as \deqn{U(C|R) = \frac{H(X) + H(Y)
#' - H(XY)}{H(Y)} } and ranges from \verb{[0, 1]}.  
#' 
#' @inheritParams ConfidenceIntervals
#' @param x a numeric vector, factor, matrix, or data frame
#' @param y \code{NULL} (default) or a vector, an ordered factor, matrix or
#' data frame with compatible dimensions to \code{x}
#' @param direction direction of calculation, one of \code{"symmetric"}
#' (default), \code{"row"}, or \code{"column"}. The row direction calculates
#' U(R|C), and the column direction calculates U(C|R).
#' @param pZeroCorrection small positive value used to replace zero cells
#' before taking logarithms
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing, for example, \code{useNA} to be set. This
#' refers only to the
#' vector interface. 
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{uncertainty coefficient estimate}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#' 
#' @note Based on code from Antti Arppe
#' 
#' 
#' @seealso \code{\link{Association}}
#' @references Theil, H. (1972), \emph{Statistical Decomposition Analysis},
#' Amsterdam: North-Holland Publishing Company. 
#' 
#' @examples
#' 
#' # example from Goodman Kruskal (1954)
#' 
#' m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))
#' dimnames(m) <- list(paste("A", 1:3), paste("B", 1:4))
#' m
#' 
#' # direction default is "symmetric"
#' uncertCoef(m)
#' uncertCoef(m, conf.level=0.95)
#' 
#' uncertCoef(m, direction="row")
#' uncertCoef(m, direction="column")
#'
#' @family assoc.nominal  
#' @concept association-measure  
#' @concept nominal  
#' @concept information-theory
#'
#'
#' @export
uncertCoef <- function(x, y = NULL, conf.level = NA, 
                       sides = c("two.sided", "left", "right"),
                       direction = c("symmetric", "row", "column"),
                       pZeroCorrection = 1/sum(x)^2, ... ) {
  
  # Theil's UC (1970)
  # slightly nudge zero values so that their logarithm can be 
  # calculated (cf. Theil 1970: x->0 => xlogx->0)
  
  if(!is.null(y)) x <- table(x, y, ...)
  
  x[x == 0] <- pZeroCorrection
  
  n <- sum(x)
  rsum <- rowSums(x)
  csum <- colSums(x)
  
  hx <- -sum((apply(x, 1, sum) * log(apply(x, 1, sum)/n))/n)
  hy <- -sum((apply(x, 2, sum) * log(apply(x, 2, sum)/n))/n)
  hxy <- -sum(apply(x, c(1, 2), sum) * log(apply(x, c(1, 2), sum)/n)/n)
  
  switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
          , "symmetric" = { res <- 2 * (hx + hy - hxy)/(hx + hy) }
          , "row" = { res <- (hx + hy - hxy)/hx }
          , "column" = { res <- (hx + hy - hxy)/hy }
  )
  
  if(!is.na(conf.level)){
    var.uc.RC <- var.uc.CR <- 0
    for(i in 1:nrow(x))
      for(j in 1:ncol(x))
      { var.uc.RC <- var.uc.RC + 
                       x[i,j]*(hx*log(x[i,j]/csum[j]) + 
                       ((hy-hxy)*log(rsum[i]/n)))^2/(n^2*hx^4);
      var.uc.CR <- var.uc.CR + 
                       x[i,j] * (hy*log(x[i,j]/rsum[i]) + 
                       ((hx-hxy)*log(csum[j]/n)))^2/(n^2*hy^4);
      }
    switch( match.arg( arg = direction, choices = c("symmetric", "row", "column") )
            , "symmetric" = {
              sigma2 <- 4*sum(x * (hxy * log(rsum %o% csum/n^2) - 
                                     (hx+hy)*log(x/n))^2 ) /
                                     (n^2*(hx+hy)^4)
            }
            , "row" = { sigma2 <- var.uc.RC }
            , "column" = { sigma2 <- var.uc.CR }
    )
    
    pr2 <- 1 - (1 - conf.level)/2
    ci <- qnorm(pr2) * sqrt(sigma2) * c(-1, 1) + res
    
    res <- c(est = res, lci = max(ci[1], -1), uci = min(ci[2], 1))
  }
  return(res)
}
