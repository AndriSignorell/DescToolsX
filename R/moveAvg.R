
#' Moving Average 
#' 
#' Compute a simple moving average (running mean). 
#' 
#' The implementation is using the function \code{filter} to calculate the
#' moving average.
#' 
#' @param x univariate time series.
#' @param order order of moving average.
#' @param align specifies whether result should be centered (default),
#' left-aligned or right-aligned.
#' @param endrule Character string indicating how the values at the beginning
#' and the end of the data should be treated.
#' \describe{
#'   \item{"keep"}{
#'     Keeps the first and last \eqn{k_2}{k2} values at both ends,
#'     where \eqn{k_2}{k2} is the half-bandwidth \code{k2 = k \%/\% 2},
#'     i.e., \code{y[j] = x[j]} for
#'     \eqn{j \in \{1,\ldots,k_2\}} and
#'     \eqn{j \in \{n-k_2+1,\ldots,n\}}.
#'   }
#'   \item{"constant"}{
#'     Fills the ends with the first and last calculated value in the output
#'     array, e.g., \code{out[1:k2] = out[k2+1]}.
#'   }
#'   \item{"NA"}{
#'     The default. Leaves the values as \code{NA},
#'     as returned by \code{\link{filter}}.
#'   }
#' }
#'  
#' @return Returns a vector of the same size and same class as x.
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @seealso There's a faster implementation of running mean in the package
#' \pkg{caTools} \code{\link[caTools]{runmean}()} and a slower one in
#' \pkg{forecast} \code{\link[forecast]{ma}()}. There's similar code in
#' \code{\link[DescToolsViz]{midx}()}. 
#' 
#' @keywords univar
#' @examples
#' 
#' moveAvg(AirPassengers, order=5)
#' 
 


#' @export
moveAvg <- function(x, order, align = c("center","left","right"),
                    endrule = c("NA", "keep", "constant")){
  
  n <- length(x)
  align   = match.arg(align)
  
  switch(align,
         "center" = {
           idx <- c(1:(order %/% 2), (n-order %/% 2+1):n)
           idx_const <- c(rep((order %/% 2)+1, order %/% 2),
                          rep(n-(order %/% 2), order %/% 2))
           
           if(order %% 2 == 1){   # order is odd
             z <- filter(x, rep(1/order, order), sides=2)
           } else {           # order is even
             z <- filter(x, c(1/(2*order), rep(1/order, order-1), 1/(2*order)), sides=2)
           }   }
         , "right" = {
           idx <- 1:(order-1)
           idx_const <- order
           z <- filter(x, rep(1/order, order), sides=1)
         }
         , "left" = {
           idx <- (n-order+2):n
           idx_const <- n-order+1
           z <- rev(filter(rev(x), rep(1/order, order), sides=1))
         }
  )
  
  endrule <- match.arg(endrule)
  switch(endrule,
         "NA" =     {},
         keep =     {z[idx] <- x[idx]},
         constant = {z[idx] <- z[idx_const]})
  
  if(!is.ts(x)) attr(z, "tsp") <- NULL
  class(z) <- class(x)
  return(z)
}
