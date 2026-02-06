
#' (Weighted) Sample Quantiles
#' 
#' Compute weighted quantiles (Eurostat definition).
#' 
#' The implementation strictly follows the Eurostat definition.
#' 
#' @param x a numeric vector.
#' @param weights an optional numeric vector giving the sample weights.
#' @param probs numeric vector of probabilities with values in \eqn{[0,1]}.
#' @param na.rm a logical indicating whether missing values in \code{x} should
#' be omitted.
#' @param names logical; if true, the result has a \code{\link{names}}
#' attribute.  Set to \code{FALSE} for speedup with many \code{probs}.
#' @param type an integer between 1 and 9 selecting one of the nine quantile
#' algorithms detailed below to be used. Currently only \code{types} \code{5}
#' and \code{7} (default) are implemented.
#' @param digits used only when \code{names} is true: the precision to use when
#' formatting the percentages. In \code{R} versions up to 4.0.x, this had been
#' set to \code{max(2, getOption("digits"))}, internally.
#' @return A named numeric vector containing the weighted quantiles of values
#' in \code{x} at probabilities \code{probs} is returned.
#' @author Andreas Alfons, Matthias Templ, some tweaks Andri Signorell
#' <andri@@signorell.net>
#' @seealso \code{\link{medianX}}, \code{\link[stats]{quantile}},
#' \code{\link{quantileCI}}
#' @references Working group on Statistics on Income and Living Conditions
#' (2004) Common cross-sectional EU indicators based on EU-SILC; the gender pay
#' gap.  \emph{EU-SILC 131-rev/04}, Eurostat.
#' @keywords univar
#' @examples
#' 
#' quantileX(d.pizza$temperature, rep(c(1:3), length.out=nrow(d.pizza)))

 
# further weighted quantiles in Hmisc and modi, both on CRAN

#' @export 
quantileX <- function(x, weights = NULL, probs = seq(0, 1, 0.25),
                     na.rm = FALSE, names=TRUE, type = 7, digits=7) {
  
  
  if(is.null(weights)){
    quantile(x=x, probs=probs, na.rm=na.rm, names=names, type=type, digits=digits)
    
  } else {
    
    # this is a not exported stats function
    format_perc <- function (x, digits = max(2L, getOption("digits")), probability = TRUE, 
                             use.fC = length(x) < 100, ...) {
      if (length(x)) {
        if (probability) 
          x <- 100 * x
        ans <- paste0(if (use.fC) 
          formatC(x, format = "fg", width = 1, digits = digits)
          else format(x, trim = TRUE, digits = digits, ...), "%")
        ans[is.na(x)] <- ""
        ans
      }
      else character(0)
    }
    
    
    
    sorted <- FALSE
    
    # initializations
    if (!is.numeric(x)) stop("'x' must be a numeric vector")
    
    n <- length(x)
    
    if (n == 0 || (!isTRUE(na.rm) && any(is.na(x)))) {
      # zero length or missing values
      return(rep.int(NA, length(probs)))
    }
    if (!is.null(weights)) {
      if (!is.numeric(weights)) stop("'weights' must be a numeric vector")
      else if (length(weights) != n) {
        stop("'weights' must have the same length as 'x'")
      } else if (!all(is.finite(weights))) stop("missing or infinite weights")
      if (any(weights < 0)) warning("negative weights")
      
      if (!is.numeric(probs) || all(is.na(probs)) ||
          isTRUE(any(probs < 0 | probs > 1))) {
        stop("'probs' must be a numeric vector with values in [0,1]")
        
      }
      
      if (all(weights == 0)) { # all zero weights
        warning("all weights equal to zero")
        return(rep.int(0, length(probs)))
      }
    }
    
    # remove NAs (if requested)
    if(isTRUE(na.rm)){
      indices <- !is.na(x)
      x <- x[indices]
      n <- length(x)
      if(!is.null(weights)) weights <- weights[indices]
    }
    # sort values and weights (if requested)
    if(!isTRUE(sorted)) {
      #        order <- order(x, na.last=NA)  ## too slow
      order <- order(x)
      x <- x[order]
      weights <- weights[order]  # also works if 'weights' is NULL
    }
    # some preparations
    if(is.null(weights)) rw <- (1:n)/n
    else rw <- cumsum(weights)/sum(weights)
    
    # obtain quantiles
    # currently only type 5
    if (type == 5) {
      qs <- sapply(probs,
                   function(p) {
                     if (p == 0) return(x[1])
                     else if (p == 1) return(x[n])
                     select <- min(which(rw >= p))
                     if(rw[select] == p) mean(x[select:(select+1)])
                     else x[select]
                   })
      
    } else if(type == 7){
      
      if(is.null(weights)){
        index <- 1 + max(n - 1, 0) * probs
        lo <- pmax(floor(index), 1)
        hi <- ceiling(index)
        x <- sort(x, partial = if (n == 0) 
          numeric()
          else unique(c(lo, hi)))
        qs <- x[lo]
        i <- which((index > lo & x[hi] != qs))
        h <- (index - lo)[i]
        qs[i] <- (1 - h) * qs[i] + h * x[hi[i]]
        
      } else {
        n     <- sum(weights)
        ord <- 1 + (n - 1) * probs
        low   <- pmax(floor(ord), 1)
        high  <- pmin(low + 1, n)
        ord <- ord %% 1
        ## Find low and high order statistics
        ## These are minimum values of x such that the cum. freqs >= c(low,high)
        allq <- approx(cumsum(weights), x, xout=c(low, high), 
                       method='constant', f=1, rule=2)$y
        k <- length(probs)
        qs <- (1 - ord)*allq[1:k] + ord*allq[-(1:k)]
      }
      
    } else {
      qs <- NA
      warning(gettextf("type %s is not implemented", type))
    }
    
    # return(unname(q))
    # why unname? change to named.. 14.10.2020
    
    if (names && length(probs) > 0L) {
      stopifnot(is.numeric(digits), digits >= 1)
      names(qs) <- format_perc(probs, digits = digits)
    }
    
    return(qs)
    
  }
}




