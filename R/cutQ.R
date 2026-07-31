
#' Create a Factor Variable Using the Quantiles of a Continuous Variable
#' 
#' Creates a factor variable using the quantiles of a continuous variable.
#' 
#' 
#' This function uses \code{\link{quantile}} to obtain the specified quantiles
#' of \code{x}, then calls \code{\link{cut}} to create a factor variable using
#' the intervals specified by these quantiles.
#' 
#' It properly handles cases where more than one quantile obtains the same
#' value, as in the second example below.  Note that in this case, there will
#' be fewer generated factor levels than the specified number of quantile
#' intervals.
#' 
#' @param x continuous variable
#' @param breaks cut points used to create groups. By default, quartiles are
#' used. See \code{\link{quantile}} for details. A single integer specifies
#' the intended number of groups; for example, \code{breaks = 10} creates
#' deciles.
#' @param labels labels for the levels of the resulting category. By default,
#' labels are defined as \code{Q1}, \code{Q2}, and so on. The argument is
#' passed to \code{\link{cut}}, so \code{labels = FALSE} returns integer codes
#' instead of a factor. When quantiles are tied, the levels are built from
#' the observed interval bounds instead and \code{labels} is ignored - a
#' warning is issued in that case.
#' @param na.rm logical. Should missing values be removed before the
#' quantiles are computed? Defaults to \code{FALSE}. Note that the
#' quantiles themselves are always taken with \code{na.rm = TRUE}; this
#' argument decides whether the missing values are dropped from the
#' returned factor as well.
#' @param \dots optional arguments passed to \code{\link{cut}}
#' @return a factor with one level for each quantile interval, or an integer
#' vector when \code{labels = FALSE}
#' @note Based on code by Gregory R. Warnes, adapted to conform to package standards.
#' 
#' @seealso \code{\link{cut}}, \code{\link{quantile}}
#' 
#' @examples
#' 
#' # create example data
#' set.seed(1234)
#' x <- rnorm(1000)
#' 
#' # cut into quartiles
#' quartiles <- cutQ(x)
#' table(quartiles)
#' 
#' # cut into deciles
#' deciles <- cutQ(x, breaks=10, labels=NULL)
#' table(deciles)
#' 
#' # show handling of 'tied' quantiles.
#' x <- round(x)  # discretize to create ties
#' stem(x)        # display the ties
#' deciles <- cutQ(x, breaks=10)
#' 
#' table(deciles) # note that there are only 5 groups (not 10) 
#'                # due to duplicates
#' @family cut
#' @concept binning
#' @export
cutQ <- function(x, breaks=quantile(x, seq(0, 1, by=0.25), na.rm=TRUE), 
                 labels=NULL, na.rm = FALSE, ...){
  
  if(na.rm) x <- as.numeric(na.omit(x))
  
  if(length(breaks)==1 && isWholeLike(breaks)){
    if(breaks < 2)
      stop("'breaks' must specify at least 2 groups")
    breaks <- quantile(x, seq(0, 1, by = 1/breaks), na.rm = TRUE)
  }
  
  if(length(breaks) < 2L)
    stop("'breaks' must contain at least two cut points")
  
  userLabels <- !is.null(labels)
  # seq_len(): breaks of length 1 gave 1:0 and hence the labels Q1 and Q0
  if(!userLabels) labels <- gettextf("Q%s", seq_len(length(breaks)-1L))
  
  # probs <- quantile(x, probs)
  dups <- duplicated(breaks)
  if(any(dups)) {
    
    if(userLabels)
      warning("quantiles are tied; 'labels' is ignored and the levels are ",
              "built from the observed interval bounds")
    
    flag <- x %in% unique(breaks[dups])
    retval <- ifelse(flag, paste("[", as.character(x), "]", sep=''), NA)
    uniqs <- unique(breaks)
    
    # move cut points over a bit...
    reposition <- function(cut) {
      flag <- x>=cut
      if(sum(flag)==0)
        return(cut)
      else
        return(min(x[flag]))
    }
    
    # unique(): reposition() maps each cut point to the smallest observed
    # value at or above it, so two distinct quantiles can land on the same
    # observation - with 20 ones and 5 twos, the 0.8 and 0.9 quantiles
    # both repositioned to 2 and cut() aborted with "'breaks' are not
    # unique".
    newprobs <- unique(sapply(uniqs, reposition))

    if(length(newprobs) >= 2L){
      retval[!flag] <- as.character(cut(x[!flag], breaks=newprobs,
                                        include.lowest=TRUE, ...))
    } else {
      # nothing but a single cut point survived, so there is no interval
      # left to bin into and the remaining values are singletons too
      retval[!flag] <- paste("[", as.character(x[!flag]), "]", sep='')
    }
    
    # NA must not become a factor level: with na.rm = FALSE and missing
    # values in x, unique() carried an NA into levels()
    levs <- unique(retval[order(x)])        # ensure factor levels are
    levs <- levs[!is.na(levs)]              # properly ordered
    retval <- factor(retval, levels=levs)
    
    ## determine open/closed interval ends
    mkpairs <- function(x) # make table of lower, upper
      sapply(x,
             function(y) if(length(y)==2) y[c(2,2)] else y[2:3]
      )
    pairs <- mkpairs(strsplit(levs, '[^0-9+\\.\\-]+'))
    rownames(pairs) <- c("lower.bound","upper.bound")
    colnames(pairs) <- levs
    
    closed.lower <- rep(FALSE, ncol(pairs)) # default lower is open
    closed.upper <- rep(TRUE, ncol(pairs))  # default upper is closed
    closed.lower[1] <- TRUE                 # lowest interval is always closed
    
    # seq_len()/seq_along style: with a single remaining level 2:1 and
    # 1:0 both ran backwards and indexed past the end of 'pairs'
    for(i in seq_len(ncol(pairs))[-1L])      # open lower interval if above singlet
      if(pairs[1,i]==pairs[1,i-1] && pairs[1,i]==pairs[2,i-1])
        closed.lower[i] <- FALSE
    
    for(i in seq_len(ncol(pairs)-1L))        # open upper interval if below singlet
      if(pairs[2,i]==pairs[1,i+1] && pairs[2,i]==pairs[2,i+1])
        closed.upper[i] <- FALSE
    
    levs <- ifelse(pairs[1,]==pairs[2,],
                   pairs[1,],
                   paste(ifelse(closed.lower,"[","("),
                         pairs[1,],
                         ",",
                         pairs[2,],
                         ifelse(closed.upper,"]",")"),
                         sep='')
    )
    levels(retval) <- levs
    
  } else
    retval <- cut( x, breaks, include.lowest=TRUE,  labels=labels, ... )
  
  return(retval)
  
}
