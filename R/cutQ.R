
#' Create a Factor Variable Using the Quantiles of a Continuous Variable
#' 
#' Create a factor variable using the quantiles of a continous variable.
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
#' @param x continous variable.
#' @param breaks the breaks for creating groups. By default the quartiles will
#' be used, say \code{quantile} \code{seq(0, 1, by = 0.25)} quantiles. See
#' \code{\link{quantile}} for details. If breaks is given as a single integer
#' it is interpreted as the intended number of groups, e.g. \code{breaks=10}
#' will return x cut in deciles.
#' @param labels labels for the levels of the resulting category. By default,
#' labels are defined as \code{Q1}, \code{Q2} to the length of breaks - 1. The
#' parameter ist passed to \code{\link{cut}}, so if \code{labels} are set to
#' \code{FALSE}, simple integer codes are returned instead of a factor. %%
#' ~~Describe \code{labels} here~~
#' @param na.rm Boolean indicating whether missing values should be removed
#' when computing quantiles.  Defaults to TRUE.
#' @param \dots Optional arguments passed to \code{\link{cut}}.
#' @return Factor variable with one level for each quantile interval given by
#' \code{q}.
#' @author Gregory R. Warnes <greg@@warnes.net>, some slight modifications
#' Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{cut}}, \code{\link{quantile}}
#' @keywords manip univar
#' @examples
#' 
#' # create example data
#' \testonly{set.seed(1234)}
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


#' @export
cutQ <- function(x, breaks=quantile(x, seq(0, 1, by=0.25), na.rm=TRUE), 
                 labels=NULL, na.rm = FALSE, ...){
  
  # old version:
  #  cut(x, breaks=probsile(x, breaks=probs, na.rm = na.rm), include.lowest=TRUE, labels=labels)
  
  # $Id: probscut.R 1431 2010-04-28 17:23:08Z ggrothendieck2 $
  # from gtools
  
  if(na.rm) x <- na.omit(x)
  
  if(length(breaks)==1 && isWhole(breaks))
    breaks <- quantile(x, seq(0, 1, by = 1/breaks), na.rm = TRUE)
  
  if(is.null(labels)) labels <- gettextf("Q%s", 1:(length(breaks)-1))
  
  # probs <- quantile(x, probs)
  dups <- duplicated(breaks)
  if(any(dups)) {
    
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
    
    newprobs <- sapply(uniqs, reposition)
    retval[!flag] <- as.character(cut(x[!flag], breaks=newprobs, include.lowest=TRUE,...))
    
    levs <- unique(retval[order(x)])        # ensure factor levels are
    # properly ordered
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
    
    for(i in 2:ncol(pairs))                 # open lower interval if above singlet
      if(pairs[1,i]==pairs[1,i-1] && pairs[1,i]==pairs[2,i-1])
        closed.lower[i] <- FALSE
    
    for(i in 1:(ncol(pairs)-1))             # open upper interval if below singlet
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


