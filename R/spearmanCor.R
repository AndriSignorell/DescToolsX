
#' Spearman Rank Correlation 
#' 
#' Calculate Spearman correlation coefficient and its confidence interval. In
#' addition to the base R function \code{\link{cor}()}, frequency tables are
#' also accepted as arguments (i.e. actually weights are used).
#' 
#' The function calculates Spearman's rho statistic by means of \code{cor(...,
#' method="spearman")} when two variables \code{x} and \code{y} are supplied.
#' If a frequency table is provided an implementation based on SAS
#' documentation is used.\cr The confidence intervals are calculated via
#' z-Transformation.\cr
#' 
#' @inheritParams Association
#' @param na.rm logical, should NAs be removed?
#' @return Either a single numeric value, if no confidence interval is
#' required, \cr or a vector with 3 elements for estimate, lower and upper
#' confidence intervall.  
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{Association}} 
#' @references Conover W. J. (1999) \emph{Practical Nonparametric Statistics
#' (3rd edition)}. Wiley 
#' @keywords multivar
#' @examples
#' 
#' pain <- as.table(matrix(c(26,  6, 26, 7, 23, 
#'                            9, 18, 14, 9, 23), 
#'                            ncol=5, byrow=TRUE, 
#'         dimnames=list(adverse=c("no", "yes"), dose=1:5)))
#' 
#' spearmanCor(pain)
#' 
#' spearmanCor(pain, conf.level=0.95)
#'   
#' # must be the same as
#' with(lapply(untable(pain, colnames = c("adverse","dose")), ordered), 
#'      spearmanCor(adverse, dose, conf.level=0.95))
#' 

#' @export 
spearmanCor <- function(x, y = NULL,
                       conf.level = NA,
                       sides = c("two.sided","left","right"),
                       na.rm = FALSE) {
  
  if(is.null(y)) {
    # implemented following
    # https://support.sas.com/documentation/onlinedoc/stat/151/freq.pdf
    # S. 3103
    
    # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
    # pp 1738
    
    # Old References:
    # https://stat.ethz.ch/pipermail/r-help/2006-October/114319.html
    # fisher z transformation for calc spearmanCor ci :
    # Conover WJ, Practical Nonparametric Statistics (3rd edition). Wiley 1999.
    
    
    n <- sum(x)
    ni. <- apply(x, 1, sum)
    n.j <- apply(x, 2, sum)
    
    ri <- rank(rownames(x))
    ci <- rank(colnames(x))
    ri <- 1:nrow(x)
    ci <- 1:ncol(x)
    
    R1i <- c(sapply(seq_along(ri), 
                    function(i) ifelse(i==1, 0, cumsum(ni.)[i-1]) + ni.[i]/2))
    C1i <- c(sapply(seq_along(ci), 
                    function(i) ifelse(i==1, 0, cumsum(n.j)[i-1]) + n.j[i]/2))
    
    Ri <- R1i - n/2
    Ci <- C1i - n/2
    
    v <- sum(x * outer(Ri, Ci))
    F <- n^3 - sum(ni.^3)
    G <- n^3 - sum(n.j^3)
    
    w <- 1/12*sqrt(F * G)
    
    rho <- v/w
    
  } else {
    
    
    if (is.ordered(x)) x <- as.numeric(x)
    if (is.ordered(y)) y <- as.numeric(y)
    
    if (!is.numeric(x) || !is.numeric(y))
      stop("'x' and 'y' must be numeric or ordered factors.",
           call. = FALSE)
    
    
    # http://www-01.ibm.com/support/docview.wss?uid=swg21478368
    
    if (na.rm) {
      ok <- complete.cases(x, y)
      x  <- x[ok]
      y  <- y[ok]
    }
    
    n <- length(x)
    rho <- cor(x, y, method="spearman")
    
    # rho <- cor(as.numeric(x), as.numeric(y), method="spearman", use = use)

  }
  
  
  e_fx <- exp( 2 * ((.5 * log((1+rho) / (1-rho))) - c(1, -1) *
                      (abs(qnorm((1 - conf.level)/2))) * (1 / sqrt(sum(n) - 3)) ))
  ci <- (e_fx - 1) / (e_fx + 1)
  
  if (is.na(conf.level)) {
    result <- rho
  } else {
    
    if(identical(rho, 1)){     # will blast the fisher z transformation
      result <- c(rho=1, lwr.ci=1, upr.ci=1)
      
    } else {
      pr2 <- 1 - (1 - conf.level) / 2
      result <- c(rho = rho, lwr.ci = max(ci[1], -1), upr.ci = min(ci[2], 1))
    }
  }
  
  return(result)
  
}

