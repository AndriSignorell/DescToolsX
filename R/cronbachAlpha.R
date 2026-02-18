

#' Cronbach's Coefficient Alpha
#' 
#' Cronbach's alpha is a measure of internal consistency and often used for
#' validating psychometric tests. It determines the internal consistency or
#' average correlation of items in a survey instrument to gauge its
#' reliability. This reduces to Kuder-Richardson formula 20 (KR-20) when the
#' columns of the data matrix are dichotomous.
#' 
#' 
#' @param x \eqn{n \times m}{k x m} matrix or dataframe with item responses, k
#' subjects (in rows) m items (in columns).
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence interval will be calculated.
#' @param cond logical. If set to \code{TRUE}, alpha is additionally calculated
#' for the dataset with each item left out.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. If set to \code{TRUE} only the complete
#' cases of the ratings will be used. Defaults to \code{FALSE}.
#' @return Either a numeric value or \cr a named vector of 3 columns if
#' confidence levels are required (estimate, lower and upper ci) or \cr
#' 
#' a list containing the following components, if the argument \code{cond} is
#' set to \code{TRUE}: \item{unconditional}{Cronbach's Alpha, either the single
#' value only or with confidence intervals} \item{condcronbachAlpha}{The alpha
#' that would be realized if the item were excluded}
#' @author Andri Signorell <andri@@signorell.net>, based on code of Harold C.
#' Doran
#' @seealso \code{\link{cohenKappa}}, \code{\link{kappaM}}
#' @references Cohen, J. (1960), A coefficient of agreement for nominal scales.
#' \emph{Educational and Psychological Measurement}, 20, 37-46.
#' 
#' @family topic.InternalConsistency
#' @concept Reliability
#' @concept Internal Consistency
#' @concept Scale Reliability
#' 
#' @examples
#' 
#' set.seed(1234)
#' tmp <- data.frame(
#'   item1=sample(c(0,1), 20, replace=TRUE),
#'   item2=sample(c(0,1), 20, replace=TRUE),
#'   item3=sample(c(0,1), 20, replace=TRUE),
#'   item4=sample(c(0,1), 20, replace=TRUE),
#'   item5=sample(c(0,1), 20, replace=TRUE)
#'   )
#' 
#' cronbachAlpha(tmp[,1:4], cond=FALSE, conf.level=0.95)
#' cronbachAlpha(tmp[,1:4], cond=TRUE, conf.level=0.95)
#' 
#' cronbachAlpha(tmp[,1:4], cond=FALSE)
#' cronbachAlpha(tmp[,1:2], cond=TRUE, conf.level=0.95)
#' 
#' \dontrun{
#' # Calculate bootstrap confidence intervals for cronbachAlpha
#' library(boot)
#' cronbach.boot <- function(data,x) {cronbachAlpha(data[x,])[[3]]}
#' res <- boot::boot(datafile, cronbach.boot, 1000)
#' 
#' # two-sided bootstrapped confidence interval of Cronbach's alpha
#' quantile(res$t, c(0.025,0.975))   
#' # adjusted bootstrap percentile (BCa) confidence interval (better)
#' boot::boot.ci(res, type="bca")    
#' }
#' 

#' @export
cronbachAlpha <- function(x, conf.level = NA, cond = FALSE, na.rm = FALSE){
  
  i.cronbachAlpha <- function(x, conf.level = NA){
    nc <- ncol(x)
    colVars <- apply(x, 2, var)
    total   <- var(rowSums(x))
    res <- (total - sum(colVars)) / total * (nc/(nc-1))
    
    if (!is.na(conf.level)) {
      N <- length(x)
      ci <- 1 - (1-res) * qf( c(1-(1-conf.level)/2, (1-conf.level)/2), N-1, (nc-1)*(N-1))
      res <- c("Cronbach Alpha"=res, lwr.ci=ci[1], upr.ci=ci[2])
    }
    return(res)
  }
  
  
  x <- as.matrix(x)
  if(na.rm) x <- na.omit(x)
  
  res <- i.cronbachAlpha(x = x, conf.level = conf.level)
  
  if(cond) {
    condcronbachAlpha <- list()
    n <- ncol(x)
    if(n > 2) {     # can't calculate conditional with only 2 items
      for(i in 1:n){
        condcronbachAlpha[[i]] <- i.cronbachAlpha(x[,-i], conf.level = conf.level)
      }
      condcronbachAlpha <- data.frame(Item = 1:n, do.call("rbind", condcronbachAlpha))
      colnames(condcronbachAlpha)[2] <- "Cronbach Alpha"
    }
    res <- list(unconditional=res, condcronbachAlpha = condcronbachAlpha)
  }
  
  return(res)
}


