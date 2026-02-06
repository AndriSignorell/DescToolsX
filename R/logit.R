
#' Generalized logit and Inverse logit Function
#' 
#' Compute generalized logit and generalized inverse logit functions.
#' 
#' 
#' The generalized logit function takes values on \verb{[min, max]} and transforms
#' them to span \eqn{[-\infty, \infty ]}{[-Inf, Inf]}. \cr It is defined as:
#' 
#' \deqn{y = log\left (\frac{p}{1-p} \right ) \;\;\; \; \textup{where} \; \;\;
#' p=\frac{x-min}{max-min}}{y = log(p/(1-p)) where p=(x-min)/(max-min)}
#' 
#' %s \deqn{y = log(\frac{p}{(1-p)})}{y = log(p/(1-p))} %s %s where %s %s
#' \deqn{p=\frac{(x-min)}{(max-min)}}{p=(x-min)/(max-min)}
#' 
#' The generalized inverse logit function provides the inverse transformation:
#' 
#' \deqn{x = p' \cdot (max-min) + min \;\;\; \; \textup{where} \; \;\;
#' p'=\frac{exp(y)}{1+exp(y)}}{x = p' * (max-min) + min where p' =
#' exp(y)/(1+exp(y))}
#' 
#' @aliases logit logitInv
#' 
#' @param x value(s) to be transformed
#' @param min lower end of logit interval
#' @param max upper end of logit interval
#' 
#' @return Transformed value(s).
#' 
#' @author Gregory R. Warnes \email{greg@@warnes.net}
#' @seealso \code{\link[car]{logit}}
#' @keywords math
#' @examples
#' 
#' 
#' x <- seq(0,10, by=0.25)
#' xt <- logit(x, min=0, max=10)
#' cbind(x,xt)
#' 
#' y <- logitInv(xt, min=0, max=10)
#' cbind(x, xt, y)



#' @export
logit <- function(x, min=0, max=1) {
  
  # variant in boot:::logit - CHECKME if better ********
  p <- (x-min)/(max-min)
  log(p/(1-p))
}



#' @export
logitInv <- function(x, min=0, max=1) {
  
  p <- exp(x)/(1+exp(x))
  p <- ifelse( is.na(p) & !is.na(x), 1, p ) # fix problems with +Inf
  p * (max-min) + min
}


