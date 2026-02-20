

#' Poisson Confidence Interval 
#' 
#' Computes the confidence intervals of a poisson distributed variable's
#' lambda. Several methods are implemented, see details.
#' 
#' The Wald interval uses the asymptotic normality of the test statistic.
#' 
#' Byar's method is quite a good approximation. Rothman and Boice (1979)
#' mention that these limits were first proposed by Byar (unpublished).
#' 
#' @param x number of events.
#' @param n time base for event count.
#' @param conf.level confidence level, defaults to 0.95.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method character string specifing which method to use; can be one out
#' of \code{"wald"}, \code{"score"}, \code{"exact"} or \code{"byar"}.  Method
#' can be abbreviated. See details. Defaults to \code{"score"}.
#' 
#' @return A vector with 3 elements for estimate, lower confidence intervall
#' and upper for the upper one.
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' 
#' @references Agresti, A. and Coull, B.A. (1998) Approximate is better than
#' "exact" for interval estimation of binomial proportions. \emph{American
#' Statistician}, \bold{52}, pp. 119-126.
#' 
#' Rothman KJ, Boice JD, Jr. (1979) Epidemiologic Analysis with a Programmable
#' Calculator (NIH Publication 79-1649). Washington DC: US Government Printing
#' Office.
#' 
#' Garwood, F. (1936) Fiducial Limits for the Poisson distribution.
#' \emph{Biometrika} 28:437-442.
#' 
#' \url{https://www.ine.pt/revstat/pdf/rs120203.pdf}
#'  
#' @seealso \code{\link{poisson.test}}
#' 
#' @family topic.categoricalData
#' @concept categorical data
#' @concept confidence intervals
#' 
#' @examples
#' # the horse kick example
#' count <- 0:4
#' deaths <- c(144, 91, 32, 11, 2)
#' 
#' n <- sum(deaths)
#' x <- sum(count * deaths)
#' 
#' lambda <- x/n
#' 
#' poissonCI(x=x, n=n, method = c("exact","score", "wald", "byar"))
#' 
#' exp <- dpois(0:4, lambda) * n
#' 
#' barplot(rbind(deaths, exp * n/sum(exp)), names=0:4, beside=TRUE,
#'   col=c("deeppink4", "skyblue3"), main = "Deaths from Horse Kicks", 
#'   xlab = "count")
#' legend("topright", legend=c("observed","expected"), 
#'   fill=c("deeppink4", "skyblue3"), bg="white")
#' 
#' 
#' # SMR, Welsh Nickel workers
#' poissonCI(x=137, n=24.19893)
#' 
#' # Source: https://www.stata.com/manuals/rci.pdf, example 4
#' # (using raw data)
#' petri <- c(1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 3, 0, 1, 2, 
#'            3, 4, 5, 0, 1, 2, 3, 4, 5, 0, 1, 2, 3, 4, 
#'            3, 0, 1, 2, 3, 4, 3, 0)
#' 
#' poissonCI(sum(petri), length(petri))
#' 

#' @export
poissonCI <- function(x, n = 1, conf.level = 0.95, 
                      sides = c("two.sided","left","right"),
                      method = c("exact","score", "wald","byar")) {

  sides <- match.arg(sides)
  
  if (missing(method)) {
    # if not provided take the first method instead of all (!)
    method <- eval(formals(sys.function())$method)[1]
    
  } else {
    # resolve methods cleanly, allowing an ".all" hidden option for method
    method <- .resolveMethod(method, several.ok = TRUE)
  }
  
  res <- .recycleApply(.poissonCI_engine,
                       x = x,
                       n = n,
                       conf.level = conf.level,
                       sides = sides,
                       method = method
  )
  
  if(length(res) == 1)
    out <- res[[1]]
  else{
    out <- as.data.frame(attr(res, "recycle"))
    out <- data.frame(do.call(rbind, res), out)
  }
  
  return(out)
  
}  


# ==  internal helper functions  ===========================================


.poissonCI_engine <- function(x, n, conf.level, sides, method, std_est){

  # ref:  http://www.ijmo.org/papers/189-S083.pdf but wacklig!!!
  # http://www.math.montana.edu/~rjboik/classes/502/ci.pdf
  # http://www.ine.pt/revstat/pdf/rs120203.pdf
  # http://www.pvamu.edu/include/Math/AAM/AAM%20Vol%206,%20Issue%201%20(June%202011)/06_%20Kibria_AAM_R308_BK_090110_Vol_6_Issue_1.pdf
  
  # see also:   pois.conf.int {epitools}
  
  alpha <- 1 - conf.level
  if (sides != "two.sided")
    alpha <- alpha / 2
  
  CI <- switch( method
                , "exact" = { .poissonCI.exact(x, n, alpha) }
                , "wald" =  { .poissonCI.wald(x, n, alpha) }
                , "score" = { .poissonCI.score(x, n, alpha) }
                , "byar" =  { .poissonCI.byar(x, n, alpha) }
                , stop("Unknown method.")
      # agresti-coull is the same as score
      # garwood is the same as exact, check that!!
  )
  
  # this is the default lambda estimator
  est <- x/n
  
  # dot not return ci bounds outside [0, Inf]
  ci <- c( est = est, 
           lci = max(0, CI["lci"]), 
           uci = CI["uci"] )    # no limits on the right side
  
  if(sides=="left")
    ci[3] <- Inf
  else if(sides=="right")
    ci[2] <- 0
  
  return(ci)
  
}



#' @keywords internal
.poissonCI.exact <- function(x, n, alpha) {
    
    return(setNamesX(
        poisson.test(x, n, conf.level = 1-alpha)$conf.int, 
      c("lci","uci")))
}


#' @keywords internal
.poissonCI.score <- function(x, n, alpha) {
  
  z <- qnorm(1 - alpha/2)
  lambda <- x/n

  term1 <- (x + z^2/2)/n
  term2 <- z * n^-0.5 * sqrt(x/n + z^2/(4*n))

  return(setNamesX(term1 - c(1,-1) * term2, c("lci","uci")))
  
}


#' @keywords internal
.poissonCI.wald <- function(x, n, alpha) {
  
  z <- qnorm(1-alpha/2)
  lambda <- x/n

  term2 <- z*sqrt(lambda/n)
  return(setNamesX(lambda - c(1,-1) * term2, c("lci","uci")))
  
}

#' @keywords internal
.poissonCI.byar <- function(x, n, alpha) {
  
  z <- qnorm(1-alpha/2)

  xcc <- x + 0.5
  zz  <- (z/3) * sqrt(1/xcc)
  
  lci <- (xcc * (1 - 1/(9 * xcc) - zz)^3)/n
  uci <- (xcc * (1 - 1/(9 * xcc) + zz)^3)/n
  
  return(c(lci=lci, uci=uci))
  
}
