
#' Confidence Intervals for Multinomial Proportions
#' 
#' Confidence intervals for multinomial proportions are often approximated by
#' single binomial confidence intervals, which might in practice often yield
#' satisfying results, but is properly speaking not correct. This function
#' calculates simultaneous confidence intervals for multinomial proportions
#' either according to the methods of Sison and Glaz, Goodman, Wald, Wald with
#' continuity correction or Wilson.
#' 
#' Given a vector of observations with the number of samples falling in each
#' class of a multinomial distribution, builds the simultaneous confidence
#' intervals for the multinomial probabilities according to the method proposed
#' by the mentioned authors. The R code for Sison and Glaz (1995) has been
#' translated from thes SAS code written by May and Johnson (2000). See the
#' references for the other methods (qh = Quesenberry-Hurst, fs =
#' Fitzpatrick-Scott).\cr Some approaches for the confidence intervals can
#' potentially yield negative results or values beyond 1. These would be reset
#' such as not to exceed the range of \verb{[0, 1]}.
#' 
#' @param x A vector of positive integers representing the number of
#' occurrences of each class. The total number of samples equals the sum of
#' such elements.
#' @param conf.level confidence level, defaults to 0.95.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method character string specifing which method to use; can be one out
#' of \code{"sisonglaz"}, \code{"cplus1"}, \code{"goodman"}, \code{"wald"},
#' \code{"waldcc"}, \code{"wilson"}, \code{"qh"}, \code{"fs"}.  Method can be
#' abbreviated. See details. Defaults to \code{"sisonglaz"}.
#' @return A matrix with 3 columns: \item{est}{estimate} \item{lwr.ci}{lower
#' bound of the confidence interval} \item{upr.ci}{upper bound of the
#' confidence interval}
#' 
#' The number of rows correspond to the dimension of x.
#' @author Pablo J. Villacorta Iglesias <pjvi@@decsai.ugr.es>\cr Department of
#' Computer Science and Artificial Intelligence, University of Granada (Spain)
#' (Sison-Glaz)
#' 
#' Andri Signorell <andri@@signorell.net> (Goodman, Wald, Wilson,
#' Fitzpatrick-Scott, Quesenberry-Hurst)
#' @references Fitzpatrick, S. and Scott, A. (1987). Quick simultaneous
#' confidence interval for multinomial proportions. \emph{Journal of American
#' Statistical Association} 82(399): 875-878.
#' 
#' Glaz, J., Sison, C.P. (1999) Simultaneous confidence intervals for
#' multinomial proportions. \emph{Journal of Statistical Planning and
#' Inference} 82:251-262.
#' 
#' Goodman, L. A. (1965) On Simultaneous Confidence Intervals for Multinomial
#' Proportions \emph{Technometrics}, 7, 247-254.
#' 
#' May, W.L., Johnson, W.D.(2000) Constructing two-sided simultaneous
#' confidence intervals for multinomial proportions for small counts in a large
#' number of cells. \emph{Journal of Statistical Software} 5(6) . Paper and
#' code available at \url{https://www.jstatsoft.org/v05/i06}.
#' 
#' Quesenberry, C.P. and Hurst, D.C. (1964). Large Sample Simultaneous
#' Confidence Intervals for Multinational Proportions. \emph{Technometrics}, 6:
#' 191-195.
#' 
#' Sangeetha, U., Subbiah, M., Srinivasan, M. R. (2013) Mathematical Analysis
#' of propensity of aberration on the methods for interval estimation of the
#' multinomial proportions. \emph{IOSR Journal of Mathematics}, e-ISSN:
#' 2278-5728,p-ISSN: 2319-765X, Volume 7, Issue 4 (Jul. - Aug. 2013), PP 23-28
#' 
#' Sison, C.P and Glaz, J. (1995) Simultaneous confidence intervals and sample
#' size determination for multinomial proportions. \emph{Journal of the
#' American Statistical Association}, 90:366-369.
#' 
#' Wald, A. Tests of statistical hypotheses concerning several parameters when
#' the number of observations is large, \emph{Trans. Am. Math. Soc.} 54 (1943)
#' 426-482.
#' 
#' Wilson, E. B. Probable inference, the law of succession and statistical
#' inference, \emph{J.Am. Stat. Assoc.} 22 (1927) 209-212.
#' @keywords univar
#' @examples
#' 
#' # Multinomial distribution with 3 classes, from which a sample of 79 elements
#' # were drawn: 23 of them belong to the first class, 12 to the
#' # second class and 44 to the third class. Punctual estimations
#' # of the probabilities from this sample would be 23/79, 12/79
#' # and 44/79 but we want to build 95% simultaneous confidence intervals
#' # for the true probabilities
#' 
#' multinomCI(c(23, 12, 44), conf.level=0.95)
#' 
#' # single sided
#' multinomCI(c(23, 12, 44), conf.level=0.95, sides="left")
#' multinomCI(c(23, 12, 44), conf.level=0.95, sides="right")
#' 
#' 
#' x <- c(35, 74, 22, 69)
#' 
#' multinomCI(x, method="goodman")
#' multinomCI(x, method="sisonglaz")
#' multinomCI(x, method="cplus1")
#' multinomCI(x, method="wald")
#' multinomCI(x, method="waldcc")
#' multinomCI(x, method="wilson")
#' 
#' # compare to
#' binomCI(x, n=sum(x))
#' 
#' # example in Goodman (1965)
#' multinomCI(x=c(91, 49, 37, 43), conf.level=0.95, method="goodman")
#' 
#' # example from Sison, Glaz (1999) in Sangeetha (2013) - Table 2
#' #
#' #    Wald          Wald_CC       Wilson        Quesnberry-Hurst	
#' #    LL     UL     LL     UL     LL     UL     LL     UL
#' # 1	 0.090	0.149  0.089  0.150	 0.094	0.153  0.076  0.183
#' # 2  0.121	0.187  0.120  0.188	 0.124	0.190  0.104  0.222
#' # 3	 0.123	0.189  0.122  0.190	 0.126	0.192  0.106  0.225
#' # 4	 0.096	0.156  0.095  0.158	 0.099	0.160  0.081  0.191
#' # 5	 0.102	0.164  0.101  0.165	 0.105	0.167  0.087  0.198
#' # 6	 0.151	0.222  0.150  0.223	 0.154	0.224  0.131  0.258
#' # 7	 0.094	0.154  0.093  0.155	 0.097	0.157  0.080  0.188
#'  
#' #    Goodman        Fitzpatrick-Scott  Sison-Glaz	
#' #    LL     UL      LL     UL          LL    UL
#' # 1	 0.085	0.166   0.075  0.165       0.079	0.164
#' # 2	 0.115	0.204   0.109  0.200       0.114	0.199
#' # 3	 0.116	0.207   0.111  0.202       0.116	0.201
#' # 4	 0.091	0.173   0.081  0.172       0.086	0.171
#' # 5	 0.096	0.181   0.087  0.178       0.092	0.177
#' # 6	 0.143	0.239   0.141  0.232       0.146	0.231
#' # 7	 0.089	0.171   0.079  0.170       0.084	0.169
#' 
#' x <- c(56, 72, 73, 59, 62, 87, 58)
#' do.call(cbind, lapply(c("wald", "waldcc", "wilson", 
#'                         "qh", "goodman", "fs", "sisonglaz"),
#'                       function(m) round(multinomCI(x, method=m)[,-1], 3)))
#'        


#' @export
multinomCI <- function(x, conf.level = 0.95, sides = c("two.sided","left","right"),
                       method = c("sisonglaz", "cplus1", "goodman", "wald", "waldcc", "wilson", "qh", "fs")) {
  
  # Code originally from 
  # Pablo J. Villacorta Iglesias <pjvi@decsai.ugr.es>\n
  # Department of Computer Science and Artificial Intelligence, University of Granada (Spain)
  
  # rewritten in R by Andri Signorell

  n <- sum(x, na.rm=TRUE)
  k <- length(x)
  p <- x/n
  
  if (missing(method)) method <- "sisonglaz"
  if(missing(sides)) sides <- "two.sided"
  
  sides <- match.arg(sides, choices = c("two.sided","left","right"), 
                     several.ok = FALSE)
  if(sides!="two.sided")
    conf.level <- 1 - 2 * (1 - conf.level)

  
  method <- match.arg(arg = method, 
                      choices = c("sisonglaz", "cplus1", "goodman", 
                                  "wald", "waldcc", "wilson", "qh", "fs"))
  
  res <- switch( method
        , "goodman" =   { .multinomCI.goodman(x, n, k, conf.level) }
        , "wald" =      { .multinomCI.wald(x, n, conf.level) }
        , "waldcc" =    { .multinomCI.wald_cc(x, n, conf.level) }
        , "wilson" =    { .multinomCI.wilson(x, n, conf.level) }
        , "fs" =        { .multinomCI.fs(x, n, conf.level) }
        , "qh" =        { .multinomCI.qh(x, n, k, conf.level) }
        , "sisonglaz" = { .multinomCI.sisonglaz(x, n, k, conf.level) }
        , "cplus1" =    { .multinomCI.cplus1(x, n, k, conf.level) }
        )
  
  if(sides=="left")
    res[, 3] <- 1
  else if(sides=="right")
    res[, 2] <- 0
  
  return(res)
}


#' @keywords internal
.moments <- function(c, lambda) {
  
  a <- lambda + c
  b <- max(lambda - c, 0)
  
  den <- diff(ppois(c(b - 1, a), lambda))
  
  r <- 1:4
  
  poisA <- ppois(a, lambda) - ppois(a - r, lambda) 
  poisB <- ppois(b - 1, lambda) - ppois(b - r - 1, lambda)
  mu <- setNamesX(lambda^r * (1 - (poisA - poisB)/den), LETTERS[1:4])
  
  res <- with(as.list(mu), 
              c(A, 
                A + B - A^2, 
                C + B * (3 - 3 * A) + 
                  (A - 3 * A^2 + 2 * A^3),
                D + C * (6 - 4 * A) + 
                  B * (7 - 12 * A + 6 * A^2) + 
                  A - 4 * A^2 + 6 * A^3 - 3 * A^4, 
                den
              ))
  return(res)
  
}


#' @keywords internal
.truncpoi <- function(c, x, n, k) {
  
  m <- t(sapply(x, .moments, c=c))
  m[,4] <- m[,4] - 3*m[, 2]^2
  
  probn <- 1/(ppois(n, n) - ppois(n - 1, n))
  
  cS <- as.list(setNamesX(colSums(m), LETTERS[1:5]))
  z  <- with(cS, (n - A)/sqrt(B))
  g1 <- with(cS, C/(B^(3/2)))
  g2 <- with(cS, D/(B^2))
  
  poly <- 1 + g1 * (z^3 - 3 * z)/6 + g2 * (z^4 - 6 * z^2 + 3)/24 + 
    g1^2 * (z^6 - 15 * z^4 + 45 * z^2 - 15)/72
  
  f <- poly * exp(-z^2/2)/(sqrt(2) * gamma(0.5))
  
  probx <- prod(m[, 5])
  
  return(probn * probx * f/sqrt(cS$B))
  
}


#' @keywords internal
.multinomCI.goodman <- function(x, n, k, conf.level) {
  
  q.chi <- qchisq(1 - (1-conf.level)/k, df = 1)
  
  lci <- (q.chi + 2*x - sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))
  uci <- (q.chi + 2*x + sqrt(q.chi*(q.chi + 4*x*(n-x)/n))) / (2*(n+q.chi))

  res <- cbind(est=x/n, lci=pmax(0, lci), uci=pmin(1, uci))
  return(res)  
} 


#' @keywords internal
.multinomCI.wald <- function(x, n, conf.level) {
  
  p <- x/n  
  
  q.chi <- qchisq(conf.level, 1)
  lci <- p - sqrt(q.chi * p * (1 - p)/n)
  uci <- p + sqrt(q.chi * p * (1 - p)/n)
  
  res <- cbind(est=p, lci=pmax(0, lci), uci=pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomCI.wald_cc <- function(x, n, conf.level) {
    
  p <- x/n  
  
  q.chi <- qchisq(conf.level, 1)
  lci <- p - sqrt(q.chi * p * (1 - p)/n) - 1/(2*n)
  uci <- p + sqrt(q.chi * p * (1 - p)/n) + 1/(2*n)
  
  res <- cbind(est=p, lci=pmax(0, lci), uci=pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomCI.wilson <- function(x, n, conf.level) {
  
  p <- x/n  

  q.chi <- qchisq(conf.level, 1)
  lci <- (q.chi + 2*x - sqrt(q.chi^2 + 4*x*q.chi * (1 - p))) / (2*(q.chi + n))
  uci <- (q.chi + 2*x + sqrt(q.chi^2 + 4*x*q.chi * (1 - p))) / (2*(q.chi + n))
  
  res <- cbind(est=p, lci=pmax(0, lci), uci=pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomCI.fs <- function(x, n, conf.level) {
    
  # references Fitzpatrick, S. and Scott, A. (1987). Quick simultaneous confidence 
  # interval for multinomial proportions. 
  # Journal of American Statistical Association 82(399): 875-878.
  
  p <- x/n  
  
  q.snorm <- qnorm(1-(1 - conf.level)/2)
  
  lci <- p - q.snorm / (2 * sqrt(n))
  uci <- p + q.snorm / (2 * sqrt(n))
  
  res <- cbind(est = p, lci = pmax(0, lci), uci = pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomCI.qh <- function(x, n, k, conf.level) {
    
  # references Quesensberry, C.P. and Hurst, D.C. (1964). 
  # Large Sample Simultaneous Confidence Intervals for 
  # Multinational Proportions. Technometrics, 6: 191-195.
  
  p <- x/n  
  
  q.chi <- qchisq(conf.level, df = k-1)
  
  lci <- (q.chi + 2*x - sqrt(q.chi^2 + 4*x*q.chi*(1 - p)))/(2*(q.chi+n))
  uci <- (q.chi + 2*x + sqrt(q.chi^2 + 4*x*q.chi*(1 - p)))/(2*(q.chi+n))
  
  res <- cbind(est = p, lci = pmax(0, lci), uci = pmin(1, uci))
  return(res)  
  
} 


#' @keywords internal
.multinomCI.sisonglaz <- function(x, n, k, conf.level) {

  pold <- const <- 0

  for(cc in 1:n){
    poi <- .truncpoi(cc, x, n, k)
    if(poi > conf.level && pold < conf.level) {
      const <- cc
      break
    }
    pold <- poi
  }
  
  delta <- (conf.level - pold)/(poi - pold)
  const <- const - 1

  p <- x/n  

  res <- cbind(est = p, 
               lci = pmax(0, p - const/n), 
               uci = pmin(1, p + const/n + 2*delta/n))
  return(res)  
  
}


#' @keywords internal
.multinomCI.cplus1 <- function(x, n, k, conf.level) {
  
  pold <- const <- 0

  for(cc in 1:n){
    poi <- .truncpoi(cc, x, n, k)
    if(poi > conf.level && pold < conf.level) {
      const <- cc
      break
    }
    pold <- poi
  }
  
  delta <- (conf.level - pold)/(poi - pold)
  const <- const - 1
  
  p <- x/n  
  
  res <- cbind(est = p, 
               lci = pmax(0, p - const/n - 1/n), 
               uci = pmin(1, p + const/n + 1/n))
  return(res)  
  
}  


