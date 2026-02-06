
#' Confidence Interval for a Difference of Binomials
#' 
#' Several confidence intervals for the difference between proportions are
#' available, but they can produce markedly different results. Traditional
#' approaches, such as the Wald interval do not perform well unless the sample
#' size is large. Better intervals are available. These include the
#' Agresti/Caffo method (2000), Newcombe Score method (1998) and more computing
#' intensive ones as by Miettinen and Nurminen (1985) or Mee (1984). The latter
#' ones are favoured by Newcombe (when forced to choose between a rock and a
#' hard place). 
#' 
#' All arguments are being recycled.
#' 
#' We estimate the difference between proportions using the sample proportions:
#' \deqn{\hat{\delta} =\hat{p}_1 - \hat{p}_2 = \frac{x_1}{n_1} -
#' \frac{x_2}{n_2}}
#' 
#' The traditional \bold{Wald } confidence interval for the difference of two
#' proportions \eqn{\delta} is based on the asymptotic normal distribution of
#' \eqn{\hat{\delta}}.
#' 
#' The \bold{Corrected Wald} interval uses a continuity correction included in
#' the test statistic. The continuity correction is subtracted from the
#' numerator of the test statistic if the numerator is greater than zero;
#' otherwise, the continuity correction is added to the numerator. The value of
#' the continuity correction is (1/n1 + 1/n2)/2.
#' 
#' The \bold{Agresti-Caffo} (code \code{"ac"}) is equal to the Wald interval
#' with the adjustment according to Agresti, Caffo (2000) for difference in
#' proportions and independent samples. It adds 1 to x1 and x2 and adds 2 to n1
#' and n2 and performs surpringly well.
#' 
#' \bold{Newcombe} (code \code{"scorecc"}) proposed a confidence interval for
#' the difference based on the Wilson score confidence interval for a single
#' proportion. A variant uses a continuity correction for the Wilson interval
#' (code \code{"scorecc"}).
#' 
#' \bold{Miettinen and Nurminen} showed that the restricted maximum likelihood
#' estimates for p1 and p2 can be obtained by solving a cubic equation and gave
#' unique closed-form expressions for them. The Miettinen-Nurminen confidence
#' interval is returned with code \code{"mn"}.
#' 
#' The \bold{Mee} (code \code{"mee"}) interval proposed by Mee (1984) and
#' Farrington-Manning (1990) is using the same maximum likelihood estimators as
#' Miettinen-Nurminen but with another correcting factor.
#' 
#' The \bold{Brown-Li-Jeffreys} (code \code{"blj"}) interval was proposed by
#' Brown and Li (2005).
#' 
#' The \bold{Hauck-Anderson} (code \code{"ha"}) interval was proposed by
#' Hauck-Anderson (1986).
#' 
#' The \bold{Haldane} (code \code{"hal"}) interval is described in Newcombe
#' (1998) and so is the \bold{Jeffreys-Perks} (code \code{"jp"}).
#' 
#' Some approaches for the confidence intervals can potentially yield negative
#' results or values beyond \verb{[-1, 1]}. These would be reset such as not to exceed
#' the range of \verb{[-1, 1]}.
#' 
#' Which of the methods to use is currently still the subject of lively
#' discussion and has not yet been conclusively clarified. See e.g. Fagerland
#' (2011).
#' 
#' The general consensus is that the most widely taught method
#' \code{method="wald"} is inappropriate in many situations and should not be
#' used. Recommendations seem to converge around the Miettinen-Nurminen based
#' methods (\code{method="mn"}).
#' 
#' @param x1 number of successes for the first group.
#' @param n1 number of trials for the first group.
#' @param x2 number of successes for the second group.
#' @param n2 number of trials for the second group.
#' @param conf.level confidence level, defaults to 0.95.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method one of \code{"wald"}, \code{"waldcc"}, \code{"ac"},
#' \code{"score"}, \code{"scorecc"}, \code{"mn"}, \code{"mee"}, \code{"blj"},
#' \code{"ha"}, \code{"hal"}, \code{"jp"}. 
#'  
#' @return A matrix with 3 columns containing the estimate, the lower and the
#' upper confidence intervall.
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{binomCI}}, \code{\link{multinomCI}},
#' \code{\link{binom.test}}, \code{\link{prop.test}},
#' \code{\link{binomRatioCI}} 
#' @references Agresti, A, Caffo, B (2000) Simple and effective confidence
#' intervals for proportions and difference of proportions result from adding
#' two successes and two failures. \emph{The American Statistician} 54 (4),
#' 280-288.
#' 
#' Beal, S L (1987) Asymptotic Confidence Intervals for the Difference Between
#' Two Binomial Parameters for Use with Small Samples; \emph{Biometrics}, 43,
#' 941-950.
#' 
#' Brown L, Li X (2005) Confidence intervals for two sample binomial
#' distribution, \emph{Journal of Statistical Planning and Inference}, 130(1),
#' 359-375.
#' 
#' Hauck WW, Anderson S. (1986) A comparison of large-sample confidence
#' interval methods for the difference of two binomial probabilities \emph{The
#' American Statistician} 40(4): 318-322.
#' 
#' Farrington, C. P. and Manning, G. (1990) Test Statistics and Sample Size
#' Formulae for Comparative Binomial Trials with Null Hypothesis of Non-zero
#' Risk Difference or Non-unity Relative Risk \emph{Statistics in Medicine}, 9,
#' 1447-1454.
#' 
#' Mee RW (1984) Confidence bounds for the difference between two
#' probabilities, \emph{Biometrics} 40:1175-1176 .
#' 
#' Miettinen OS, Nurminen M. (1985) Comparative analysis of two rates.
#' \emph{Statistics in Medicine} 4, 213-226.
#' 
#' Newcombe, R G (1998). Interval Estimation for the Difference Between
#' Independent Proportions: Comparison of Eleven Methods. \emph{Statistics in
#' Medicine}, 17, 873--890.
#' 
#' Fagerland M W, Lydersen S and Laake P (2011) Recommended confidence
#' intervals for two independent binomial proportions, \emph{Statistical
#' Methods in Medical Research} 0(0) 1-31
#' @keywords category
#' @examples
#' 
#' x1 <- 56; n1 <- 70; x2 <- 48; n2 <- 80
#' xci <- binomDiffCI(x1, n1, x2, n2, method=c("wald", "waldcc", "ac", "score",
#'             "scorecc", "mn", "mee", "blj", "ha"))
#' fm(xci[,-1], digits=4)
#' 
#' x1 <- 9; n1 <- 10; x2 <- 3; n2 <- 10
#' yci <- binomDiffCI(x1, n1, x2, n2, method=c("wald", "waldcc", "ac", "score",
#'             "scorecc", "mn", "mee", "blj", "ha"))
#' fm(yci[, -1], digits=4)
#' 
#' # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf, page 9
#' setNamesX(round(
#'   binomDiffCI(56, 70, 48, 80, 
#'               method=c("wald", "waldcc", "hal", 
#'                        "jp", "mee",
#'                        "mn", "score", "scorecc", 
#'                        "ha", "ac", "blj"))[,-1], 4),
#'   rownames=c("1. Wald, no CC", "2. Wald, CC", "3. Haldane", "4. Jeffreys-Perks",
#'              "5. Mee", "6. Miettinen-Nurminen", "10. Score, no CC", "11. Score, CC",
#'              "12. Hauck-Andersen", "13. Agresti-Caffo", "16. Brown-Li"))
#' 
#'  
# x1 <- 56; n1 <- 70; x2 <- 48; n2 <- 80
# xci <- binomDiffCI(x1, n1, x2, n2, method=eval(formals(binomDiffCI)$method))
# 
# alpha <- 0.05
# conf.level <- 0.95
# xci
# 


#' @export
binomDiffCI <- function(x1, n1, x2, n2, conf.level = 0.95, sides = c("two.sided","left","right"),
                        method=c("ac", "wald", "waldcc", "score", "scorecc", "mn",
                                 "mee", "blj", "ha", "hal", "jp")) {
  
  
  if(missing(sides))    sides <- match.arg(sides)
  if(missing(method))   method <- match.arg(method)
  
  
  ibinomDiffCI <- function(x1, n1, x2, n2, conf.level, sides, method) {
    
    #   .Wald #1
    #   .Wald (Corrected) #2
    #   .Exact
    #   .Exact (FM Score)
    #   .Newcombe Score #10
    #   .Newcombe Score (Corrected) #11
    #   .Farrington-Manning
    #   .Hauck-Anderson
    # http://www.jiangtanghu.com/blog/2012/09/23/statistical-notes-5-confidence-intervals-for-difference-between-independent-binomial-proportions-using-sas/
    #  Interval estimation for the difference between independent proportions: comparison of eleven methods.
    
    # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf
    # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.633.9380&rep=rep1&type=pdf
    
    # Newcombe (1998) (free):
    # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.408.7354&rep=rep1&type=pdf

    
    if(sides!="two.sided")
      conf.level <- 1 - 2*(1-conf.level)
    
    alpha <- 1 - conf.level

    p1.hat <- x1/n1
    p2.hat <- x2/n2
    est <- p1.hat - p2.hat
    
    CI <- switch(method,
       "wald" =    { .bdci.wald(x1, n1, x2, n2, alpha, correct=FALSE) },
       "waldcc" =  { .bdci.wald(x1, n1, x2, n2, alpha, correct=TRUE) },
       "ac" =      { .bdci.ac(x1, n1, x2, n2, alpha)  } ,     # Agresti-Caffo
       "exact" =   { .bdci.exact(x1, n1, x2, n2, alpha) },    # exact
       "score" =   { .bdci.score(x1, n1, x2, n2, alpha) },    # Newcombe
       "scorecc" = { .bdci.scorecc(x1, n1, x2, n2, alpha) },  # Newcombe
       "mee" =     { .bdci.mee(x1, n1, x2, n2, alpha)  },     # Mee, also called Farrington-Mannig
       "blj" =     { .bdci.blj(x1, n1, x2, n2, alpha) },      # brown-li-jeffreys
       "ha" =      { .bdci.ha(x1, n1, x2, n2, alpha) },       # Hauck-Anderson
       "mn" =      { .bdci.mn(x1, n1, x2, n2, alpha)   },     # Miettinen-Nurminen
       "beal" =    { .bdci.beal(x1, n1, x2, n2, alpha) },     # Beal
       "hal" =     { .bdci.hal(x1, n1, x2, n2, alpha) },      # haldane 
       "jp" =      { .bdci.jp(x1, n1, x2, n2, alpha) }        # jeffreys-perks
    )

    ci <- c(est = est, 
            lci = max(-1, min(CI)), uci = min(1, max(CI)))
    
    if(sides=="left")
      ci[3] <- 1
    else if(sides=="right")
      ci[2] <- -1
    
    return(ci)
    
  }
  
  
  method <- match.arg(arg=method, several.ok = TRUE)
  sides <- match.arg(arg=sides, several.ok = TRUE)
  
  # Recycle arguments
  lst <- recycle(x1=x1, n1=n1, x2=x2, n2=n2, 
                 conf.level=conf.level, sides=sides, method=method)
  
  res <- t(sapply(1:attr(lst, "maxdim"),
                  function(i) ibinomDiffCI(x1=lst$x1[i], n1=lst$n1[i], x2=lst$x2[i], n2=lst$n2[i],
                                           conf.level=lst$conf.level[i],
                                           sides=lst$sides[i],
                                           method=lst$method[i])))
  
  # get rownames
  lgn <- recycle(x1=if(is.null(names(x1))) paste("x1", seq_along(x1), sep=".") else names(x1),
                 n1=if(is.null(names(n1))) paste("n1", seq_along(n1), sep=".") else names(n1),
                 x2=if(is.null(names(x2))) paste("x2", seq_along(x2), sep=".") else names(x2),
                 n2=if(is.null(names(n2))) paste("n2", seq_along(n2), sep=".") else names(n2),
                 conf.level=conf.level, sides=sides, method=method)
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 1)]), 1, paste, collapse=":")
  
  rownames(res) <- xn
  return(res)
  
}



# ===============================================================
# internal helper functions


.bdci.wald <- function(x1, n1, x2, n2, alpha, correct=FALSE) {
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  SE <- p1.hat * (1 - p1.hat) / n1 + p2.hat * (1 - p2.hat) / n2
  ME <- qnorm(1 - alpha/2) * sqrt(SE)
  
  if(correct)
    ME <- ME + 0.5 * (1/n1 + 1/n2)
  
  return( c(lci=est - ME, uci=est + ME) )
  
}  


.bdci.ac <- function(x1, n1, x2, n2, alpha) {
  # "ac" = Agresti-Caffo
  
  n1 <- n1+2
  n2 <- n2+2
  x1  <- x1+1
  x2  <- x2+1
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  ME <- qnorm(1 - alpha/2) * 
    sqrt(p1.hat * (1-p1.hat) / n1 + p2.hat * (1-p2.hat) / n2)
  
  return( c( lci = est - ME, uci = est + ME) )
} 



.bdci.exact <- function(p1.hat, n1, p2.hat, n2, alpha) {
  # exact
  warning("exact is not yet implemented!")
  return( c( lci = NA, uci = NA) )
  
}


.bdci.score <- function(x1, n1, x2, n2, alpha) {
  # "score" or newcombe
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  z <- qnorm(1 - alpha/2)
  
  ci1 <- binomCI(x=x1, n=n1, conf.level=1-alpha, method="wilson")[1,]
  ci2 <- binomCI(x=x2, n=n2, conf.level=1-alpha, method="wilson")[1,]
  
  lci <- est - z * sqrt( ci1["lci"] * (1-ci1["lci"])/n1 + ci2["uci"] * (1-ci2["uci"])/n2)
  uci <- est + z * sqrt( ci1["uci"] * (1-ci1["uci"])/n1 + ci2["lci"] * (1-ci2["lci"])/n2)
  
  return( c( lci = lci, uci = uci) )
  
}


.bdci.scorecc <- function(x1, n1, x2, n2, alpha) {
  # "scorecc" or newcombe_cc 
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  ci1 <- binomCI(x=x1, n=n1, conf.level=1-alpha, method="wilsoncc")[1,]
  ci2 <- binomCI(x=x2, n=n2, conf.level=1-alpha, method="wilsoncc")[1,]
  
  lci <- est - sqrt((p1.hat - ci1["lci"])^2 + (ci2["uci"] - p2.hat)^2) 
  uci <- est + sqrt((ci1["uci"] - p1.hat)^2 + (p2.hat - ci2["lci"])^2) 
  
  return( c( lci = lci, uci = uci) )
  
}


.bdci.blj <- function(x1, n1, x2, n2, alpha) {
  # "blj"  brown-li-jeffreys
  
  p1.hat <- (x1 + 0.5) / (n1 + 1)
  p2.hat <- (x2 + 0.5) / (n2 + 1)
  est <- p1.hat - p2.hat
  
  ME <- qnorm(1 - alpha/2) * 
    sqrt(p1.hat * (1 - p1.hat)/n1 + p2.hat * (1 - p2.hat)/n2)
  
  return( c( lci = est - ME, uci = est + ME) )
  
}


.bdci.ha <- function(x1, n1, x2, n2, alpha) {
  # "ha"  Hauck-Anderson
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  ME <- 1/(2 * min(n1, n2)) + 
    qnorm(1 - alpha/2) * 
    sqrt(p1.hat * (1 - p1.hat)/(n1-1) + p2.hat * (1 - p2.hat) / (n2-1))
  
  return( c( lci = est - ME, uci = est + ME) )
  
}


.bdci.mn <- function(x1, n1, x2, n2, alpha) {
  # "mn"  Miettinen-Nurminen
  z <- qchisq(1-alpha, 1)
  
  return( c(
    lci = binomdiffciMN(x1, n1, x2, n2, z, TRUE),
    uci = binomdiffciMN(x1, n1, x2, n2, z, FALSE)
  ))
  
}


.bdci.mee <- function(x1, n1, x2, n2, alpha) {
  #  "mee"  Mee, also called Farrington-Mannig
  
  return( c(
    lci = binomdiffciMee(x1, n1, x2, n2, alpha, TRUE),
    uci = binomdiffciMee(x1, n1, x2, n2, alpha, FALSE)
  ))
  
}



.bdci.hal <- function(x1, n1, x2, n2, alpha, correct=FALSE) {
  # "hal"  haldane 
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  
  psi <- (p1.hat + p2.hat) / 2
  
  if(correct)
    # "jp" jeffreys-perks
    # same as haldane but with other psi
    psi <- 0.5 * ((x1 + 0.5) / (n1 + 1) + (x2 + 0.5) / (n2 + 1) )
  
  u <- (1/n1 + 1/n2) / 4
  v <- (1/n1 - 1/n2) / 4
  
  z <- qnorm(1 - alpha/2)
  
  theta <- ((p1.hat - p2.hat) + z^2 * v * (1 - 2*psi)) / (1 + z^2 * u)
  w <- z / (1+z^2*u) * sqrt(u * (4*psi*(1-psi) - (p1.hat - p2.hat)^2) + 
                              2*v*(1-2*psi) *(p1.hat - p2.hat) + 
                              4*z^2*u^2*(1-psi)*psi + z^2*v^2*(1-2*psi)^2)
  
  return( c( lci = theta - w, uci = theta + w) )
  
}


.bdci.jp <- function(x1, n1, x2, n2, alpha) {
  # "jp" jeffreys-perks
  
  # same as haldane but with other psi
  .bdci.hal(x1, n1, x2, n2, alpha, correct=TRUE)
}



.bdci.beal <- function(p1.hat, n1, p2.hat, n2, alpha, correct=FALSE) {
  # "beal" = {
  
  # experimental code only...
  # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.633.9380&rep=rep1&type=pdf
  
  a <- p1.hat + p2.hat
  b <- p1.hat - p2.hat
  u <- ((1/n1) + (1/n2)) / 4
  v <- ((1/n1) - (1/n2)) / 4
  V <- u*((2-a)*a - b^2) + 2*v*(1-a)*b
  z <- qchisq(p=1-alpha/2, df = 1)
  A <- sqrt(z*(V + z*u^2*(2-a)*a + z*v^2*(1-a)^2))
  B <- (b + z*v*(1-a)) / (1+z*u)
  
  CI.lower <- max(-1, B - A / (1 + z*u))
  CI.upper <- min(1, B + A / (1 + z*u))
  
}



