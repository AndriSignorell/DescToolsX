
#' Confidence Intervals for the Difference of Two Binomial Proportions
#'
#' Computes confidence intervals for the difference between two independent
#' binomial proportions. A variety of classical and modern methods are
#' available, which may yield substantially different results, particularly
#' for small sample sizes or extreme proportions.
#'
#' All arguments are vectorized and recycled according to standard R rules.
#'
#' The difference in proportions is estimated by
#' \deqn{
#' \hat{\delta} = \hat{p}_1 - \hat{p}_2 =
#' \frac{x_1}{n_1} - \frac{x_2}{n_2}.
#' }
#'
#' \strong{Wald}:
#' The traditional large-sample normal approximation interval based on the
#' asymptotic distribution of \eqn{\hat{\delta}}.
#'
#' \strong{Wald with continuity correction}:
#' A continuity-corrected version of the Wald interval. The correction
#' term \eqn{(1/n_1 + 1/n_2)/2} is added or subtracted from the test statistic
#' depending on its sign.
#'
#' \strong{Agresti-Caffo}:
#' A simple adjustment of the Wald interval (Agresti and Caffo, 2000) obtained
#' by adding one success and one failure to each group. This approach performs
#' well in many practical situations.
#'
#' \strong{Newcombe score}:
#' Based on inverting the Wilson score interval for each proportion and
#' combining them to obtain an interval for the difference (Newcombe, 1998).
#'
#' \strong{Newcombe score with continuity correction}:
#' A continuity-corrected variant of the Newcombe score method.
#'
#' \strong{Miettinen-Nurminen}:
#' Based on restricted maximum likelihood estimation obtained by solving a
#' cubic equation (Miettinen and Nurminen, 1985). Often recommended for small
#' to moderate sample sizes.
#'
#' \strong{Mee-Farrington-Manning}:
#' Uses the same maximum likelihood estimators as the
#' Miettinen-Nurminen method but applies a different correction factor
#' (Mee, 1984; Farrington and Manning, 1990).
#'
#' \strong{Brown-Li-Jeffreys}:
#' A method proposed by Brown and Li (2005).
#'
#' \strong{Hauck-Anderson}:
#' A large-sample method described by Hauck and Anderson (1986).
#'
#' \strong{Beal}:
#' An asymptotic method intended for use with small samples (Beal, 1987).
#'
#' \strong{Haldane}:
#' Described in Newcombe (1998), based on adding 0.5 to all cells.
#'
#' \strong{Jeffreys-Perks}:
#' Also described in Newcombe (1998), based on Bayesian-type adjustments.
#'
#' Some methods may produce limits outside the admissible parameter space
#' \eqn{[-1, 1]}. In such cases, interval bounds are truncated to remain within
#' the valid range.
#'
#' The choice of method remains an active topic of discussion. The Wald
#' interval is known to perform poorly in many practical situations.
#' Reviews such as Fagerland et al. (2011) provide comparative evaluations and
#' recommendations.
#' 
#' Miettinen-Nurminen might be a sensible default. Newcombe-Score is almost equivalent.
#'
#' @param x1 Number of successes in the first group.
#' @param n1 Number of trials in the first group.
#' @param x2 Number of successes in the second group.
#' @param n2 Number of trials in the second group.
#' @param conf.level Confidence level, default is 0.95.
#' @param sides A character string specifying the type of confidence interval:
#'   \code{"two.sided"} (default), \code{"left"}, or \code{"right"}.
#'   Partial matching is allowed.
#' @param method One of:
#'   \code{"wald"},
#'   \code{"wald-cc"},
#'   \code{"agresti-caffo"},
#'   \code{"exact"},
#'   \code{"newcombe-score"},
#'   \code{"newcombe-score-cc"},
#'   \code{"miettinen-nurminen"},
#'   \code{"mee-farrington-manning"},
#'   \code{"brown-li-jeffreys"},
#'   \code{"hauck-anderson"},
#'   \code{"beal"},
#'   \code{"haldane"},
#'   \code{"jeffreys-perks"}.
#'
#' @return A matrix with three columns containing the estimate of the
#' difference and the lower and upper confidence limits.
#'
#' @references
#' Agresti A, Caffo B (2000).
#' Simple and effective confidence intervals for proportions and difference of
#' proportions result from adding two successes and two failures.
#' \emph{The American Statistician}, 54(4), 280-288.
#'
#' Beal SL (1987).
#' Asymptotic confidence intervals for the difference between two binomial
#' parameters for use with small samples.
#' \emph{Biometrics}, 43, 941-950.
#'
#' Brown L, Li X (2005).
#' Confidence intervals for two sample binomial distribution.
#' \emph{Journal of Statistical Planning and Inference}, 130(1), 359-375.
#'
#' Fagerland MW, Lydersen S, Laake P (2011).
#' Recommended confidence intervals for two independent binomial proportions.
#' \emph{Statistical Methods in Medical Research}.
#'
#' Farrington CP, Manning G (1990).
#' Test statistics and sample size formulae for comparative binomial trials.
#' \emph{Statistics in Medicine}, 9, 1447-1454.
#'
#' Hauck WW, Anderson S (1986).
#' A comparison of large-sample confidence interval methods for the difference
#' of two binomial probabilities.
#' \emph{The American Statistician}, 40(4), 318-322.
#'
#' Mee RW (1984).
#' Confidence bounds for the difference between two probabilities.
#' \emph{Biometrics}, 40, 1175-1176.
#'
#' Miettinen OS, Nurminen M (1985).
#' Comparative analysis of two rates.
#' \emph{Statistics in Medicine}, 4, 213-226.
#'
#' Newcombe RG (1998).
#' Interval estimation for the difference between independent proportions.
#' \emph{Statistics in Medicine}, 17, 873-890.
#'
#' @seealso \code{\link{binom.test}}, \code{\link{prop.test}}
#'
#' @family topic.categoricalData
#' @concept categorical data
#' @concept confidence intervals
#'  
#' @examples
#' 
#' x1 <- 56; n1 <- 70; x2 <- 48; n2 <- 80
#' meths <- c("wald", "wald-cc", "agresti-caffo", 
#'                     "newcombe-score", "newcombe-score-cc", 
#'                     "miettinen-nurminen", "mee-farrington-manning", 
#'                     "brown-li-jeffreys", "hauck-anderson")
#'                     
#' xci <- binomDiffCI(x1, n1, x2, n2, method=meths)
#' fm(xci[,-1], digits=4)
#' 
#' x1 <- 9; n1 <- 10; x2 <- 3; n2 <- 10
#' yci <- binomDiffCI(x1, n1, x2, n2, method=meths)
#' fm(yci[, -1], digits=4)
#' 
#' # https://www.lexjansen.com/wuss/2016/127_Final_Paper_PDF.pdf, page 9
#' setNamesX(round(
#'   binomDiffCI(56, 70, 48, 80, 
#'               method=c("wald", "wald-cc", "haldane", 
#'                        "jeffreys-perks", "mee-farrington-manning",
#'                        "miettinen-nurminen", "newcombe-score", 
#'                        "newcombe-score-cc", 
#'                        "hauck-anderson", "agresti-caffo" ,
#'                        "brown-li-jeffreys")
#'   )[,c(2,3)], 4),
#'   rownames=c("1. Wald, no CC", "2. Wald, CC", "3. Haldane", "4. Jeffreys-Perks",
#'              "5. Mee", "6. Miettinen-Nurminen", "10. Score, no CC", "11. Score, CC",
#'              "12. Hauck-Andersen", "13. Agresti-Caffo", "16. Brown-Li"))
#'

  
# x1 <- 56; n1 <- 70; x2 <- 48; n2 <- 80
# xci <- binomDiffCI(x1, n1, x2, n2, method=eval(formals(binomDiffCI)$method))
# 
# alpha <- 0.05
# conf.level <- 0.95
# xci
# 


#' @export
binomDiffCI <- function(x1, n1, x2, n2, 
                        conf.level = 0.95, 
                        sides = c("two.sided","left","right"),
                        method = c(
                          "wald",
                          "wald-cc",
                          "agresti-caffo",
                          "exact",
                          "newcombe-score",
                          "newcombe-score-cc",
                          "miettinen-nurminen",
                          "mee-farrington-manning",
                          "brown-li-jeffreys",
                          "hauck-anderson",
                          "beal",
                          "haldane",
                          "jeffreys-perks"
                        )) {
  
  # old DescTools codes:
  # method <- switch(method,
  #                  "wald"     = "wald",
  #                  "waldcc"   = "wald-cc",
  #                  "ac"       = "agresti-caffo",
  #                  "exact"    = "exact"
  #                  "score"    = "newcombe-score",
  #                  "scorecc"  = "newcombe-score-cc",
  #                  "mn"       = "miettinen-nurminen",
  #                  "mee"      = "mee-farrington-manning",
  #                  "blj"      = "brown-li-jeffreys",
  #                  "ha"       = "hauck-anderson",
  #                  "beal"     = "beal"
  #                  "hal"      = "haldane",
  #                  "jp"       = "jeffreys-perks",
  #                  method
  # )
  
  
  sides <- match.arg(sides)
  
  if (missing(method)) {
    # if not provided take the first method instead of all (!)
    method <- eval(formals(sys.function())$method)[1]
    
  } else {
    # resolve methods cleanly, allowing an ".all" hidden option for method
    method <- .resolveMethod(method, several.ok = TRUE)
  }
  
  res <- .recycleApply(.binomDiffCI_engine,
                       x1=x1, n1=n1, 
                       x2=x2, n2=n2, 
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



.binomDiffCI_engine <- function(x1, n1, x2, n2, conf.level, sides, method){

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
  
  
    
  alpha <- 1 - conf.level
  if (sides != "two.sided")
    alpha <- alpha / 2
  
  CI <- switch(method,
               "wald" =                    { .bdci.wald(x1, n1, x2, n2, alpha, correct=FALSE) },
               "wald-cc" =                 { .bdci.wald(x1, n1, x2, n2, alpha, correct=TRUE) },
               "agresti-caffo" =           { .bdci.ac(x1, n1, x2, n2, alpha)  } ,     # Agresti-Caffo
               "exact" =                   { .bdci.exact(x1, n1, x2, n2, alpha) },    # exact
               "newcombe-score" =          { .bdci.score(x1, n1, x2, n2, alpha) },    # Newcombe
               "newcombe-score-cc" =       { .bdci.scorecc(x1, n1, x2, n2, alpha) },  # Newcombe
               "mee-farrington-manning" =  { .bdci.mee(x1, n1, x2, n2, alpha)  },     # Mee, also called Farrington-Mannig
               "brown-li-jeffreys" =       { .bdci.blj(x1, n1, x2, n2, alpha) },      # brown-li-jeffreys
               "hauck-anderson" =          { .bdci.ha(x1, n1, x2, n2, alpha) },       # Hauck-Anderson
               "miettinen-nurminen" =      { .bdci.mn(x1, n1, x2, n2, alpha)   },     # Miettinen-Nurminen
               "beal" =                    { .bdci.beal(x1, n1, x2, n2, alpha) },     # Beal
               "haldane" =                 { .bdci.hal(x1, n1, x2, n2, alpha) },      # haldane 
               "jeffreys-perks" =          { .bdci.jp(x1, n1, x2, n2, alpha) },       # jeffreys-perks
               stop(gettextf("Unknown method '%s'.", method))
  )
  
  # this is the default estimator used by the most (but not all) methods
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat

  # dot not return ci bounds outside [0,1]
  ci <- c( est = est, 
           lci = max(-1, CI["lci"]), 
           uci = min(1, CI["uci"]) )
  
  if(sides=="left")
    ci[3] <- 1
  else if(sides=="right")
    ci[2] <- -1
  
  return(ci)
  
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



.bdci.exact <- function(x1, n1, x2, n2, alpha) {
    
    # # observed difference
    # delta_hat <- x1/n1 - x2/n2
    # 
    # # grid for nuisance parameter optimisation
    # p2_grid <- seq(0, 1, length.out = 200)
    # 
    # # two-sided p-value under H0: p1 = p2 + delta
    # pval_fun <- function(delta) {
    #   
    #   max_p <- 0
    #   
    #   for (p2 in p2_grid) {
    #     
    #     p1 <- p2 + delta
    #     
    #     if (p1 < 0 || p1 > 1) next
    #     
    #     prob_obs <- dbinom(x1, n1, p1) * dbinom(x2, n2, p2)
    #     
    #     # enumerate all tables
    #     p_sum <- 0
    #     
    #     for (i in 0:n1) {
    #       for (j in 0:n2) {
    #         
    #         if ( (i/n1 - j/n2 - delta)^2 >=
    #              (x1/n1 - x2/n2 - delta)^2 ) {
    #           
    #           p_sum <- p_sum +
    #             dbinom(i, n1, p1) * dbinom(j, n2, p2)
    #         }
    #       }
    #     }
    #     
    #     max_p <- max(max_p, p_sum)
    #   }
    #   
    #   max_p
    # }
    # 
    # # lower bound
    # lower <- uniroot(function(d) pval_fun(d) - alpha,
    #                  lower = -1,
    #                  upper = delta_hat)$root
    # 
    # # upper bound
    # upper <- uniroot(function(d) pval_fun(d) - alpha,
    #                  lower = delta_hat,
    #                  upper = 1)$root
    # 
    # c(estimate = delta_hat,
    #   lci  = max(-1, lower),
    #   uci  = min(1, upper))
  
  # experimental code only...
  # bdci_exact_rcpp(x1, n1, x2, n2, alpha)
  return(lci=NA, uci=NA)
}



.bdci.score <- function(x1, n1, x2, n2, alpha) {
  # "score" or newcombe
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  z <- qnorm(1 - alpha/2)
  
  ci1 <- binomCI(x=x1, n=n1, conf.level=1-alpha, method="wilson")
  ci2 <- binomCI(x=x2, n=n2, conf.level=1-alpha, method="wilson")
  
  lci <- est - z * sqrt( ci1["lci"] * (1-ci1["lci"])/n1 + ci2["uci"] * (1-ci2["uci"])/n2)
  uci <- est + z * sqrt( ci1["uci"] * (1-ci1["uci"])/n1 + ci2["lci"] * (1-ci2["lci"])/n2)
  
  return( c( lci, uci) )
  
}


.bdci.scorecc <- function(x1, n1, x2, n2, alpha) {
  # "scorecc" or newcombe_cc 
  
  p1.hat <- x1/n1
  p2.hat <- x2/n2
  est <- p1.hat - p2.hat
  
  ci1 <- binomCI(x=x1, n=n1, conf.level=1-alpha, method="wilson-cc")
  ci2 <- binomCI(x=x2, n=n2, conf.level=1-alpha, method="wilson-cc")
  
  lci <- est - sqrt((p1.hat - ci1["lci"])^2 + (ci2["uci"] - p2.hat)^2) 
  uci <- est + sqrt((ci1["uci"] - p1.hat)^2 + (p2.hat - ci2["lci"])^2) 
  
  return( c( lci, uci) )
  
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



# .bdci.beal <- function(p1.hat, n1, p2.hat, n2, alpha, correct=FALSE) {
  # "beal" = {
  
  # experimental code only...
  # http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.633.9380&rep=rep1&type=pdf
  
  # a <- p1.hat + p2.hat
  # b <- p1.hat - p2.hat
  # u <- ((1/n1) + (1/n2)) / 4
  # v <- ((1/n1) - (1/n2)) / 4
  # V <- u*((2-a)*a - b^2) + 2*v*(1-a)*b
  # z <- qchisq(p=1-alpha/2, df = 1)
  # A <- sqrt(z*(V + z*u^2*(2-a)*a + z*v^2*(1-a)^2))
  # B <- (b + z*v*(1-a)) / (1+z*u)
  # 
  # CI.lower <- max(-1, B - A / (1 + z*u))
  # CI.upper <- min(1, B + A / (1 + z*u))
  
.bdci.beal <- function(x1, n1, x2, n2, alpha) {
    
    warning("Not yet thoroughly tested.")  
  
    z <- qnorm(1 - alpha/2)
    
    p1_hat <- x1 / n1
    p2_hat <- x2 / n2
    
    delta_hat <- p1_hat - p2_hat
    
    # Beal adjustment
    p1_tilde <- (x1 + 0.5) / (n1 + 1)
    p2_tilde <- (x2 + 0.5) / (n2 + 1)
    
    var_hat <-
      p1_tilde * (1 - p1_tilde) / n1 +
      p2_tilde * (1 - p2_tilde) / n2
    
    half_width <- z * sqrt(var_hat)
    
    lower <- delta_hat - half_width
    upper <- delta_hat + half_width
    
    c(
      est = delta_hat,
      lci = max(-1, lower),
      uci = min( 1, upper)
    )
    
}
  




