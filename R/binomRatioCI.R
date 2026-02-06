
#' Confidence Intervals for the Ratio of Binomial Proportions
#' 
#' A number of methods have been develeloped for obtaining confidence intervals
#' for the ratio of two binomial proportions. These include the Wald/Katz-log
#' method (Katz et al. 1978), adjusted-log (Walter 1975, Pettigrew et al.
#' 1986), Koopman asymptotic score (Koopman 1984), Inverse hyperbolic sine
#' transformation (Newman 2001), the Bailey method (Bailey (1987), and the
#' Noether (1957) procedure. Koopman results are found iteratively for most
#' intervals using root finding.
#' 
#' All arguments are being recycled.
#' 
#' Let \eqn{Y_1} and \eqn{Y_2} be multinomial random variables with parameters
#' \eqn{n_1, \pi_{1i}}, and \eqn{n_2, \pi_{2i}}, respectively; where \eqn{i =
#' \{1, 2, 3, \dots, r\}}.  This encompasses the binomial case in which \eqn{r
#' = 1}. We define the true selection ratio for the \emph{i}th resource of
#' \emph{r} total resources to be: \deqn{\theta_{i}=\frac{\pi _{1i}}{\pi
#' _{2i}}}
#' 
#' where \eqn{\pi_{1i}} and \eqn{\pi_{2i}} represent the proportional use and
#' availability of the \emph{i}th resource, respectively. Note that if \eqn{r =
#' 1} the selection ratio becomes relative risk.  The maximum likelihood
#' estimators for \eqn{\pi_{1i}} and \eqn{\pi_{2i}} are the sample proportions:
#' 
#' \deqn{{{\hat{\pi }}_{1i}}=\frac{{{y}_{1i}}}{{{n}_{1}}},} and
#' \deqn{{{\hat{\pi }}_{2i}}=\frac{{{y}_{2i}}}{{{n}_{2}}}}
#' 
#' where \eqn{y_{1i}} and \eqn{y_{2i}} are the observed counts for use and
#' availability for the \emph{i}th resource.  The estimator for \eqn{\theta_i}
#' is:
#' 
#' \deqn{\hat{\theta}_{i}=\frac{\hat{\pi}_{1i}}{\hat{\pi }_{2i}}.}
#' 
#' \tabular{ll}{ Method \tab Algorithm \cr \tab \cr
#' 
#' % Katz-log Katz-log \tab \eqn{\hat\theta_i\times} exp\eqn{(\pm
#' z_1-\alpha/2\hat{\sigma}_W)}, \cr \tab where
#' \eqn{\hat\sigma_W^2=\frac{(1-\hat{\pi}
#' _{1i})}{\hat{\pi}_{1i}n_1}+\frac{(1-\hat{\pi}_{2i})}{\hat{\pi}_{2i}n_2}}.
#' \cr \tab \cr
#' 
#' % Adjusted log Adjusted-log \tab \eqn{\hat{\theta}_{Ai}\times} exp\eqn{(\pm
#' z_1-\alpha /2\hat{\sigma}_A)}, \cr \tab where
#' \eqn{\hat{\theta}_{Ai}=\frac{y_{1i}+0.5/n_1+0.5}{y_{2i}+0.5/n_2+0.5}}, \cr
#' \tab
#' \eqn{\hat{\sigma}_A^2=\frac{1}{y_1+0.5}-\frac{1}{n_1+0.5}+\frac{1}{y_2+0.5}-\frac{1}{n_2+0.5}}.
#' \cr \tab \cr
#' 
#' % Bailey Bailey \tab \eqn{\hat{\theta} _i\left[\frac{1\pm z_1-\left( \alpha
#' /2 \right)\left(
#' \hat{\pi}_{1i}'/y_{1i}+\hat{\pi}_{2i}'/y_{2i}-z_1-\left(\alpha/2
#' \right)^2\hat{\pi} _{1i}'\hat{\pi}_{2i}'/9y_{1i}y_{2i}
#' \right)^{1/2}/3}{1-z_{1-\left(\alpha/2 \right)^2}\hat{\pi} _{2i}'/9y_{2i}}
#' \right]^3},\cr \tab where \eqn{\hat{\pi_{1i}}'} = 1 - \eqn{\hat{\pi}_{1i}},
#' and \eqn{\hat{\pi}_{2i}'} = 1 - \eqn{\hat{\pi}_{2i}}.\cr \tab \cr
#' 
#' % Inv sin Inv. hyperbolic sine \tab \eqn{\ln({{\hat{\theta }}_{i}})\pm
#' \left[ 2sin{{h}^{-1}}\left( \frac{{{z}_{(1-\alpha
#' /2)}}}{2}\sqrt{\frac{1}{{{y}_{1i}}}-\frac{1}{{{n}_{1}}}+\frac{1}{{{y}_{2i}}}-\frac{1}{{{n}_{2}}}}
#' \right) \right]}, \cr \tab\cr
#' 
#' % Koopman Koopman \tab Find \eqn{X^2(\theta_0)} = \eqn{\chi _1^2(1 -
#' \alpha)}, where \cr \tab \eqn{{{\tilde{\pi }}_{1i}}=\frac{{{\theta
#' }_{0}}({{n}_{1}}+{{y}_{2i}})+{{y}_{1i}}+{{n}_{2}}-{{[{{\{{{\theta
#' }_{0}}({{n}_{1}}+{{y}_{2i}})+{{y}_{1i}}+ {{n}_{2}}\}}^{2}}-4{{\theta
#' }_{0}}({{n}_{1}}+{{n}_{2}})({{y}_{1i}}+{{y}_{2i}})]}^{0.5}}}{2({{n}_{1}}+{{n}_{2}})}},
#' \cr \tab \eqn{{{\tilde{\pi }}_{2i}}=\frac{{{{\tilde{\pi }}}_{1i}}}{{{\theta
#' }_{0}}}, and {{X}^{2}}({{\theta}_{0}})=\frac{{{\left(
#' {{y}_{1i}}-{{n}_{1}}{{{\tilde{\pi }}}_{1i}} \right)}^{2}}}
#' {{{n}_{1}}{{{\tilde{\pi }}}_{1i}}(1-{{{\tilde{\pi }}}_{1i}})}\left\{
#' 1+\frac{{{n}_{1}}({{\theta}_{0}}-{{{\tilde{\pi
#' }}}_{1i}})}{{{n}_{2}}(1-{\tilde{\pi}_{1i}})} \right\}}. \cr \tab \cr %%
#' Noether Noether \tab \eqn{\hat{\theta}_i\pm z_1-\alpha/2\hat{\sigma}_N}, \cr
#' \tab where \eqn{\hat{\sigma }_{N}^{2}=\hat{\theta }_{i}^{2}\left(
#' \frac{1}{{{y}_{1i}}}-\frac{1}{{{n}_{1}}}+\frac{1}{{{y}_{2i}}}-\frac{1}{{{n}_{2}}}
#' \right)}.  }
#' 
#' Exception handling strategies are generally necessary in the cases \eqn{x_1}
#' = 0, \eqn{n_1} = \eqn{x_1}, \eqn{x_2} = 0, and \eqn{n_2} = \eqn{x_2} (see
#' Aho and Bowyer, in review).
#' 
#' The bootstrap method currently employs percentile confidence intervals.
#' 
#' @param x1 number of successes for the ratio numerator.
#' @param n1 number of trials for the ratio numerator.
#' @param x2 number of successes for the ratio denominator.
#' @param n2 number of successes for the ratio denominator.
#' @param conf.level confidence level, defaults to 0.95.
#' @param sides a character string specifying the side of the confidence
#' interval, must be one of \code{"two.sided"} (default), \code{"left"} or
#' \code{"right"}. You can specify just the initial letter. \code{"left"} would
#' be analogue to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method confidence interval method, one of \code{"katz.log"}
#' (default), \code{"adj.log"}, \code{"bailey"}, \code{"boot"},
#' \code{"koopman"}, \code{"noether"} or \code{"sinh-1"}. Can be abbreviated.
#' @param tol The desired accuracy (convergence tolerance) for the iterative
#' root finding procedure when finding Koopman intevals. The default is taken
#' to be the smallest positive floating-point number of the workstation
#' implementing the function, raised to the 0.25 power, and will normally be
#' approximately 0.0001.
#' @param R If method \code{"boot"} is chosen, the number of bootstrap
#' iterations.
#' @return A matrix with 3 columns containing the estimate, the lower and the
#' upper confidence intervall.
#' @author Ken Aho <kenaho1@@gmail.com>, some tweaks Andri Signorell
#' <andri@@signorell.net>
#' @seealso \code{\link{binomCI}, \link{binomDiffCI}}
#' @references Agresti, A., Min, Y. (2001) On small-sample confidence intervals
#' for parameters in discrete distributions.  \emph{Biometrics} 57: 963-97.
#' 
#' Aho, K., and Bowyer, T. (In review) Confidence intervals for ratios of
#' multinomial proportions: implications for selection ratios. \emph{Methods in
#' Ecology and Evolution}.
#' 
#' Bailey, B.J.R. (1987) Confidence limits to the risk ratio.
#' \emph{Biometrics} 43(1): 201-205.
#' 
#' Katz, D., Baptista, J., Azen, S. P., and Pike, M. C. (1978) Obtaining
#' confidence intervals for the risk ratio in cohort studies. \emph{Biometrics}
#' 34: 469-474
#' 
#' Koopman, P. A. R. (1984) Confidence intervals for the ratio of two binomial
#' proportions. \emph{Biometrics} 40:513-517.
#' 
#' Manly, B. F., McDonald, L. L., Thomas, D. L., McDonald, T. L. and Erickson,
#' W.P. (2002) \emph{Resource Selection by Animals: Statistical Design and
#' Analysis for Field Studies.  2nd edn.} Kluwer, New York, NY
#' 
#' Newcombe, R. G. (2001) Logit confidence intervals and the inverse sinh
#' transformation.  \emph{The American Statistician} 55: 200-202.
#' 
#' Pettigrew H. M., Gart, J. J., Thomas, D. G. (1986) The bias and higher
#' cumulants of the logarithm of a binomial variate.  \emph{Biometrika} 73(2):
#' 425-435.
#' 
#' Walter, S. D. (1975) The distribution of Levins measure of attributable
#' risk. \emph{Biometrika} 62(2): 371-374.
#' @examples
#' 
#' # From Koopman (1984)
#' 
#' binomRatioCI(x1 = 36, n1 = 40, x2 = 16, n2 = 80, method = "katz")
#' binomRatioCI(x1 = 36, n1 = 40, x2 = 16, n2 = 80, method = "koop")
#' 


#' @export
binomRatioCI <- function(x1, n1, x2, n2, conf.level = 0.95, sides = c("two.sided","left","right"),
                         method =c("katz.log","adj.log","bailey","koopman","noether","sinh-1","boot"),
                         tol = .Machine$double.eps^0.25, R = 1000) {
  
  # source: asbio::ci.prat by Ken Aho <kenaho1 at gmail.com>
  
  
  ibinomRatioCI <- function(x, m, y, n, conf, sides, method) {
    
    if((x > m)|(y > n)) stop("Use correct parameterization for x1, x2, n1, and n2")
    
    method <- match.arg(method, c("katz.log","adj.log","bailey","koopman","noether","sinh-1","boot"))
    
    if(sides!="two.sided")
      conf <- 1 - 2*(1-conf)
    
    alpha <- 1 - conf
    
    z.star <- qnorm(1 - (1 - conf)/2)
    x2 <- qchisq(conf, 1)
    
    #-------------------------- Adj-log ------------------------------#
    
    if(method == "adj.log"){
      if((x == m & y == n)){
        rat <- (x/m)/(y/n) 
        x <- m - 0.5
        y <- n - 0.5
        nrat <- ((x+0.5)/(m+0.5))/((y+0.5)/(n+0.5))
        varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
        
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
        CIU <- nrat * exp(z.star * sqrt(varhat))
        
      } else if(x == 0 & y == 0){
        
        CIL = 0 
        CIU = Inf 
        rat = 0 
        varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
        
      } else {
        
        rat <- (x/m)/(y/n) 
        nrat <- ((x+0.5)/(m+0.5))/((y+0.5)/(n+0.5))
        varhat <- (1/(x+0.5)) - (1/(m+0.5)) + (1/(y+0.5)) - (1/(n+0.5))
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
        CIU <- nrat * exp(z.star * sqrt(varhat))
      }
      
      CI <- c(rat, CIL, CIU)
      
    }
    
    #-------------------------------Bailey-----------------------------#
    
    if(method == "bailey"){
      rat <- (x/m)/(y/n)
      varhat <- ifelse((x == m) & (y == n),(1/(m-0.5)) - (1/(m)) + (1/(n-0.5)) - (1/(n)),(1/(x)) - (1/(m)) + (1/(y)) - (1/(n)))
      
      p.hat1 <- x/m; p.hat2 <- y/n;
      q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
      
      if(x == 0 | y == 0){
        xn <- ifelse(x == 0, 0.5, x)
        yn <- ifelse(y == 0, 0.5, y)
        nrat <- (xn/m)/(yn/n)
        p.hat1 <- xn/m; p.hat2 <- yn/n;
        q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        if(xn == m | yn == n){
          xn <- ifelse(xn == m, m - 0.5, xn)
          yn <- ifelse(yn == n, n - 0.5, yn)
          nrat <- (xn/m)/(yn/n)
          p.hat1 <- xn/m; p.hat2 <- yn/n;
          q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        }
      }
      
      if(x == 0 | y == 0){
        if(x == 0 & y == 0){
          rat <- Inf
          CIL <- 0
          CIU <- Inf
        }
        if(x == 0 & y != 0){
          CIL <- 0
          CIU <- nrat * ((1+ z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        }
        if(y == 0 & x != 0){
          CIU = Inf
          CIL <- nrat * ((1- z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        }
      }else if(x == m | y == n){
        xn <- ifelse(x == m, m - 0.5, x)
        yn <- ifelse(y == n, n - 0.5, y)
        nrat <- (xn/m)/(yn/n)
        p.hat1 <- xn/m; p.hat2 <- yn/n;
        q.hat1 <- 1 - p.hat1; q.hat2 <- 1 - p.hat2
        CIL <- nrat * ((1- z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
        CIU <- nrat * ((1+ z.star * sqrt((q.hat1/xn) + (q.hat2/yn) - (z.star^2 * q.hat1 * q.hat2)/(9 * xn * yn))/3)/((1 - (z.star^2 * q.hat2)/(9 * yn))))^3
      }else{
        CIL <- rat * ((1- z.star * sqrt((q.hat1/x) + (q.hat2/y) - (z.star^2 * q.hat1 * q.hat2)/(9 * x * y))/3)/((1 - (z.star^2 * q.hat2)/(9 * y))))^3
        CIU <- rat * ((1+ z.star * sqrt((q.hat1/x) + (q.hat2/y) - (z.star^2 * q.hat1 * q.hat2)/(9 * x * y))/3)/((1 - (z.star^2 * q.hat2)/(9 * y))))^3
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Boot ------------------------------#
    
    if(method == "boot"){
      rat <- (x/m)/(y/n)
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {CIL <- 0;  rat <- (x/m)/(y/n); x <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIU <- nrat * exp(z.star * sqrt(varhat))}
        if(x != 0 & y == 0) {CIU <- Inf;  rat <- (x/m)/(y/n); y <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))}
      } else{
        num.data <- c(rep(1, x), rep(0, m - x))
        den.data <- c(rep(1, y), rep(0, n - y))
        nd <- matrix(ncol = R, nrow = m)
        dd <- matrix(ncol = R, nrow = n)
        brat <- 1:R
        for(i in 1L:R){
          nd[,i] <- sample(num.data, m, replace = TRUE)
          dd[,i] <- sample(den.data, n, replace = TRUE)
          brat[i] <- (sum(nd[,i])/m)/(sum(dd[,i])/n)
        }
        alpha <- 1 - conf
        CIU <- quantile(brat, 1 - alpha/2, na.rm = TRUE)
        CIL <- quantile(brat, alpha/2, na.rm = TRUE)
        varhat <- var(brat)
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Katz-log ------------------------------#
    
    if(method == "katz.log"){
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {CIL <- 0;  rat <- (x/m)/(y/n); x <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIU <- nrat * exp(z.star * sqrt(varhat))}
        if(x != 0 & y == 0) {CIU <- Inf;  rat <- (x/m)/(y/n); y <- 0.5; nrat <- (x/m)/(y/n)
        varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        CIL <- nrat * exp(-1 * z.star * sqrt(varhat))}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n); CIL <- nrat * exp(-1 * z.star * sqrt(varhat))
          x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n); CIU <- nrat * exp(z.star * sqrt(varhat))
        }
      } else
      {rat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n)
      CIL <- rat * exp(-1 * z.star * sqrt(varhat))
      CIU <- rat * exp(z.star * sqrt(varhat))}
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Koopman ------------------------------#
    
    if(method == "koopman"){
      
      if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA
      } else {
        
        a1 = n * (n * (n + m) * x + m * (n + x) * (z.star^2))
        a2 = -n * (n * m * (y + x) + 2 * (n + m) * y *
                     x + m * (n + y + 2 * x) * (z.star^2))
        a3 = 2 * n * m * y * (y + x) + (n + m) * (y^2) *
          x + n * m * (y + x) * (z.star^2)
        a4 = -m * (y^2) * (y + x)
        b1 = a2/a1; b2 = a3/a1; b3 = a4/a1
        c1 = b2 - (b1^2)/3;  c2 = b3 - b1 * b2/3 + 2 * (b1^3)/27
        ceta = suppressWarnings(acos(sqrt(27) * c2/(2 * c1 * sqrt(-c1))))
        t1 <- suppressWarnings(-2 * sqrt(-c1/3) * cos(pi/3 - ceta/3))
        t2 <- suppressWarnings(-2 * sqrt(-c1/3) * cos(pi/3 + ceta/3))
        t3 <- suppressWarnings(2 * sqrt(-c1/3) * cos(ceta/3))
        p01 = t1 - b1/3; p02 = t2 - b1/3; p03 = t3 - b1/3
        p0sum = p01 + p02 + p03; p0up = min(p01, p02, p03); p0low = p0sum - p0up - max(p01, p02, p03)
        
        
        U <- function(a){
          p.hatf <- function(a){
            (a * (m + y) + x + n - ((a * (m + y) + x + n)^2 - 4 * a * (m + n) * (x + y))^0.5)/(2 * (m + n))
          }
          p.hat <- p.hatf(a)
          (((x - m * p.hat)^2)/(m * p.hat * (1 - p.hat)))*(1 + (m * (a - p.hat))/(n * (1 - p.hat))) - x2
        }
        
        rat <- (x/m)/(y/n); nrat <- (x/m)/(y/n); varhat <- (1/x) - (1/m) + (1/y) - (1/n)
        if((x == 0) & (y != 0)) {nrat <- ((x + 0.5)/m)/(y/n); varhat <- (1/(x + 0.5)) - (1/m) + (1/y) - (1/n)}
        if((y == 0) & (x != 0)) {nrat <- (x/m)/((y + 0.5)/n); varhat <- (1/x) - (1/m) + (1/(y + 0.5)) - (1/n)}
        if((y == n) & (x == m)) {nrat <- 1; varhat <- (1/(m - 0.5)) - (1/m) + 1/(n - 0.5) - (1/n)}
        
        La <- nrat * exp(-1 * z.star * sqrt(varhat)) * 1/4
        Lb <- nrat
        Ha <- nrat
        Hb <- nrat * exp(z.star * sqrt(varhat)) * 4
        
        #----------------------------------------------------------------------------#
        
        if((x != 0) & (y == 0)) {
          if(x == m){
            CIL = (1 - (m - x) * (1 - p0low)/(y + m - (n + m) * p0low))/p0low
            CIU <- Inf
          }
          else{
            CIL <- uniroot(U, c(La, Lb), tol=tol)$root
            CIU <- Inf
          }
        }
        
        #------------------------------------------------------------#
        
        if((x == 0) & (y != n)) {
          CIU <- uniroot(U, c(Ha, Hb), tol=tol)$root
          CIL <- 0
        }
        
        #------------------------------------------------------------#
        
        if(((x == m)|(y == n)) & (y != 0)){
          
          
          if((x == m)&(y == n)){
            U.0 <- function(a){if(a <= 1) {m * (1 - a)/a - x2}
              else{(n * (a - 1)) - x2}
            }
            CIL <- uniroot(U.0, c(La, rat), tol = tol)$root
            CIU <- uniroot(U.0, c(rat, Hb), tol = tol)$root
          }
          
          #------------------------------------------------------------#
          
          if((x == m) & (y != n)){
            
            phat1 = x/m; phat2 = y/n
            phihat = phat2/phat1
            phiu = 1.1 * phihat
            r = 0
            while (r >= -z.star) {
              a = (m + n) * phiu
              b = -((x + n) * phiu + y + m)
              c = x + y
              p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
              p2hat = p1hat * phiu
              q2hat = 1 - p2hat
              var = (m * n * p2hat)/(n * (phiu - p2hat) +
                                       m * q2hat)
              r = ((y - n * p2hat)/q2hat)/sqrt(var)
              phiu1 = phiu
              phiu = 1.0001 * phiu1
            }
            CIU = (1 - (m - x) * (1 - p0up)/(y + m - (n + m) * p0up))/p0up
            CIL = 1/phiu1
          }
          
          #------------------------------------------------------------#
          
          if((y == n) & (x != m)){
            p.hat2 = y/n; p.hat1 = x/m; phihat = p.hat1/p.hat2
            phil = 0.95 * phihat; r = 0
            if(x != 0){
              while(r <= z.star) {
                a = (n + m) * phil
                b = -((y + m) * phil + x + n)
                c = y + x
                p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
                p2hat = p1hat * phil
                q2hat = 1 - p2hat
                var = (n * m * p2hat)/(m * (phil - p2hat) +
                                         n * q2hat)
                r = ((x - m * p2hat)/q2hat)/sqrt(var)
                CIL = phil
                phil = CIL/1.0001
              }
            }
            
            phiu = 1.1 * phihat
            
            if(x == 0){CIL = 0; phiu <- ifelse(n < 100, 0.01, 0.001)}
            
            r = 0
            while(r >= -z.star) {
              a = (n + m) * phiu
              b = -((y + m) * phiu + x  + n)
              c = y + x
              p1hat = (-b - sqrt(b^2 - 4 * a * c))/(2 * a)
              p2hat = p1hat * phiu
              q2hat = 1 - p2hat
              var = (n * m * p2hat)/(m * (phiu - p2hat) +
                                       n * q2hat)
              r = ((x  - m * p2hat)/q2hat)/sqrt(var)
              phiu1 = phiu
              phiu = 1.0001 * phiu1
            }
            CIU <- phiu1
          }
        } else if((y != n) & (x != m) & (x != 0) & (y != 0)){
          CIL <- uniroot(U, c(La, Lb), tol=tol)$root
          CIU <- uniroot(U, c(Ha, Hb), tol=tol)$root
        }
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #-------------------------- Noether ------------------------------#
    
    if(method == "noether"){
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; se.hat <- NA; varhat = NA}
        if(x == 0 & y != 0) {rat <- (x/m)/(y/n); CIL <- 0;  x <- 0.5
        nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIU <- nrat + z.star * se.hat}
        if(x != 0 & y == 0) {rat <- Inf; CIU <- Inf;  y <- 0.5
        nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIL <- nrat - z.star * se.hat}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); se.hat <- nrat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
          CIU <- nrat + z.star * se.hat
          CIL <- nrat - z.star * se.hat
        }
      } else
      {
        rat <- (x/m)/(y/n)
        se.hat <- rat * sqrt((1/x) - (1/m) + (1/y) - (1/n))
        CIL <- rat - z.star * se.hat
        CIU <- rat + z.star * se.hat
      }
      varhat <- ifelse(is.na(se.hat), NA, se.hat^2)
      CI <- c(rat, max(0,CIL), CIU)
    }
    
    #------------------------- sinh-1 -----------------------------#
    
    if(method == "sinh-1"){
      
      if((x == 0 & y == 0)|(x == 0 & y != 0)|(x != 0 & y == 0)|(x == m & y == n)){
        if(x == 0 & y == 0) {CIL <- 0;  CIU <- Inf; rat = 0; varhat = NA}
        if(x == 0 & y != 0) {rat <- (x/m)/(y/n); CIL <- 0;  x <- z.star
        nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
        CIU <- exp(log(nrat) + varhat)}
        if(x != 0 & y == 0) {rat = Inf; CIU <- Inf;  y <- z.star
        nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
        CIL <- exp(log(nrat) - varhat)}
        if(x == m & y == n) {
          rat <- (x/m)/(y/n); x <- m - 0.5; y <- n - 0.5; nrat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
          CIL <- exp(log(nrat) - varhat)
          CIU <- exp(log(nrat) + varhat)
        }
      } else
      {rat <- (x/m)/(y/n); varhat <- 2 * asinh((z.star/2)*sqrt(1/x + 1/y - 1/m - 1/n))
      CIL <- exp(log(rat) - varhat)
      CIU <- exp(log(rat) + varhat)
      }
      CI <- c(rat, CIL, CIU)
    }
    
    #------------------------Results ------------------------------#
    
    # res <- list(CI = CI, varhat = varhat)
    CI
    
  }
  
  
  
  if(missing(sides))    sides <- match.arg(sides)
  if(missing(method))   method <- match.arg(method)
  
  # Recycle arguments
  lst <- recycle(x1=x1, n1=n1, x2=x2, n2=n2, conf.level=conf.level, sides=sides, method=method)
  
  # ibinomRatioCI <- function(x, m, y, n, conf, method){
  
  res <- t(sapply(1:attr(lst, "maxdim"),
                  function(i) ibinomRatioCI(x=lst$x1[i], m=lst$n1[i], y=lst$x2[i], n=lst$n2[i],
                                            conf=lst$conf.level[i],
                                            sides=lst$sides[i],
                                            method=lst$method[i])))
  
  # get rownames
  lgn <- recycle(x1=if(is.null(names(x1))) paste("x1", seq_along(x1), sep=".") else names(x1),
                 n1=if(is.null(names(n1))) paste("n1", seq_along(n1), sep=".") else names(n1),
                 x2=if(is.null(names(x2))) paste("x2", seq_along(x2), sep=".") else names(x2),
                 n2=if(is.null(names(n2))) paste("n2", seq_along(n2), sep=".") else names(n2),
                 conf.level=conf.level, sides=sides, method=method)
  
  xn <- apply(as.data.frame(lgn[sapply(lgn, function(x) length(unique(x)) != 1)]), 1, paste, collapse=":")
  
  return(setNamesX(res, 
                  rownames=xn, 
                  colnames=c("est", "lci", "uci")))
  
}



