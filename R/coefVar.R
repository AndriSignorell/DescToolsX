
#' Coefficient of Variation 
#' 
#' Calculates the coefficient of variation and its confidence limits using
#' various methods. 
#' 
#' In order for the coefficient of variation to be an unbiased estimate of the
#' true population value, the coefficient of variation is corrected as: \deqn{
#' CV_{korr} = CV \cdot \left( 1 - \frac{1}{4\cdot(n-1)} + \frac{1}{n} \cdot
#' CV^2 + \frac{1}{2 \cdot (n-1)^2} \right) }
#' 
#' For determining\verb{ }\bold{the confidence intervals}\verb{ } for the
#' coefficient of variation a number of methods have been proposed.
#' \code{coefVarCI()} currently supports five different methods. The details
#' for the methods are given in the specific references.
#' 
#' The \bold{"naive" method} \verb{ } is based on dividing the standard
#' confidence limit for the standard deviation by the sample mean.
#' 
#' \bold{McKay's} \verb{ } approximation is asymptotically exact as n goes to
#' infinity. McKay recommends this approximation only if the coefficient of
#' variation is less than 0.33. Note that if the coefficient of variation is
#' greater than 0.33, either the normality of the data is suspect or the
#' probability of negative values in the data is non-negligible. In this case,
#' McKay's approximation may not be valid. Also, it is generally recommended
#' that the sample size should be at least 10 before using McKay's
#' approximation.
#' 
#' \bold{Vangel's modified McKay method} \verb{ } is more accurate than the
#' McKay method in most cases, particularly for small samples. According to Vangel,
#' the unmodified McKay is only more accurate when both the coefficient of
#' variation and alpha are large. However, if the coefficient of variation is
#' large, then this implies either that the data contains negative values or
#' the data does not follow a normal distribution. In this case, neither the
#' McKay or the modified McKay should be used. In general, the Vangel's
#' modified McKay method is recommended over the McKay method. It generally
#' provides good approximations as long as the data is approximately normal and
#' the coefficient of variation is less than 0.33.
#' 
#' See also:
#' https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/coefvacl.htm
#' 
#' \bold{nct} \verb{ }uses the noncentral t-distribution to calculate the
#' confidence intervals. See Smithson (2003).
#' 
#' \bold{Note:}\verb{ } Analytic (precision) weights are not supported. For
#' likelihood-based weighted variance estimation, see
#' \code{\link[stats]{cov.wt}}.
#' 
#' @aliases coefVar coefVar.lm coefVar.aov coefVar.default coefVarCI
#' 
#' @param x a non-empty numeric vector of data values
#' @param weights a numeric vector of weights the same length as \code{x}
#' giving the weights to use for elements of \code{x}
#' @param unbiased logical; whether to apply a bias correction. See Details.
#' Defaults to \code{FALSE}.
#' @param conf.level confidence level of the interval; defaults to 0.95
#' @param sides a character string specifying the side of the confidence
#' interval. Must be one of \code{"two.sided"} (default), \code{"left"}, or
#' \code{"right"}. Partial matching is supported. \code{"left"} is analogous
#' to a hypothesis of \code{"greater"} in a \code{t.test}.
#' @param method character string specifying the confidence interval method:
#' \code{"nct"} (default),
#' \code{"vangel"}, \code{"mckay"}, \code{"verrill"} (currently not yet
#' implemented), or \code{"naive"}. Partial matching is supported. See
#' Details.
#' @param na.rm logical. Should missing values be removed? Defaults to
#' \code{FALSE}. 
#' @param \dots further arguments
#' @return an unnamed numeric scalar containing the coefficient of variation
#' for \code{coefVar()}. If recycling in \code{coefVarCI()} yields a
#' single case, it returns a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the coefficient of variation.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#' Otherwise, \code{coefVarCI()} returns a numeric matrix with one row per case
#' and the columns \code{est}, \code{lci}, and \code{uci}.
#' 
#' @note
#' Parts of the code contributed by Michael Smithson.
#'  
#' @seealso \code{\link{meanX}}, \code{\link{sdX}}, (both supporting weights) 
#' 
#' @references McKay, A. T. (1932). Distribution of the coefficient of
#' variation and the extended \emph{t} distribution, \emph{Journal of the Royal
#' Statistical Society}, \emph{95}, 695--698.
#' 
#' Johnson, B. L., Welch, B. L. (1940). Applications of the non-central
#' t-distribution. \emph{Biometrika}, 31, 362--389.
#' 
#' Mark Vangel (1996) Confidence Intervals for a Normal Coefficient of
#' Variation, \emph{American Statistician}, Vol. 15, No. 1, pp. 21-26.
#' 
#' Kelley, K. (2007). Sample size planning for the coefficient of variation from
#' the accuracy in parameter estimation approach. \emph{Behavior Research
#' Methods, 39} (4), 755-766
#' 
#' Kelley, K. (2007). Constructing confidence intervals for standardized effect
#' sizes: Theory, application, and implementation. \emph{Journal of Statistical
#' Software, 20} (8), 1-24
#' 
#' Smithson, M.J. (2003) \emph{Confidence Intervals, Quantitative Applications
#' in the Social Sciences Series}, No. 140. Thousand Oaks, CA: Sage. pp. 39-41
#' 
#' Steve Verrill (2003) Confidence Bounds for Normal and Lognormal Distribution
#' Coefficients of Variation, \emph{Research Paper 609}, USDA Forest Products
#' Laboratory, Madison, Wisconsin.
#' 
#' Verrill, S. and Johnson, R.A. (2007) Confidence Bounds and Hypothesis Tests
#' for Normal Distribution Coefficients of Variation, \emph{Communications in
#' Statistics Theory and Methods}, Volume 36, No. 12, pp 2187-2206.
#' 
#' 
#' @examples
#' 
#' set.seed(15)
#' x <- runif(100)
#' coefVar(x)
#' coefVarCI(x)
#' 
#' # Coefficient of variation for a linear model
#' r.lm <- lm(Fertility ~ ., swiss)
#' coefVar(r.lm)
#' 
#' # the function is vectorized, so arguments are recycled
#' fn <- "https://www.itl.nist.gov/div898/handbook/datasets/ZARR13.DAT"
#' zarr <- as.numeric(readr::read_lines(fn, skip=25))
#' 
#' round(coefVarCI(zarr, method="vangel", 
#'           sides="two.sided", conf.level = c(.5,.8,.9,.95,.99,.999)), 5)
#'           
#' # https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/coefvacl.htm
#' #
#' #  Two-Sided Confidence Limits for the Coefficient of Variation
#' #                 for Normally Distributed Data
#' # 
#' # ---------------------------------------------------------
#' #  Confidence    Coefficient          Lower          Upper
#' #   Value (%)   of Variation          Limit          Limit
#' # ---------------------------------------------------------
#' #        50.0        0.00246        0.00238        0.00255
#' #        80.0        0.00246        0.00231        0.00263
#' #        90.0        0.00246        0.00227        0.00269
#' #        95.0        0.00246        0.00224        0.00273
#' #        99.0        0.00246        0.00217        0.00283
#' #        99.9        0.00246        0.00210        0.00294
#' 
#' 
#' 



#' @rdname coefVar

#' @family dispersion  
#' @concept dispersion
#'
#'
#' @export
coefVar <- function (x, ...) {
  UseMethod("coefVar")
}


#' @rdname coefVar
#' @export
coefVar.default <- function (x, weights = NULL, unbiased = FALSE, 
                             na.rm = FALSE, ...) {
  
  
  if (is.null(weights)) {
    if (na.rm) 
      x <- na.omit(x)
    res <- sdX(x)/(m <- meanX(x))
    n <- length(x)
    
  } else {
    res <- sdX(x, weights = weights) / (m <- meanX(x, weights = weights))
    n <- sum(weights)
  }
  
  if (abs(m) < 0.1)
    warning("Mean close to zero - CV may be unstable,\n  nCI is based on noncentral t inversion and may not contain the plug-in estimate")
  
  if (unbiased) {
    res <- res * ((1 - (1/(4 * (n - 1))) + (1/n) * res^2) + (1/(2 * (n - 1)^2)))
  }
  
  return(res)
  
}




#' @rdname coefVar
#' @export
coefVar.lm <- function (x, unbiased = FALSE, na.rm = FALSE, ...) {
  
  # source:  http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm
  
  # In the modeling setting, the CV is calculated as the ratio of the root mean squared error (RMSE)
  # to the mean of the dependent variable.
  
  # root mean squared error
  rmse <- sqrt(sum(x$residuals^2) / x$df.residual)
  res <- rmse / mean(x$model[[1]], na.rm=na.rm)
  
  # This is the same approach as in coefVar.default, but it's not clear
  # if it is correct in the environment of a model
  n <- x$df.residual
  if (unbiased) {
    res <- res * ((1 - (1/(4 * (n - 1))) + (1/n) * res^2) +
                    (1/(2 * (n - 1)^2)))
  }
  
  # if (!is.na(conf.level)) {
  #   ci <- .nctCI(sqrt(n)/res, df = n - 1, conf = conf.level)
  #   res <- c(est = res, low.ci = unname(sqrt(n)/ci["upr.ci"]),
  #            upr.ci = unname(sqrt(n)/ci["lwr.ci"]))
  # }
  
  return(res)
  
}


# aus agricolae: Variations Koeffizient aus aov objekt
#
# coefVar.aov <- function(x){
#   return(sqrt(sum(x$residual^2) / x$df.residual) / mean(x$fitted.values))
# }


#' @rdname coefVar
#' @export
coefVar.aov <- function (x, unbiased = FALSE, na.rm = FALSE, ...) {
  
  # source:  http://www.ats.ucla.edu/stat/mult_pkg/faq/general/coefficient_of_variation.htm
  
  # In the modeling setting, the CV is calculated as the ratio of the root mean squared error (RMSE)
  # to the mean of the dependent variable.
  
  # root mean squared error
  rmse <- sqrt(sum(x$residuals^2) / x$df.residual)
  res <- rmse / mean(x$model[[1]], na.rm=na.rm)
  
  # This is the same approach as in coefVar.default, but it's not clear
  # if it is correct in the enviroment of a model
  n <- x$df.residual
  if (unbiased) {
    res <- res * ((1 - (1/(4 * (n - 1))) + (1/n) * res^2) +
                    (1/(2 * (n - 1)^2)))
  }
  
  # if (!is.na(conf.level)) {
  #   ci <- .nctCI(sqrt(n)/res, df = n - 1, conf = conf.level)
  #   res <- c(est = res, low.ci = unname(sqrt(n)/ci["upr.ci"]),
  #            upr.ci = unname(sqrt(n)/ci["lwr.ci"]))
  # }
  
  return(res)
  
}



# ============================================
#
#    Confidence intervals
#
# ============================================


#' @rdname coefVar
#' @export
coefVarCI <- function (x, conf.level = 0.95, 
                       sides = c("two.sided", "left", "right"), 
                       method = c("nct","vangel","mckay","verrill","naive"), 
                       na.rm=FALSE, ... ) {
  ## NA-Handling
  if (na.rm) {
    ok <- !is.na(x)
    x <- x[ok]
  }
  
  res <- .coefVarCI(coefVar(x), n = length(x), 
                    conf.level=conf.level, sides=sides, method=method)
  
  if(nrow(res) == 1)
    res <- res[1, ]
  
  return(res)
  
}  


# == internal helper functions ============================================

.coefVarCI <- function (K, n, conf.level = 0.95, 
                        sides = c("two.sided", "left", "right"), 
                        method = c("nct","vangel","mckay","verrill","naive")) {
  
  # Description of confidence intervals
  # https://www.itl.nist.gov/div898/software/dataplot/refman1/auxillar/coefvacl.htm
  
  
  .icoefVarCI <- Vectorize(function(K, n, conf.level=0.95, 
                                    sides = c("two.sided", "left", "right"), 
                                    method = c("vangel","mckay","verrill","nct","naive")) {
    
    method <- match.arg(method)
    sides <- match.arg(sides, choices = c("two.sided", "left", "right"), 
                       several.ok = FALSE)
    
    # double alpha in case of one-sided intervals in order to be able
    # to generally calculate twosided intervals and select afterwards..
    if (sides != "two.sided") 
      conf.level <- 1 - 2 * (1 - conf.level)
    
    alpha <- 1 - conf.level
    
    df <- n - 1
    u1 <- qchisq(1-alpha/2, df)
    u2 <- qchisq(alpha/2, df)
    
    switch(method, verrill = {
      CI.lower <- 0
      CI.upper <- 1
      
    }, vangel = {
      CI.lower <- K / sqrt(((u1+2)/n - 1) * K^2 + u1/df)
      CI.upper <- K / sqrt(((u2+2)/n - 1) * K^2 + u2/df)
      
    }, mckay = {
      CI.lower <- K / sqrt((u1/n - 1) * K^2 + u1/df)
      CI.upper <- K / sqrt((u2/n - 1) * K^2 + u2/df)
      
    }, nct = {
      ci <- .nctCI(sqrt(n)/K, df = df, conf.level = conf.level)
      CI.lower <- unname(sqrt(n)/ci[2])
      CI.upper <- unname(sqrt(n)/ci[1])
      
    }, naive = {
      CI.lower <- K * sqrt(df / u1)
      CI.upper <- K * sqrt(df / u2)
    }
    )
    
    ci <- c(est = K, 
            lci = min(CI.lower, CI.upper), # max(0, CI.lower), 
            uci = max(CI.lower, CI.upper)) # min(1, CI.upper))
    
    if (sides == "left") 
      ci[3] <- Inf
    
    else if (sides == "right") 
      ci[2] <- -Inf
    
    return(ci)
    
  })
  
  
  sides <- match.arg(sides)
  method <- match.arg(method)
  
  res <- t(.icoefVarCI(K=K, n=n, method=method, sides=sides, conf.level = conf.level))
  
  return(res)
  
}  



.nctCI <- function(tval, df, conf.level = 0.95, tol = 1e-8) {
  
  stopifnot(
    is.numeric(tval), length(tval) == 1,
    is.numeric(df), df > 0,
    is.numeric(conf.level), conf.level > 0, conf.level < 1
  )
  
  t_abs <- abs(tval)
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)
  
  f <- function(delta, p) {
    suppressWarnings(
      pt(t_abs, df = df, ncp = delta) - p
    )  
  }
  
  find_root <- function(p) {
    search <- max(1, t_abs)
    for (i in 1:50) {
      lo <- -search
      hi <-  search
      if (f(lo, p) * f(hi, p) < 0)
        return(uniroot(f, c(lo, hi), p = p, tol = tol)$root)
      search <- search * 2
    }
    stop("Could not bracket root for noncentral t CI.")
  }
  
  lower <- find_root(probs[2])
  upper <- find_root(probs[1])
  
  if (tval < 0) {
    tmp <- -upper
    upper <- -lower
    lower <- tmp
  }
  
  return(  c(lci = lower, uci = upper)  )
}
