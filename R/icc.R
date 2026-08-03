
#' Intraclass Correlation Coefficient (ICC)
#'
#' Computes intraclass correlation coefficients (ICC) according to
#' Shrout and Fleiss (1979) and McGraw and Wong (1996).
#'
#' The ICC is a measure of reliability for ratings of \eqn{n} subjects
#' by \eqn{k} raters. The specific coefficient depends on three design
#' decisions:
#'
#' \itemize{
#'   \item \strong{model}: one-way or two-way ANOVA design
#'   \item \strong{type}: agreement or consistency
#'   \item \strong{unit}: single rating or average of k ratings
#' }
#'
#' The six classical Shrout--Fleiss cases are:
#'
#' \tabular{lll}{
#' model \tab type \tab unit \cr
#' oneway  \tab agreement   \tab single   (ICC1)  \cr
#' oneway  \tab agreement   \tab average  (ICC1k) \cr
#' twoway  \tab agreement   \tab single   (ICC2)  \cr
#' twoway  \tab agreement   \tab average  (ICC2k) \cr
#' twoway  \tab consistency \tab single   (ICC3)  \cr
#' twoway  \tab consistency \tab average  (ICC3k) \cr
#' }
#'
#' For \code{model = "oneway"} only \code{type = "agreement"} is meaningful.
#'
#' Confidence intervals can be computed using different inference methods:
#'
#' \itemize{
#'   \item \code{"anova"}: exact F-based intervals following Shrout and Fleiss (1979)
#'   \item \code{"reml"}: variance components estimated via REML. Point
#'     estimate only; no confidence interval is available for this method.
#'   \item \code{"boot"}: nonparametric percentile bootstrap
#' }
#'
#' @param x numeric matrix or data frame with subjects in rows and raters in
#' columns
#' @param model character string, either \code{"oneway"} or \code{"twoway"}
#' @param type character string, either \code{"agreement"} or
#' \code{"consistency"}
#' @param unit character string, either \code{"single"} or \code{"average"}
#' @param method character string specifying the estimation and confidence
#' interval method; defaults to \code{"anova"}
#' @param conf.level confidence level of the interval. If \code{NA}
#' (default), no confidence interval is computed.
#' @param sides character string specifying the side of the interval; currently
#' only \code{"two.sided"} is implemented
#' @param na.rm logical; if \code{TRUE}, complete cases are used
#' @param \dots additional arguments. For \code{method = "boot"},
#' the number of bootstrap resamples can be specified via \code{R}.
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the intraclass correlation}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @section Random number generation:
#' \code{method = "boot"} resamples subjects and therefore advances R's
#' global random number generator. Call \code{\link[base]{set.seed}}
#' beforehand for reproducible intervals.
#'
#' @details
#' ICC(1) is based on a one-way random effects ANOVA and measures
#' absolute agreement. ICC(2) assumes raters are randomly sampled
#' and generalizable, while ICC(3) assumes a fixed set of raters.
#'
#' The average forms (k) reflect the reliability of the mean of k raters
#' and correspond to the Spearman--Brown adjusted reliability.
#'
#' The ANOVA-based confidence intervals follow the exact formulas
#' of Shrout and Fleiss (1979), including the variance approximation
#' for ICC(2).
#'
#' @references
#' Shrout, P. E., Fleiss, J. L. (1979).
#' Intraclass correlations: uses in assessing rater reliability.
#' \emph{Psychological Bulletin}, 86, 420--428.
#'
#' McGraw, K. O., Wong, S. P. (1996).
#' Forming inferences about some intraclass correlation coefficients.
#' \emph{Psychological Methods}, 1, 30--46.
#'
#' @examples
#' #example from Shrout and Fleiss (1979)
#' sf <- matrix(c( 9, 2, 5, 8,    6, 1, 3, 2,    8, 4, 6, 8,     
#'                 7, 1, 2, 6,   10, 5, 6, 9,    6, 2, 4, 7),
#'       ncol=4, byrow=TRUE,
#'       dimnames=list(c("S1","S2","S3","S4","S5","S6"), 
#'                     c("J1","J2","J3","J4"))  )
#' 
#' icc(sf)
#' 
#' # get all versions
#' args <- formals(icc)[c("model","type","unit")]
#' grid <- expand.grid(lapply(args, eval), 
#'                     stringsAsFactors = FALSE)[-c(4,8),]
#'                     
#' out <- apply(grid, 1, function(row)
#'   icc(sf,
#'       model = row["model"],
#'       type  = row["type"],
#'       unit  = row["unit"],
#'       method = "anova",
#'       conf.level = 0.95) )
#'       
#' t(simplify2array(out))
#'
#' @rdname icc
#' @family assoc.agreement
#' @concept agreement
#' @concept reliability
#' @concept variance-component
#' @export
icc <- function(x,
                model = c("twoway","oneway"),
                type  = c("agreement","consistency"),
                unit  = c("single","average"),
                conf.level = NA,
                sides = c("two.sided","left","right"),
                method = c("anova","reml","boot"),
                na.rm = FALSE,
                ...) {
  
  
  # Shrout & Fleiss	Deine API	McGraw & Wong
  # ICC(1)	oneway_agreement_single	    Single_raters_absolute
  # ICC(1k)	oneway_agreement_average	  Average_raters_absolute
  # ICC(2)	twoway_agreement_single	    Single_raters_absolute
  # ICC(2k)	twoway_agreement_average	  Average_raters_absolute
  # ICC(3)	twoway_consistency_single	  Single_raters_consistency
  # ICC(3k)	twoway_consistency_average	Average_raters_consistency
  
  model  <- match.arg(model)
  type   <- match.arg(type)
  unit   <- match.arg(unit)
  method <- match.arg(method)
  sides  <- match.arg(sides)

  # A one-way design has no rater effect to hold constant, so consistency
  # is undefined. The switch() in .iccEstimateAnova() has no entry for the
  # combination and returned NULL, which travelled out of icc() as NULL -
  # or, with conf.level set, as a two-element vector missing its est. The
  # documented example works around it by dropping rows 4 and 8 of the
  # grid, a sign that the case was known but never refused.
  if(model == "oneway" && type == "consistency")
    stop("type = \"consistency\" is not defined for model = \"oneway\"; ",
         "a one-way design has no rater effect", call. = FALSE)

  if(!is.na(conf.level) && sides != "two.sided")
    stop("only two-sided confidence intervals are currently implemented")
  
  dots <- list(...)
  
  # extract bootstrap arguments
  R <- if(!is.null(dots$R)) dots$R else 1000
  
  if(inherits(x,"formula"))
    x <- raterFrame(x)
  
  ratings <- as.matrix(x)
  if(na.rm) ratings <- na.omit(ratings)
  
  if(method == "anova" || method == "boot") {
    estObj <- .iccEstimateAnova(ratings, model, type, unit)
  } else {
    estObj <- .iccEstimateReml(ratings, model, type, unit)
  }
  
  if(!is.na(conf.level)) {
    ci <- .iccCI(estObj, ratings, conf.level,
                 model, type, unit, method, R)
    res <- c(est = estObj$est, lci = ci[1], uci = ci[2])
  } else {
    res <- estObj$est
  }
  
  return(res)
}

############################################################
## ANOVA Estimator
############################################################

.iccEstimateAnova <- function(ratings, model, type, unit) {
  
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  
  df_long <- data.frame(
    value = as.vector(ratings),
    subject = factor(rep(seq_len(ns), times = nr)),
    rater   = factor(rep(seq_len(nr), each = ns))
  )
  
  aov_tab <- summary(aov(value ~ subject + rater, data=df_long))[[1]]
  
  MSB <- aov_tab["subject","Mean Sq"]
  MSJ <- aov_tab["rater","Mean Sq"]
  MSE <- aov_tab["Residuals","Mean Sq"]
  
  MSW <- (aov_tab["rater","Sum Sq"] +
            aov_tab["Residuals","Sum Sq"]) /
    (aov_tab["rater","Df"] +
       aov_tab["Residuals","Df"])
  
  icc1  <- (MSB - MSW) / (MSB + (nr - 1) * MSW)
  icc2  <- (MSB - MSE) /
    (MSB + (nr - 1) * MSE + nr * (MSJ - MSE) / ns)
  icc3  <- (MSB - MSE) / (MSB + (nr - 1) * MSE)
  
  icc1k <- (MSB - MSW) / MSB
  icc2k <- (MSB - MSE) / (MSB + (MSJ - MSE) / ns)
  icc3k <- (MSB - MSE) / MSB
  
  est <- switch(paste(model,type,unit,sep="_"),
                "oneway_agreement_single"    = icc1,
                "oneway_agreement_average"   = icc1k,
                "twoway_agreement_single"    = icc2,
                "twoway_agreement_average"   = icc2k,
                "twoway_consistency_single"  = icc3,
                "twoway_consistency_average" = icc3k)
  
  list(est=est, icc2=icc2,
       MSB=MSB, MSJ=MSJ, MSE=MSE, MSW=MSW,
       ns=ns, nr=nr)
}

############################################################
## REML Estimator
############################################################

.iccEstimateReml <- function(ratings, model, type, unit) {
  
  if(!requireNamespace("lme4", quietly=TRUE))
    stop("Package 'lme4' required for REML.")
  
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  
  df_long <- data.frame(
    value = as.vector(ratings),
    subject = factor(rep(seq_len(ns), times = nr)),
    rater   = factor(rep(seq_len(nr), each = ns))
  )
  
  # By name, not by position: the one-way branch used vc$vcov[1] and [2],
  # which relies on VarCorr() ordering subject before Residual.
  getVc <- function(fit, grp) {
    vc <- as.data.frame(lme4::VarCorr(fit))
    vc$vcov[vc$grp == grp]
  }

  if(model=="oneway") {

    fit <- lme4::lmer(value ~ 1 + (1|subject), df_long, REML=TRUE)
    sigma_s <- getVc(fit, "subject")
    sigma_r <- 0
    sigma_e <- getVc(fit, "Residual")

  } else {

    fit <- lme4::lmer(value ~ 1 + (1|subject)+(1|rater), df_long, REML=TRUE)
    sigma_s <- getVc(fit, "subject")
    sigma_r <- getVc(fit, "rater")
    sigma_e <- getVc(fit, "Residual")

  }

  # type and unit were ignored entirely: the function always returned the
  # single-rating ABSOLUTE-agreement coefficient, so
  # icc(x, method = "reml", unit = "average") silently gave the same
  # number as unit = "single", and type = "consistency" the same as
  # type = "agreement".
  #
  # consistency drops the rater variance from the denominator, agreement
  # keeps it; the average form divides the error terms by nr, which is the
  # Spearman-Brown adjustment written in variance components.
  icc <- if(unit == "single") {

    sigma_s / if(type == "consistency") sigma_s + sigma_e
              else sigma_s + sigma_r + sigma_e

  } else {

    sigma_s / if(type == "consistency") sigma_s + sigma_e / nr
              else sigma_s + (sigma_r + sigma_e) / nr

  }

  list(est = icc, ns = ns, nr = nr,
       sigmaS = sigma_s, sigmaR = sigma_r, sigmaE = sigma_e)
}

############################################################
## CI Dispatcher
############################################################

.iccCI <- function(obj, ratings, conf.level,
                   model, type, unit, method, R) {
  
  switch(method,
         anova = .iccCIAnova(obj, conf.level, model, type, unit),
         reml  = .iccCIReml(obj, conf.level),
         boot  = .iccCIBoot(ratings, conf.level,
                            model, type, unit, R)
  )
}

############################################################
## ANOVA CI  (Shrout & Fleiss exact)
############################################################

.iccCIAnova <- function(obj, conf.level, model, type, unit) {
  
  alpha <- 1 - conf.level
  
  MSB <- obj$MSB; MSJ <- obj$MSJ
  MSE <- obj$MSE; MSW <- obj$MSW
  ns  <- obj$ns;  nr  <- obj$nr
  icc2 <- obj$icc2
  
  if(model=="oneway") {
    
    F  <- MSB/MSW
    df1 <- ns-1
    df2 <- ns*(nr-1)
    
    FL <- F/qf(1-alpha/2,df1,df2)
    FU <- F*qf(1-alpha/2,df2,df1)
    
    if(unit=="single") {
      lwr <- (FL-1)/(FL+nr-1)
      upr <- (FU-1)/(FU+nr-1)
    } else {
      lwr <- 1-1/FL
      upr <- 1-1/FU
    }
    
  } else if(model=="twoway" && type=="consistency") {
    
    F  <- MSB/MSE
    df1 <- ns-1
    df2 <- (ns-1)*(nr-1)
    
    FL <- F/qf(1-alpha/2,df1,df2)
    FU <- F*qf(1-alpha/2,df2,df1)
    
    if(unit=="single") {
      lwr <- (FL-1)/(FL+nr-1)
      upr <- (FU-1)/(FU+nr-1)
    } else {
      lwr <- 1-1/FL
      upr <- 1-1/FU
    }
    
  } else {
    
    Fj <- MSJ/MSE
    
    vn <- (nr-1)*(ns-1)*
      ( (nr*icc2*Fj + ns*(1+(nr-1)*icc2)-nr*icc2)^2 )
    
    vd <- (ns-1)*nr^2*icc2^2*Fj^2 +
      (ns*(1+(nr-1)*icc2)-nr*icc2)^2
    
    v <- vn/vd
    
    F_upper <- qf(1-alpha/2,ns-1,v)
    F_lower <- qf(1-alpha/2,v,ns-1)
    
    L <- ns*(MSB-F_upper*MSE)/
      (F_upper*(nr*MSJ+(nr*ns-nr-ns)*MSE)+ns*MSB)
    
    U <- ns*(F_lower*MSB-MSE)/
      (nr*MSJ+(nr*ns-nr-ns)*MSE+ns*F_lower*MSB)
    
    if(unit=="single") {
      lwr <- L
      upr <- U
    } else {
      lwr <- L*nr/(1+L*(nr-1))
      upr <- U*nr/(1+U*(nr-1))
    }
  }
  
  c(lwr,upr)
}

############################################################
## REML CI (Wald on Fisher-z)
############################################################

.iccCIReml <- function(obj, conf.level) {

  # The former body read
  #
  #     z  <- atanh(obj$est)
  #     se <- 1/sqrt(50)          # <- a constant
  #     c(tanh(z - qnorm(...)*se), tanh(z + qnorm(...)*se))
  #
  # The standard error did not depend on the number of subjects, the
  # number of raters, or the fitted variance components, so the interval
  # was exactly as wide for ns = 10 as for ns = 10000 - the 50 amounts to
  # a fixed n of 53 - while being documented as a REML "Wald
  # approximation". A number that looks like a confidence interval and is
  # not one is worse than none.
  #
  # A defensible interval needs the sampling covariance of the variance
  # components, which lme4 does not expose directly; the usual route is a
  # profile or parametric bootstrap over the fit (lme4::confint.merMod).
  # Until that exists, refuse.
  stop("confidence intervals for method = \"reml\" are not implemented; ",
       "use method = \"anova\" for the exact F-based interval, ",
       "or method = \"boot\"", call. = FALSE)
}

############################################################
## Bootstrap CI (percentile)
############################################################

.iccCIBoot <- function(ratings, conf.level,
                       model, type, unit, R) {
  
  alpha <- 1-conf.level
  ns <- nrow(ratings)
  
  vals <- replicate(R,{
    idx <- sample(seq_len(ns),replace=TRUE)
    .iccEstimateAnova(ratings[idx,,drop=FALSE],
                      model,type,unit)$est
  })
  
  unname(quantile(vals, c(alpha/2, 1-alpha/2)))
}
