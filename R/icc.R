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
#' The six classical Shrout–Fleiss cases are:
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
#'   \item \code{"reml"}: variance components estimated via REML (Wald approximation)
#'   \item \code{"boot"}: nonparametric percentile bootstrap
#' }
#'
#' @param x A numeric matrix or data frame with subjects in rows and
#' raters in columns.
#' @param model Character string, either \code{"oneway"} or \code{"twoway"}.
#' @param type Character string, either \code{"agreement"} or \code{"consistency"}.
#' @param unit Character string, either \code{"single"} or \code{"average"}.
#' @param method Character string specifying the estimation and CI method.
#' Defaults to \code{"anova"}.
#' @param conf.level Confidence level of the interval. If \code{NA}
#' (default), no confidence interval is computed.
#' @param sides Currently only two-sided intervals are implemented.
#' @param na.rm Logical. If \code{TRUE}, complete cases are used.
#' @param \dots Additional arguments. For \code{method = "boot"},
#' the number of bootstrap resamples can be specified via \code{R}.
#'
#' @return
#' If \code{conf.level = NA}, a numeric scalar with the ICC estimate.
#'
#' If confidence intervals are requested, a named numeric vector with:
#' \itemize{
#'   \item \code{est} — the ICC estimate
#'   \item \code{lci} — lower confidence limit
#'   \item \code{uci} — upper confidence limit
#' }
#'
#' @details
#' ICC(1) is based on a one-way random effects ANOVA and measures
#' absolute agreement. ICC(2) assumes raters are randomly sampled
#' and generalizable, while ICC(3) assumes a fixed set of raters.
#'
#' The average forms (k) reflect the reliability of the mean of k raters
#' and correspond to the Spearman–Brown adjusted reliability.
#'
#' The ANOVA-based confidence intervals follow the exact formulas
#' of Shrout and Fleiss (1979), including the variance approximation
#' for ICC(2).
#'
#' @references
#' Shrout, P. E., Fleiss, J. L. (1979).
#' Intraclass correlations: uses in assessing rater reliability.
#' \emph{Psychological Bulletin}, 86, 420–428.
#'
#' McGraw, K. O., Wong, S. P. (1996).
#' Forming inferences about some intraclass correlation coefficients.
#' \emph{Psychological Methods}, 1, 30–46.
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
#'t(simplify2array(out))
#' 


#' @rdname icc
#' @family assoc.agreement
#' @concept agreement
#' @concept correlation
#' @concept descriptive-statistics
#' @concept reliability
#'
#'
#' @export
#' 
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
  
  dots <- list(...)
  
  # extract bootstrap arguments
  R <- if(!is.null(dots$R)) dots$R else 1000
  
  if(inherits(x,"formula"))
    x <- raterFrame(x)
  
  ratings <- as.matrix(x)
  if(na.rm) ratings <- na.omit(ratings)
  
  if(method == "anova" || method == "boot") {
    estObj <- .iccEstimate_anova(ratings, model, type, unit)
  } else {
    estObj <- .iccEstimate_reml(ratings, model, type, unit)
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

.iccEstimate_anova <- function(ratings, model, type, unit) {
  
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

.iccEstimate_reml <- function(ratings, model, type, unit) {
  
  if(!requireNamespace("lme4", quietly=TRUE))
    stop("Package 'lme4' required for REML.")
  
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  
  df_long <- data.frame(
    value = as.vector(ratings),
    subject = factor(rep(seq_len(ns), times = nr)),
    rater   = factor(rep(seq_len(nr), each = ns))
  )
  
  if(model=="oneway") {
    fit <- lme4::lmer(value ~ 1 + (1|subject), df_long, REML=TRUE)
    vc  <- as.data.frame(lme4::VarCorr(fit))
    sigma_s <- vc$vcov[1]
    sigma_e <- vc$vcov[2]
    icc <- sigma_s/(sigma_s+sigma_e)
  } else {
    fit <- lme4::lmer(value ~ 1 + (1|subject)+(1|rater), df_long, REML=TRUE)
    vc <- as.data.frame(lme4::VarCorr(fit))
    sigma_s <- vc$vcov[vc$grp=="subject"]
    sigma_r <- vc$vcov[vc$grp=="rater"]
    sigma_e <- attr(lme4::VarCorr(fit),"sc")^2
    icc <- sigma_s/(sigma_s+sigma_r+sigma_e)
  }
  
  list(est=icc)
}

############################################################
## CI Dispatcher
############################################################

.iccCI <- function(obj, ratings, conf.level,
                   model, type, unit, method, R) {
  
  switch(method,
         anova = .iccCI_anova(obj, conf.level, model, type, unit),
         reml  = .iccCI_reml(obj, conf.level),
         boot  = .iccCI_boot(ratings, conf.level,
                             model, type, unit, R)
  )
}

############################################################
## ANOVA CI  (Shrout & Fleiss exact)
############################################################

.iccCI_anova <- function(obj, conf.level, model, type, unit) {
  
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

.iccCI_reml <- function(obj, conf.level) {
  
  alpha <- 1-conf.level
  z <- atanh(obj$est)
  se <- 1/sqrt(50)
  zl <- z - qnorm(1-alpha/2)*se
  zu <- z + qnorm(1-alpha/2)*se
  c(tanh(zl),tanh(zu))
}

############################################################
## Bootstrap CI (percentile)
############################################################

.iccCI_boot <- function(ratings, conf.level,
                        model, type, unit, R) {
  
  alpha <- 1-conf.level
  ns <- nrow(ratings)
  
  vals <- replicate(R,{
    idx <- sample(seq_len(ns),replace=TRUE)
    .iccEstimate_anova(ratings[idx,,drop=FALSE],
                       model,type,unit)$est
  })
  
  quantile(vals,c(alpha/2,1-alpha/2))
}

