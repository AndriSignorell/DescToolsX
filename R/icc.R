
#' Intraclass Correlations (ICC1, ICC2, ICC3 From Shrout and Fleiss)
#' 
#' The Intraclass correlation is used as a measure of association when studying
#' the reliability of raters.  Shrout and Fleiss (1979) outline 6 different
#' estimates, that depend upon the particular experimental design. All are
#' implemented and given confidence limits.
#' 
#' Shrout and Fleiss (1979) consider six cases of reliability of ratings done
#' by k raters on n targets.
#' 
#' \tabular{ll}{ ICC1 \tab Each target is rated by a different judge and the
#' judges are selected at random.\cr \tab (This is a one-way ANOVA fixed
#' effects model and is found by (MSB- MSW)/(MSB+ (nr-1)*MSW)) \cr
#' 
#' ICC2 \tab A random sample of k judges rate each target.  The measure is one
#' of absolute agreement \cr \tab in the ratings. Found as (MSB- MSE)/(MSB +
#' (nr-1)*MSE + nr*(MSJ-MSE)/nc) \cr ICC3 \tab A fixed set of k judges rate
#' each target. There is no generalization to a larger population \cr \tab of
#' judges. (MSB - MSE)/(MSB+ (nr-1)*MSE) \cr }
#' 
#' Then, for each of these cases, is reliability to be estimated for a single
#' rating or for the average of k ratings?  (The 1 rating case is equivalent to
#' the average intercorrelation, the k rating case to the Spearman Brown
#' adjusted reliability.)
#' 
#' ICC1 is sensitive to differences in means between raters and is a measure of
#' absolute agreement.
#' 
#' ICC2 and ICC3 remove mean differences between judges, but are sensitive to
#' interactions of raters by judges.  \cr The difference between ICC2 and ICC3
#' is whether raters are seen as fixed or random effects.
#' 
#' ICC1k, ICC2k, ICC3K reflect the means of k raters.
#' 
#' The intraclass correlation is used if raters are all of the same ``class".
#' That is, there is no logical way of distinguishing them.  Examples include
#' correlations between pairs of twins, correlations between raters.  If the
#' variables are logically distinguishable (e.g., different items on a test),
#' then the more typical coefficient is based upon the inter-class correlation
#' (e.g., a Pearson r) and a statistic such as alpha or omega might be used.
#' 
#' @name icc
#' @aliases ICC print.ICC
#' @param x \eqn{n \times m}{k x m} matrix or dataframe, k subjects (in rows) m
#' raters (in columns).
#' @param type one out of "all", "ICC1", "ICC2", "ICC3", "ICC1k", "ICC2k",
#' "ICC3k". See details.
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence intervals will be calculated.
#' @param na.rm logical, indicating whether \code{NA} values should be stripped
#' before the computation proceeds. If set to \code{TRUE} only the complete
#' cases of the ratings will be used. Defaults to \code{FALSE}.
#' @param \dots further arguments to be passed to or from methods.
#' @return if method is set to "all", then the result will be
#' 
#' \item{results}{A matrix of 6 rows and 8 columns, including the ICCs, F test,
#' p values, and confidence limits} \item{summary}{The anova summary table}
#' \item{stats}{The anova statistics} \item{MSW}{Mean Square Within based upon
#' the anova}
#' 
#' if a specific type has been defined, the function will first check, whether
#' no confidence intervals are requested: if so, the result will be the
#' estimate as numeric value\cr\cr else a named numeric vector with 3 elements
#' \item{ICCx}{estimate (name is the selected type of coefficient)}
#' \item{lwr.ci}{lower confidence interval} \item{upr.ci}{upper confidence
#' interval}
#' @note The results for the lower and upper Bounds for ICC(2,k) do not match
#' those of SPSS 9 or 10, but do match the definitions of Shrout and Fleiss.
#' SPSS seems to have been using the formula in McGraw and Wong, but not the
#' errata on p 390.  They seem to have fixed it in more recent releases (15).
#' @author William Revelle <revelle@@northwestern.edu>, some editorial
#' amendments Andri Signorell <andri@@signorell.net>
#' @references Shrout, P. E., Fleiss, J. L. (1979) Intraclass correlations:
#' uses in assessing rater reliability. \emph{ Psychological Bulletin}, 86,
#' 420-3428.
#' 
#' McGraw, K. O., Wong, S. P. (1996) Forming inferences about some intraclass
#' correlation coefficients.  \emph{ Psychological Methods}, 1, 30-46. + errata
#' on page 390.
#' 
#' Revelle, W. (in prep) \emph{ An introduction to psychometric theory with
#' applications in R} Springer. (working draft available at
#' \url{http://personality-project.org/r/book/}
#' 
#' @family Agreement
#' @concept Interrater Agreement
#' @concept Metric Agreement
#' 
#' @examples
#' 
#' sf <- matrix(c(
#'       9, 2, 5, 8,
#'       6, 1, 3, 2,
#'       8, 4, 6, 8,
#'       7, 1, 2, 6,
#'       10,5, 6, 9,
#'       6, 2, 4, 7),
#'       ncol=4, byrow=TRUE,
#'       dimnames=list(paste("S", 1:6, sep=""), paste("J", 1:4, sep=""))
#' )
#' 
#' sf  #example from Shrout and Fleiss (1979)
#' icc(sf)
#' 


#' @rdname icc
#' @export
icc <- function(x, type=c("all", "ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"), 
                conf.level = NA, na.rm = FALSE) {

  # ICC(ratings)
  # ICC_(ratings, type="ICC3", conf.level=0.95)
  # ICC_(ratings, type="all", conf.level=0.95)
  
  ratings <- as.matrix(x)
  if(na.rm) ratings <- na.omit(ratings)
  
  ns <- nrow(ratings)
  nr <- ncol(ratings)
  
  x.s <- stack(data.frame(ratings))
  x.df <- data.frame(x.s, subs = rep(paste("S", 1:ns, sep = ""), nr))
  
  s.aov <- summary(aov(values ~ subs + ind, data=x.df))
  stats <- matrix(unlist(s.aov), ncol=3, byrow=TRUE)
  MSB <- stats[3,1]
  MSW <- (stats[2,2] + stats[2,3])/(stats[1,2] + stats[1,3])
  MSJ <- stats[3,2]
  MSE <- stats[3,3]
  
  ICC1 <- (MSB- MSW)/(MSB+ (nr-1)*MSW)
  ICC2 <- (MSB- MSE)/(MSB + (nr-1)*MSE + nr*(MSJ-MSE)/ns)
  ICC3 <- (MSB - MSE)/(MSB+ (nr-1)*MSE)
  ICC12 <- (MSB-MSW)/(MSB)
  ICC22 <- (MSB- MSE)/(MSB +(MSJ-MSE)/ns)
  ICC32 <- (MSB-MSE)/MSB
  
  #find the various F values from Shrout and Fleiss
  F11 <- MSB/MSW
  df11n <- ns-1
  df11d <- ns*(nr-1)
  p11 <- 1 - pf(F11, df11n, df11d)
  F21 <- MSB/MSE
  df21n <- ns-1
  df21d <- (ns-1)*(nr-1)
  p21 <- 1-pf(F21, df21n, df21d)
  F31 <- F21
  
  
  # results <- t(results)
  
  results <- data.frame(matrix(NA, ncol=8, nrow=6))
  colnames(results ) <- c("type", "est","F-val","df1","df2","p-val","lwr.ci","upr.ci")
  rownames(results) <- c("Single_raters_absolute","Single_random_raters","Single_fixed_raters", "Average_raters_absolute","Average_random_raters","Average_fixed_raters")
  
  results[,1] = c("ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k")
  results[,2] = c(ICC1, ICC2, ICC3, ICC12, ICC22, ICC32)
  results[1,3] <- results[4,3] <- F11
  results[2,3] <- F21
  results[3,3] <- results[6,3] <- results[5,3] <- F31 <- F21
  results[5,3] <- F21
  results[1,4] <- results[4,4] <- df11n
  results[1,5] <- results[4,5] <- df11d
  results[1,6] <- results[4,6] <- p11
  results[2,4] <- results[3,4] <- results[5,4] <- results[6,4] <- df21n
  results[2,5] <- results[3,5] <- results[5,5] <- results[6,5] <- df21d
  results[2,6] <- results[5,6] <- results[3,6] <- results[6,6] <- p21
  
  #now find confidence limits
  #first, the easy ones
  alpha <- 1 - conf.level
  F1L <- F11 / qf(1-alpha/2, df11n, df11d)
  F1U <- F11 * qf(1-alpha/2, df11d, df11n)
  L1 <- (F1L-1) / (F1L + (nr - 1))
  U1 <- (F1U -1) / (F1U + nr - 1)
  F3L <- F31 / qf(1-alpha/2, df21n, df21d)
  F3U <- F31 * qf(1-alpha/2, df21d, df21n)
  results[1,7] <- L1
  results[1,8] <- U1
  results[3,7] <- (F3L-1)/(F3L+nr-1)
  results[3,8] <- (F3U-1)/(F3U+nr-1)
  results[4,7] <- 1- 1/F1L
  results[4,8] <- 1- 1/F1U
  results[6,7] <- 1- 1/F3L
  results[6,8] <- 1 - 1/F3U
  
  #the hard one is case 2
  Fj <- MSJ/MSE
  vn <- (nr-1)*(ns-1)* ( (nr*ICC2*Fj+ns*(1+(nr-1)*ICC2) - nr*ICC2))^2
  vd <- (ns-1)*nr^2 * ICC2^2 * Fj^2 + (ns *(1 + (nr-1)*ICC2) - nr*ICC2)^2
  v <- vn/vd
  F3U <- qf(1-alpha/2,ns-1,v)
  F3L <- qf(1-alpha/2,v,ns-1)
  
  L3 <- ns *(MSB- F3U*MSE)/(F3U*(nr * MSJ + (nr*ns-nr-ns) * MSE)+ ns*MSB)
  results[2, 7] <- L3
  U3 <- ns *(F3L * MSB - MSE)/(nr * MSJ + (nr * ns - nr - ns)*MSE + ns * F3L * MSB)
  results[2, 8] <- U3
  L3k <- L3 * nr/(1+ L3*(nr-1))
  U3k <- U3 * nr/(1+ U3*(nr-1))
  results[5, 7] <- L3k
  results[5, 8] <- U3k
  
  
  #clean up the output
  results[,2:8] <- results[,2:8]
  
  type <- match.arg(type, c("all", "ICC1","ICC2","ICC3","ICC1k","ICC2k","ICC3k"))
  
  switch(type
         , all={res <- list(results=results, summary=s.aov, stats=stats, MSW=MSW, ns=ns, nr=nr)
         class(res) <- "ICC"
         }
         , ICC1={idx <- 1}
         , ICC2={idx <- 2}
         , ICC3={idx <- 3}
         , ICC1k={idx <- 4}
         , ICC2k={idx <- 5}
         , ICC3k={idx <- 6}
  )
  
  if(type!="all"){
    if(is.na(conf.level)){
      res <- results[idx, c(2)]
    } else {
      res <- unlist(results[idx, c(2, 7:8)])
      names(res) <- c(type,"lwr.ci","upr.ci")
    }
  }
  
  return(res)
  
}


#' @param digits number of digits to use in printing
#' @rdname icc
#' @export
print.ICC <- function(x, digits = 3, ...){
  cat("\nIntraclass correlation coefficients \n")
  print(x$results, digits=digits)
  cat("\n Number of subjects =", x$ns, "    Number of raters =", x$nr, "\n")
}


