
#' Kappa for m Raters
#' 
#' Computes Fleiss' kappa, which quantifies agreement among \eqn{m \geq 2} raters on
#' categorical items by contrasting the mean observed agreement with the chance
#' agreement implied by the overall category proportions. It equals Scott's pi
#' when m=2 (but not Cohen's kappa).
#' 
#' Missing data are omitted in a listwise way.\cr Fleiss' kappa (Fleiss, 1971)
#' is a multi-rater agreement coefficient that generalises Scott's pi (Scott,
#' 1955) to m raters by basing chance agreement on the average category
#' proportions across raters. For m = 2 it equals Scott's pi (not Cohen's
#' kappa). Cohen's kappa, by contrast, is defined for two raters and computes
#' chance agreement from each rater's own marginal distribution; it can also be
#' extended with weights for ordinal scales (weighted Cohen's kappa). Light's
#' kappa (Light, 1971) is simply the unweighted mean of all pairwise Cohen
#' kappas among multiple raters, whereas Conger's kappa (Conger, 1980) is a
#' principled multi-rater generalisation that reduces exactly to Cohen's kappa
#' when m = 2.\cr Standard errors and Wald-type confidence intervals are
#' available for all of these coefficients (Cohen, Scott, Fleiss, Conger);
#' bootstrap intervals are a practical alternative when assumptions are
#' doubtful.
#' 
#' @param x \eqn{n \times m}{n x m} matrix or dataframe, n subjects m raters.
#' @param method a logical indicating whether the exact Kappa (Conger, 1980),
#' the Kappa described by Fleiss (1971) or Light's Kappa (1971) should be
#' computed.
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence intervals will be calculated.
#' @return a single numeric value if no confidence intervals are requested,\cr
#' and otherwise a numeric vector with 3 elements for the estimate, the lower
#' and the upper confidence interval
#' @note This function was previously published as \code{kappaM.fleiss()} in
#' the \pkg{irr} package and has been integrated here with some changes in the
#' interface.
#' @author Matthias Gamer, with some modifications by Andri Signorell
#' <andri@@signorell.net>
#' @seealso \code{\link{cohenKappa}}
#' @references Conger, A.J. (1980): Integration and generalisation of Kappas
#' for multiple raters. \emph{Psychological Bulletin}, 88, 322-328
#' 
#' Fleiss, J.L. (1971): Measuring nominal scale agreement among many raters
#' \emph{Psychological Bulletin}, 76, 378-382
#' 
#' Fleiss, J.L., Levin, B., & Paik, M.C. (2003): \emph{Statistical Methods for
#' Rates and Proportions}, 3rd Edition. New York: John Wiley & Sons
#' 
#' Light, R.J. (1971): Measures of response agreement for qualitative data:
#' Some generalizations and alternatives.  \emph{Psychological Bulletin}, 76,
#' 365-377.
#' 
#' Scott, W.A. (1955). Reliability of content analysis: the case of nominal
#' scale coding. \emph{Public Opinion Quarterly}, XIX, 321-325.
#' 
#' @family topic.Agreement
#' @concept Interrater Agreement
#' @concept Multi-Rater Agreement
#' @concept Nominal Agreement
#' 
#' 
#' @examples
#' 
#' statement <- data.frame(
#'   A=c(2,3,1,3,1,2,1,2,3,3,3,3,3,2,1,3,3,2,2,1,
#'       2,1,3,3,2,2,1,2,1,1,2,3,3,3,3,3,1,2,1,1),
#'   B=c(2,2,2,1,1,2,1,2,3,3,2,3,1,3,1,1,3,2,1,2,
#'       2,1,3,2,2,2,3,2,1,1,2,2,3,3,3,3,2,2,2,3),
#'   C=c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,2,2,2,2,3,
#'       2,2,3,3,2,2,3,2,2,2,2,3,3,3,3,3,3,2,2,2),
#'   D=c(2,2,2,1,1,2,1,2,3,3,2,3,3,3,3,3,2,2,2,2,
#'       3,1,3,2,2,2,1,2,2,1,2,3,3,3,3,3,3,2,2,1),
#'   E=c(2,2,2,3,3,2,3,1,3,3,2,3,3,3,3,3,2,2,2,3,
#'       2,3,3,2,2,2,3,2,1,3,2,3,3,1,3,3,3,2,2,1)
#' )
#' 
#' kappaM(statement)
#' 
#' kappaM(statement, method="Conger")   # Exact Kappa
#' kappaM(statement, conf.level=0.95)   # Fleiss' Kappa and confidence intervals
#' 
#' kappaM(statement, method="Light")   # Exact Kappa
#' 

#' @export
kappaM <- function(x, method = c("Fleiss", "Conger", "Light"), conf.level = NA) {
  
  # ratings <- as.matrix(na.omit(x))
  #
  # ns <- nrow(ratings)
  # nr <- ncol(ratings)
  #
  # # Build table
  # lev <- levels(as.factor(ratings))
  #
  # for (i in 1:ns) {
  #   frow <- factor(ratings[i,],levels=lev)
  #
  #   if (i==1)
  #     ttab <- as.numeric(table(frow))
  #   else
  #     ttab <- rbind(ttab, as.numeric(table(frow)))
  # }
  #
  # ttab <- matrix(ttab, nrow=ns)
  
  
  calc_Pe2_i <- function(M, pjr) {
    # M: N x R Matrix (factors mit gleichen levels)
    # pjr: K x R Matrix, Randverteilungen (Zeilen = Kategorien, Spalten = Rater)
    
    N <- nrow(M)
    R <- ncol(M)
    
    # Indizes: mappe jede Kategorie auf ihre Zeilennummer in pjr
    lv <- rownames(pjr)
    idx <- apply(M, 2, function(col) match(as.character(col), lv))
    # idx: N x R Matrix mit Kategorie-Indices
    
    # p_lookup[i,r,r2] = pjr[idx[i,r2], r]
    # bedeutet: nehme Randwahrscheinlichkeit von Rater r for die Kategorie,
    # die Rater r2 bei Subjekt i gewaehlt hat
    Pe2_i <- sapply(1:N, function(i) {
      mat <- outer(1:R, 1:R, Vectorize(function(r, r2) {
        if (r == r2) return(0)
        pjr[idx[i, r2], r]
      }))
      sum(mat) / (R * (R - 1))
    })
    
    Pe2_i
  }
  
  
  
  # we have not factors for matrices, but we need factors below...
  if(is.matrix(x))
    x <- as.data.frame(x)
  
  x <- na.omit(x)
  ns <- nrow(x)
  nr <- ncol(x)
  
  # find all levels in the data (data.frame)
  lev <- levels(factor(unlist(x)))
  levi <- seq_along(lev)
  # apply the same levels to all variables and switch to integer matrix
  xx <- do.call(cbind, lapply(x, factor, levels=lev))
  
  ttab <- apply(abind(lapply(as.data.frame(xx), 
                             function(z) dummy(z, method="full", 
                                               levels=levi)), 
                      along = 3),
                c(1,2), sum)
  
  agreeP <- sum((rowSums(ttab^2)-nr)/(nr*(nr-1))/ns)
  
  switch( match.arg(method, choices= c("Fleiss", "Conger", "Light"))
          
          , "Fleiss" = {
            chanceP <- sum(colSums(ttab)^2)/(ns*nr)^2
            value <- (agreeP - chanceP)/(1 - chanceP)
            
            pj <- colSums(ttab)/(ns*nr)
            qj <- 1-pj
            
            varkappa <- (2/(sum(pj*qj)^2*(ns*nr*(nr-1))))*(sum(pj*qj)^2-sum(pj*qj*(qj-pj)))
            SEkappa <- sqrt(varkappa)
            
            ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
          }
          
          , "Conger" = {
            
            rtab <- apply(abind(lapply(as.data.frame(t(xx)), 
                                       function(z) dummy(z, method="full", 
                                                         levels=levi)), 
                                along = 3),
                          c(1,2), sum)
            
            rtab <- rtab/ns
            
            chanceP <- sum(colSums(ttab)^2)/(ns*nr)^2 - sum(apply(rtab, 2, var)*(nr-1)/nr)/(nr-1)
            value <- (agreeP - chanceP)/(1 - chanceP)
            
            
            
            
            # 1. Beobachtete Uebereinstimmung pro Subjekt (Po_i)
            
            Po_i <- apply(xx, 1, function(row) {
              counts <- table(row)
              sum(counts * (counts - 1)) / (nr * (nr - 1))
            })
            
            ## 2. Raterspezifische Randverteilungen p_j(r)
            pjr <- lapply(1:nr, function(r) {
              tab <- table(xx[, r])
              as.numeric(tab) / ns
            })
            pjr <- do.call(cbind, pjr)   # K x R Matrix
            rownames(pjr) <- as.character(levi)
            
            ## 4. Erwartete Uebereinstimmung pro Subjekt (Pe2_i)
            # chanceP_i <- numeric(ns)
            # for (i in 1:ns) {
            #   row <- xx[i, ]
            #   for (r in 1:nr) {
            #     cat_r <- as.character(row[r])
            #     for (r2 in setdiff(1:nr, r)) {
            #       chanceP_i[i] <- chanceP_i[i] + pjr[cat_r, r]
            #     }
            #   }
            #   chanceP_i[i] <- chanceP_i[i] / (nr * (nr - 1))
            # }
            
            chanceP_i <- calc_Pe2_i(xx, pjr)
            
            # SE for exact Kappa value
            num <- mean(((1 - chanceP) * Po_i - 2 * (1 - agreeP) * chanceP_i)^2) - 
              (agreeP * chanceP - 2 * chanceP + agreeP)^2
            varkappa <- num / ((1 - chanceP)^4 * ns)
            SEkappa <- sqrt(varkappa)
            
            ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
            
          }
          
          , "Light" = {
            m <- pairApply(x, cohenKappa, symmetric=TRUE)
            value <- mean(m[upper.tri(m)])
            
            levlen <- length(lev)
            for (nri in 1:(nr - 1)) for (nrj in (nri + 1):nr) {
              for (i in 1:levlen) for (j in 1:levlen) {
                if (i != j) {
                  r1i <- sum(x[, nri] == lev[i])
                  r2j <- sum(x[, nrj] == lev[j])
                  if (!exists("dis"))
                    dis <- r1i * r2j
                  else dis <- c(dis, r1i * r2j)
                }
              }
              if (!exists("disrater"))
                disrater <- sum(dis)
              else disrater <- c(disrater, sum(dis))
              rm(dis)
            }
            B <- length(disrater) * prod(disrater)
            chanceP <- 1 - B/ns^(choose(nr, 2) * 2)
            varkappa <- chanceP/(ns * (1 - chanceP))
            SEkappa <- sqrt(varkappa)
            
            ci <- value + c(1,-1) * qnorm((1-conf.level)/2) * SEkappa
            
          }
  )
  
  
  if (is.na(conf.level)) {
    res <- value
  } else {
    res <- c("kappa"=value, lwr.ci=ci[1], upr.ci=ci[2])
  }
  return(res)
  
}
