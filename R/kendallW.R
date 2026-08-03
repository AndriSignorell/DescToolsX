
#' Kendall's Coefficient of Concordance W
#' 
#' Computes Kendall's coefficient of concordance, a popular measure of
#' association. It is an index of interrater reliability of ordinal data. The
#' coefficient could be corrected for ties within raters.
#' 
#' The test for Kendall's W is completely equivalent to
#' \code{\link[stats]{friedman.test}}. The only advantage of this test over
#' Friedman's is that Kendall's W has an interpretation as the coefficient of
#' concordance. The test itself is only valid for large samples.\cr Kendall's W
#' should be corrected for ties, if raters did not use a true ranking order for
#' the subjects. 
#' The function warns if ties are present and no correction has been required.
#' 
#' In the presence of \code{NAs} the algorithm is switched to a generalized form for 
#' randomly incomplete datasets introduced in Brueckl (2011).
#' This approach uses the mean Spearman \eqn{\rho}{rho} of all pairwise comparisons 
#' (see Kendall, 1962):\cr
#' 
#' \deqn{W = (1+mean(\rho)*(k-1)) / k}
#' 
#' where k is the mean number of (pairwise) ratings per object and mean(\eqn{\rho}{rho}) is 
#' calculated weighted, according to Taylor (1987), since the pairwise are 
#' possibly based on a different number of ratings, what must be reflected 
#' in weights.
#' In case of complete datasets, it yields the same results 
#' as usual implementations of Kendall's W, except for tied ranks. In case 
#' of tied ranks, the (pairwise) correction of s used, which (already with 
#' complete datasets) results in slightly different values than the tie 
#' correction explicitly specified for W.
#' 
#' @param x \eqn{n \times m}{k x m} matrix or dataframe, k subjects (in rows) m
#' raters (in columns)
#' @param correct a logical indicating whether the coefficient should be
#' corrected for ties within raters (default \code{FALSE})
#' @param test a logical indicating whether the test statistic and p-value
#' should be reported (default \code{FALSE})
#' @param na.rm deprecated and ignored
#'
#' @return if \code{test = FALSE}, a numeric scalar containing Kendall's W.
#' Otherwise an object of class \code{"htest"} with components:
#' \describe{
#'   \item{\code{statistic}}{chi-squared test statistic}
#'   \item{\code{p.value}}{p-value for the test}
#'   \item{\code{method}}{description of the test and coefficient variant}
#'   \item{\code{data.name}}{name of the data}
#'   \item{\code{estimate}}{coefficient of concordance W}
#'   \item{\code{parameter}}{degrees of freedom and numbers of subjects and raters}
#' }
#' 
#' @note Based on code by Matthias Gamer and Markus Brueckl, 
#' adapted to conform to package standards.
#' 
#' @references Kendall, M.G. (1948) \emph{Rank correlation methods}. London:
#' Griffin.
#' 
#' Kendall, M.G. (1962). Rank correlation methods (3rd ed.). London: Griffin.
#' 
#' Brueckl, M. (2011). Statistische Verfahren zur Ermittlung der 
#' Urteileruebereinstimmung. in: Altersbedingte Veraenderungen der 
#' Stimme und Sprechweise von Frauen, Berlin: Logos, 88-103.
#' 
#' Taylor, J.M.G. (1987). Kendall's and Spearman's correlation coefficients in the presence of a blocking variable. \emph{Biometrics}, 43, 409-416.
#' 
#' @examples
#' 
#' anxiety <- data.frame(rater1=c(3,3,3,4,5,5,2,3,5,2,2,6,1,5,2,2,1,2,4,3),
#'                       rater2=c(3,6,4,6,2,4,2,4,3,3,2,3,3,3,2,2,1,3,3,4),
#'                       rater3=c(2,1,4,4,3,2,1,6,1,1,1,2,3,3,1,1,3,3,2,2))
#' 
#' kendallW(anxiety, TRUE)
#' 
#' # with test results
#' kendallW(anxiety, TRUE, test=TRUE)
#' 
#' # example from Siegel and Castellan (1988)
#' d.att <- data.frame(
#'   id        = c(4,21,11),
#'   airfare   = c(5,1,4),
#'   climate   = c(6,7,5),
#'   season    = c(7,6,1),
#'   people    = c(1,2,3),
#'   program   = c(2,3,2),
#'   publicity = c(4,5,7),
#'   present   = c(3,4,6),
#'   interest  = c(8,8,8)
#' )
#' 
#' kendallW(t(d.att[, -1]), test = TRUE)
#' 
#' # which is perfectly the same as
#' friedman.test(y=as.matrix(d.att[,-1]), groups = d.att$id)
#' 
#' 
#' @seealso [stats::cor], [stats::friedman.test]
#'
#' @family assoc.ordinal  
#' @concept association-measure  
#' @concept ordinal  
#' @concept interrater-agreement
#'
#'
#' @export
kendallW <- function(x, correct=FALSE, test=FALSE, na.rm=NULL) {
  
  # see also old Jim Lemon function kendall.w
  # other solution: library(irr);  kendall(ratings, correct = TRUE)
  # http://www.real-statistics.com/reliability/kendalls-w/
  
  if(!is.null(na.rm))
    warning("na.rm is not longer supported, see help!")
  
  dname <- deparse(substitute(x))
  
  ratings <- as.matrix(x)
  ns <- nrow(ratings)  # number of subjects
  nr <- ncol(ratings)  # number of raters
  
  # check for NAs and escalate to
  # Brueckl, M. (2011). Statistische Verfahren zur Ermittlung der Urteileruebereinstimmung. in: Altersbedingte Veraenderungen der Stimme und Sprechweise von Frauen, Berlin: Logos, 88-103.
  if(sum(is.na(ratings)) > 0){
    
    # no correction required
    TIES <- FALSE
    
    N <- nrow(ratings)
    m <- ncol(ratings)
    
    rho <- naReplace(stats::cor(ratings, method = "spearman", 
                               use = "pairwise.complete"), 0)
    
    w <- t(!is.na(ratings)) %*% (!is.na(ratings)) -1 
    w <- pmax(0, w[lower.tri(w)])
    wsum <- sum(w, na.rm = TRUE)
    
    kq <- mean(apply(!is.na(ratings), 1, sum), na.rm = TRUE)
    
    wmean_rho <- sum(rho[lower.tri(rho)] * w, na.rm = TRUE) / wsum
    
    coeff.name <- "W (generalized)"
    coeff <- (1 + wmean_rho * (kq - 1))/kq
    #test statistic
    stat <- kq * (N - 1) * coeff
    
  }  else {
    
    ratings.rank <- apply(ratings,2,rank)
    
    #Without correction for ties
    if (!correct) {
      
      #Test for ties
      TIES = FALSE
      testties <- apply(ratings, 2, unique)
      if (!is.matrix(testties)) 
        TIES = TRUE
      else { 
        if (length(testties) < length(ratings)) 
          TIES = TRUE 
      }
      
      coeff.name <- "W"
      coeff <- (12*var(apply(ratings.rank,1,sum))*(ns-1))/(nr^2*(ns^3-ns))
    }
    else { #With correction for ties
      
      Tj <- 0
      for (i in seq_len(nr)) {
        rater <- table(ratings.rank[,i])
        ties  <- rater[rater>1]
        l 	  <- as.numeric(ties)
        Tj	  <- Tj + sum(l^3-l)
      }
      
      coeff.name <- "W (with ties correction)"
      coeff <- (12*var(apply(ratings.rank,1,sum))*(ns-1))/(nr^2*(ns^3-ns)-nr*Tj)
    }
    
    #test statistic
    stat  <- nr * (ns-1) * coeff
    
  }
  
  if(test){
    p.value <- pchisq(stat, ns-1, lower.tail = FALSE)
    method <- paste("Kendall's coefficient of concordance", coeff.name)
    
    rval <- list(
      estimate = setNamesX(coeff, "W"), 
      parameter=c(df=ns-1, subjects=ns, raters=nr),
      statistic = setNamesX(stat, "Kendall chi-squared"), 
      p.value = p.value,
      alternative = "W is greater 0", 
      method = method, 
      data.name = dname)
    
    class(rval) <- "htest"
    
  } else {
    rval <- coeff
  }
  
  if (!correct && TIES) warning("Coefficient may be incorrect due to ties, consider setting correct = TRUE!")
  
  return(rval)
  
}
