
#' Cohen's Kappa and Weighted Kappa
#' 
#' Computes the agreement rates Cohen's kappa and weighted kappa and their
#' confidence intervals.
#' 
#' Cohen's kappa is the diagonal sum of the (possibly weighted) relative
#' frequencies, corrected for expected values and standardized by its maximum
#' value. \cr The equal-spacing weights (see Cicchetti and Allison 1971) are
#' defined by \deqn{1 - \frac{|i - j|}{r - 1}} \code{r} being the number of
#' columns/rows, and the Fleiss-Cohen weights by \deqn{1 - \frac{(i - j)^2}{(r
#' - 1)^2}} The latter attaches greater importance to closer disagreements.\cr
#' \cr Data can be passed to the function either as matrix or data.frame in
#' \code{x}, or as two numeric vectors \code{x} and \code{y}. In the latter
#' case \code{table(x, y, ...)} is calculated. Thus \code{NA}s are handled the
#' same way as \code{\link{table}} does. Note that tables are by default
#' calculated \bold{without} NAs. The specific argument \code{useNA} can be
#' passed via the ... argument.\cr The vector interface \code{(x, y)} is only
#' supported for the calculation of unweighted kappa. This is because we cannot
#' ensure a safe construction of a confusion table for two factors with
#' different levels, which is independent of the order of the levels in
#' \code{x} and \code{y}. So weights might lead to inconsistent results. The
#' function will raise an error in this case.
#' 
#' @param x can either be a numeric vector or a confusion matrix. In the latter
#' case x must be a square matrix.
#' @param y \code{NULL} (default) or a vector with compatible dimensions to
#' \code{x}. If \code{y} is provided, \code{table(x, y, \dots)} is calculated.
#' In order to get a square matrix, \code{x} and \code{y} are coerced to
#' factors with synchronized levels. (Note, that the vector interface can not
#' be used together with weights.)
#' @param weights either one out of \code{"Unweighted"} (default),
#' \code{"Equal-Spacing"}, \code{"Fleiss-Cohen"}, which will calculate the
#' weights accordingly, or a user-specified matrix having the same dimensions
#' as x containing the weights for each cell.
#' @param conf.level confidence level of the interval. If set to \code{NA}
#' (which is the default) no confidence intervals will be calculated.
#' @param \dots further arguments are passed to the function
#' \code{\link{table}}, allowing i.e. to set \code{useNA}. This refers only to
#' the vector interface.
#' 
#' @return if no confidence intervals are requested: the estimate as numeric
#' value\cr\cr else a named numeric vector with 3 elements \item{est}{estimate}
#' \item{lci}{lower confidence interval} \item{uci}{upper confidence interval}
#' @author David Meyer <david.meyer@@r-project.org>, some changes and tweaks
#' Andri Signorell <andri@@signorell.net>
#' 
#' @seealso \code{\link{cronbachAlpha}}, \code{\link{kappaM}},
#' \code{\link{krippAlpha}}
#' 
#' @references Cohen, J. (1960) A coefficient of agreement for nominal scales.
#' \emph{Educational and Psychological Measurement}, 20, 37-46.
#' 
#' Everitt, B.S. (1968), Moments of statistics kappa and weighted kappa.
#' \emph{The British Journal of Mathematical and Statistical Psychology}, 21,
#' 97-103.
#' 
#' Fleiss, J.L., Cohen, J., and Everitt, B.S. (1969), Large sample standard
#' errors of kappa and weighted kappa. \emph{Psychological Bulletin}, 72,
#' 332-327.
#' 
#' Cicchetti, D.V., Allison, T. (1971) A New Procedure for Assessing
#' Reliability of Scoring EEG Sleep Recordings \emph{American Journal of EEG
#' Technology}, 11, 101-109.
#' 
#' @family Agreement
#' @concept Interrater Agreement
#' @concept Nominal Agreement
#' 
#' @examples
#' 
#' 
#' # from Bortz et. al (1990) Verteilungsfreie Methoden in der Biostatistik, Springer, pp. 459
#' m <- matrix(c(53,  5, 2,
#'               11, 14, 5,
#'                1,  6, 3), nrow=3, byrow=TRUE,
#'             dimnames = list(rater1 = c("V","N","P"), rater2 = c("V","N","P")) )
#' 
#' # confusion matrix interface
#' cohenKappa(m, weight="Unweighted")
#' 
#' # vector interface
#' x <- untable(m)
#' cohenKappa(x$rater1, x$rater2, weight="Unweighted")
#' 
#' # pairwise Kappa
#' rating <- data.frame(
#'   rtr1 = c(4,2,2,5,2, 1,3,1,1,5, 1,1,2,1,2, 3,1,1,2,1, 5,2,2,1,1, 2,1,2,1,5),
#'   rtr2 = c(4,2,3,5,2, 1,3,1,1,5, 4,2,2,4,2, 3,1,1,2,3, 5,4,2,1,4, 2,1,2,3,5),
#'   rtr3 = c(4,2,3,5,2, 3,3,3,4,5, 4,4,2,4,4, 3,1,1,4,3, 5,4,4,4,4, 2,1,4,3,5),
#'   rtr4 = c(4,5,3,5,4, 3,3,3,4,5, 4,4,3,4,4, 3,4,1,4,5, 5,4,5,4,4, 2,1,4,3,5),
#'   rtr5 = c(4,5,3,5,4, 3,5,3,4,5, 4,4,3,4,4, 3,5,1,4,5, 5,4,5,4,4, 2,5,4,3,5),
#'   rtr6 = c(4,5,5,5,4, 3,5,4,4,5, 4,4,3,4,5, 5,5,2,4,5, 5,4,5,4,5, 4,5,4,3,5)
#' )
#' 
#' pairApply(rating, FUN=cohenKappa, symmetric=TRUE)
#' 
#' # Weighted Kappa
#' cats <- c("<10%", "11-20%", "21-30%", "31-40%", "41-50%", ">50%")
#' m <- matrix(c(5,8,1,2,4,2, 3,5,3,5,5,0, 1,2,6,11,2,1,
#'               0,1,5,4,3,3, 0,0,1,2,5,2, 0,0,1,2,1,4), nrow=6, byrow=TRUE,
#'             dimnames = list(rater1 = cats, rater2 = cats) )
#' cohenKappa(m, weight="Equal-Spacing")
#' 
#' 
#' # supply an explicit weight matrix
#' ncol(m)
#' (wm <- outer(1:ncol(m), 1:ncol(m), function(x, y) {
#'         1 - ((abs(x-y)) / (ncol(m)-1)) } ))
#' cohenKappa(m, weight=wm, conf.level=0.95)
#' 
#' 
#' # however, Fleiss, Cohen and Everitt weight similarities
#' fleiss <- matrix(c(
#'   106, 10,  4,
#'   22,  28, 10,
#'    2,  12,  6
#'   ), ncol=3, byrow=TRUE)
#' 
#' #Fleiss weights the similarities
#' weights <- matrix(c(
#'  1.0000, 0.0000, 0.4444,
#'  0.0000, 1.0000, 0.6666,
#'  0.4444, 0.6666, 1.0000
#'  ), ncol=3)
#' 
#' cohenKappa(fleiss, weights)
#' 
#' 
#' # using the formula interface
#' d.long <- data.frame(
#'   subj = c("1", "2", "3", "4", "5", "1", "2", "3", "4", "5", 
#'            "1", "2", "3", "4", "5", "1", "2", "3", "4", "5"), 
#'   rater = c("rtr1", "rtr1", "rtr1", "rtr1", "rtr1", "rtr2", 
#'             "rtr2", "rtr2", "rtr2", "rtr2", "rtr3", "rtr3", 
#'             "rtr3", "rtr3", "rtr3", "rtr4", "rtr4", "rtr4", 
#'             "rtr4", "rtr4"), 
#'   rat = factor(c("V","V","V","V","P","V","N","V","V","P","V",
#'                  "P","V","V","P","V","V","V","V","N")))
#' 
#' cohenKappa(raterFrame(rat ~ subj | rater, d.long, 
#'                       subset=rater %in% c("rtr1","rtr2"))[, -1], 
#'            conf.level = 0.95)
#' 
#' 




#' @export
cohenKappa <- function (x, y = NULL, 
                        weights = c("Unweighted", "Equal-Spacing", "Fleiss-Cohen"), 
                        conf.level = NA, ...) {
  
  # originally based on Kappa from library(vcd)
  # author: David Meyer
  # see also: kappa in library(psych)
  
  
  if (is.character(weights)) 
    weights <- match.arg(weights)
  
  if (!is.null(y) & !identical(weights, "Unweighted")) {
    # we can not ensure a reliable weighted kappa for 2 factors with different levels
    # so refuse trying it... (unweighted is no problem)
    stop("Vector interface for weighted Kappa is not supported. Provide confusion matrix.")
  }
  
  x <- normalizeToConfusion(x=x, y=y, ...)
  
  d <- diag(x)
  n <- sum(x)
  nc <- ncol(x)
  colFreqs <- colSums(x)/n
  rowFreqs <- rowSums(x)/n
  
  kappa <- function(po, pc) {
    (po - pc)/(1 - pc)
  }
  
  std <- function(p, pc, k, W = diag(1, ncol = nc, nrow = nc)) {
    sqrt((sum(p * sweep(sweep(W, 1, W %*% colSums(p) * (1 - k)), 
                        2, W %*% rowSums(p) * (1 - k))^2) - 
            (k - pc * (1 - k))^2) / crossprod(1 - pc)/n)
  }
  
  if(identical(weights, "Unweighted")) {
    
    po <- sum(d)/n
    pc <- as.vector(crossprod(colFreqs, rowFreqs))
    k <- kappa(po, pc)
    s <- as.vector(std(x/n, pc, k))
    
  } else {  
    
    # some kind of weights defined
    W <- if (is.matrix(weights)) 
      weights
    
    else if (weights == "Equal-Spacing") 
      1 - abs(outer(1:nc, 1:nc, "-"))/(nc - 1)
    
    else # weights == "Fleiss-Cohen"
      1 - (abs(outer(1:nc, 1:nc, "-"))/(nc - 1))^2
    
    po <- sum(W * x)/n
    pc <- sum(W * colFreqs %o% rowFreqs)
    k <- kappa(po, pc)
    s <- as.vector(std(x/n, pc, k, W))
  }
  
  if (is.na(conf.level)) {
    res <- k
  } else {
    ci <- k + c(1, -1) * qnorm((1 - conf.level)/2) * s
    res <- c(est = k, lci = ci[1], uci = ci[2])
  }
  
  return(res)
  
}


# Use as test:
# https://online.stat.psu.edu/stat509/lesson/18/18.7

# The weighted kappa coefficient is 0.57 and the asymptotic 95% confidence 
# interval is (0.44, 0.70). This indicates that the amount of agreement 
# between the two radiologists is modest (and not as strong as the researchers 
# had hoped it would be).

# lbl<-c("Normal","Benign","Suspect","Cancer")
# m <- t(setNamesX(matrix(c(21,12,0,0,
#                           4,17,1,0,
#                           3,9,15,2,
#                           0,0,0,1), nrow=4), rownames=lbl, colnames=lbl))
# 
# # matrix interface

# FmCI(cohenKappa(m, conf.level = 0.95, weights = "E"), digits=2)
# expected: 0.57 [0.44, 0.70]


# cohenKappa(m, conf.level = 0.95)
# 
# # vector interface
# with(untable(m, colnames = c("rtr1","rtr2")), 
#      cohenKappa(rtr1, rtr2, conf.level = 0.95))
# 
# 
# # long data.frame interface
# d.wide <- appendRowNames(Untable(m, colnames = c("rtr1","rtr2")), 
#                          "subj")
# d.long <- reshape(d.wide,
#                   varying=2:3,
#                   idvar=c("subj"),
#                   times=colnames(d.wide)[2:3],
#                   v.names="rat", timevar="rater",
#                   direction="long",
#                   new.row.names=seq(prod(dim(d.wide))))
# 
# cohenKappa(rat ~ subj | rater, data=d.long, 
#            conf.level = 0.95)
# 




