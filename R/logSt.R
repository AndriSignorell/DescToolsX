
#' Started Logarithmic Transformation and Its Inverse 
#' 
#' Transforms the data by a log transformation, modifying small and zero
#' observations such that the transformation is linear for \eqn{x <=
#' threshold}{x <= threshold} and logarithmic for x > threshold. So the
#' transformation yields finite values and is continuously differentiable. 
#' 
#' In order to avoid \eqn{log(x) = -\infty}{log(x) = -inf} for \eqn{x=0} in
#' log-transformations there's often a constant added to the variable before
#' taking the \eqn{log}. This is not always a pleasable strategy. The function
#' \code{LogSt} handles this problem based on the following ideas: \itemize{
#' \item The modification should only affect the values for "small" arguments.
#' \item What "small" is should be determined in connection with the non-zero
#' values of the original variable, since it should behave well (be
#' equivariant) with respect to a change in the "unit of measurement". \item The
#' function must remain monotone, and it should remain (weakly) convex. } These
#' criteria are implemented here as follows: The shape is determined by a
#' threshold \eqn{c} at which - coming from above - the log function switches
#' to a linear function with the same slope at this point.
#' 
#' This is obtained by
#' 
#' \deqn{g(x) = }{g(x)=log_10(x), if x>c, log_10(c) - (c-x)/(c log(10)),
#' otherwise}\deqn{ \left\{\begin{array}{ll} }{g(x)=log_10(x), if x>c,
#' log_10(c) - (c-x)/(c log(10)), otherwise}\deqn{ log_{10}(x) &\textup{for }x
#' \ge c\\ }{g(x)=log_10(x), if x>c, log_10(c) - (c-x)/(c log(10)),
#' otherwise}\deqn{ log_{10}(c) - \frac{c - x}{c \cdot log(10)} &\textup{for }
#' x < c }{g(x)=log_10(x), if x>c, log_10(c) - (c-x)/(c log(10)),
#' otherwise}\deqn{ \end{array}\right. }{g(x)=log_10(x), if x>c, log_10(c) -
#' (c-x)/(c log(10)), otherwise}
#' 
#' Small values are determined by the threshold \eqn{c}. If not given by the
#' argument \code{threshold}, it is determined by the quartiles \eqn{q_1} and
#' \eqn{q_3} of the non-zero data as those smaller than \eqn{c =
#' \frac{q_1^{1+r}}{q_3^r}}{c=q_1^{1+r}/q_3^r} where \eqn{r} can be set by the
#' argument \code{mult}. The rationale is, that, for lognormal data, this
#' constant identifies 2 percent of the data as small.\cr Beyond this limit,
#' the transformation continues linear with the derivative of the log curve at
#' this point. \cr
#' 
#' Another idea for choosing the threshold \eqn{c} was: median(x) /
#' (median(x)/quantile(x, 0.25))^2.9)\cr\cr The function chooses \eqn{log_{10}}
#' rather than natural logs by default because they can be backtransformed
#' relatively easily in mind.
#' 
#' A generalized log (see: Rocke 2003) can be calculated in order to stabilize
#' the variance as: \preformatted{function (x, a) { return(log((x + sqrt(x^2 +
#' a^2)) / 2)) }}
#' 
#' @name logSt
#' @aliases logSt logStInv
#' @param x a vector or matrix of data, which is to be transformed 
#' @param base a positive or complex number: the base with respect to which
#' logarithms are computed. Defaults to 10. Use=exp(1) for natural log. 
#' @param calib a vector or matrix of data used to calibrate the
#' transformation(s), i.e., to determine the constant \eqn{c} needed 
#' @param threshold constant \eqn{c} that determines the transformation. The
#' inverse function \code{LogStInv} will look for an attribute named
#' \code{"threshold"} if the argument is set to \code{NULL}. 
#' @param mult a tuning constant affecting the transformation of small values,
#' see \code{Details}. 
#' 
#' @return the transformed data. The value \eqn{c} used for the transformation
#' and needed for inverse transformation is returned as
#' \code{attr(.,"threshold")} and the used base as \code{attr(.,"base")}.
#' 
#' @author Werner A. Stahel, ETH Zurich \cr slight modifications Andri
#' Signorell <andri@@signorell.net>
#' @seealso \code{\link{log}}, \code{\link{log10}}
#' @references Rocke, D M, Durbin B (2003): Approximate variance-stabilizing
#' transformations for gene-expression microarray data, \emph{Bioinformatics}.
#' 22;19(8):966-72.
#' @keywords math
#' @examples
#' 
#' dd <- c(seq(0,1,0.1), 5 * 10^rnorm(100, 0, 0.2))
#' dd <- sort(dd)
#' r.dl <- logSt(dd)
#' plot(dd, r.dl, type="l")
#' abline(v=attr(r.dl, "threshold"), lty=2)
#' 
#' x <- rchisq(df=3, n=100)
#' # should give 0 (or at least something small):
#' logStInv(logSt(x)) - x
#' 


#' @rdname logSt
#' @export
logSt <- function(x, base = 10, calib = x, threshold = NULL, mult = 1) {
  
  # original function logst in source regr
  #
  #   # Purpose:   logs of x, zeros and small values treated well
  #   # *********************************************************************
  #   # Author: Werner Stahel, Date:  3 Nov 2001, 08:22
  #   x <- cbind(x)
  #   calib <- cbind(calib)
  #   lncol <- ncol(calib)
  #   ljthr <- length(threshold) > 0
  #   if (ljthr) {
  #     if (!length(threshold) %in% c(1, lncol))
  #       stop("!LogSt! length of argument 'threshold' is inadequate")
  #     lthr <- rep(threshold, length=lncol)
  #     ljdt <- !is.na(lthr)
  #   } else {
  #     ljdt <- rep(TRUE, lncol)
  #     lthr <- rep(NA, lncol)
  #     for (lj in 1:lncol) {
  #       lcal <- calib[, lj]
  #       ldp <- lcal[lcal > 0 & !is.na(lcal)]
  #       if(length(ldp) == 0) ljdt[lj] <- FALSE else {
  #         lq <- quantile(ldp,probs = c(0.25,0.75), na.rm = TRUE)
  #         if(lq[1] == lq[2]) lq[1] <- lq[2]/2
  #         lthr[lj] <- lc <- lq[1]^(1 + mult) / lq[2]^mult
  #       }
  #     }
  #   }
  #   # transform x
  #   for (lj in 1:lncol) {
  #     ldt <- x[,lj]
  #     lc <- lthr[lj]
  #     li <- which(ldt < lc)
  #     if (length(li))
  #       ldt[li] <- lc * 10^((ldt[li] - lc) / (lc * log(10)))
  #     x[,lj] <- log10(ldt)
  #   }
  #   if (length(colnames(x)))
  #     lnmpd <- names(ljdt) <- names(lthr) <- colnames(x)  else
  #     lnmpd <- as.character(1:lncol)
  #
  #   attr(x,"threshold") <- c(lthr)
  #
  #   if (any(!ljdt)) {
  #     warning(':LogSt: no positive x for variables',lnmpd[!ljdt],
  #             '. These are not transformed')
  #     attr(x,"untransformed") <- c(ljdt)
  #   }
  #   x
  
  
  if(is.null(threshold)){
    lq <- quantile(calib[calib > 0], probs = c(0.25, 0.75), na.rm = TRUE)
    if (lq[1] == lq[2]) lq[1] <- lq[2]/2
    threshold <- lq[1]^(1 + mult)/lq[2]^mult
  }
  
  res <- rep(NA, length(x))
  idx <- (x < threshold)
  idx.na <- is.na(idx)
  res[idx & !idx.na] <- log(x = threshold, base=base) + ((x[idx & !idx.na] - threshold)/(threshold * log(base)))
  res[!idx & !idx.na] <- log(x = x[!idx & !idx.na], base=base)
  
  attr(res, "threshold") <- threshold
  attr(res, "base") <- base
  return(res)
  
}



#' @rdname logSt
#' @export
logStInv <- function (x, base=NULL, threshold = NULL) {
  
  if(is.null(threshold)) threshold <- attr(x, "threshold")
  if(is.null(base)) base <- attr(x, "base")
  
  res <- rep(NA, length(x))
  idx <- (x < (lgth <- log(threshold, base)))
  idx.na <- is.na(idx)
  res[ idx & !idx.na] <- threshold - (threshold * log(base)) * (lgth - x[idx & !idx.na]) 
  res[!idx & !idx.na] <- base^(x[!idx & !idx.na])
  
  return(res)
}
