
#' Scheffe Test for Pairwise and Otherwise Comparisons 
#' 
#' Scheffe's method applies to the set of estimates of all possible contrasts
#' among the factor level means, not just the pairwise differences considered
#' by Tukey's method.   
#' 
#' @name scheffeTest
#' @aliases scheffeTest scheffeTest.default scheffeTest.aov scheffeTest.formula
#' 
#' @inheritParams Formulas
#' 
#' @param x either a fitted model object, usually an \code{\link{aov}} fit,
#' when g is left to \code{NULL} or a response variable to be evalutated by g
#' (which mustn't be \code{NULL} then). 
#' @param g the grouping variable. 
#' @param which character vector listing terms in the fitted model for which
#' the intervals should be calculated. Defaults to all the terms. 
#' @param contrasts a \eqn{r \times c}{r x c} matrix containing the contrasts
#' to be computed, while \code{r} is the number of factor levels and \code{c}
#' the number of contrasts. Each column must contain a full contrast ("sum")
#' adding up to 0. Note that the argument \code{which} must be defined, when
#' non default contrasts are used.  Default value of \code{contrasts} is
#' \code{NULL}. In this case all pairwise contrasts will be reported. 
#' @param conf.level numeric value between zero and one giving the confidence
#' level to use.  If this is set to NA, just a matrix with the p-values will be
#' returned. 
#' @param \dots further arguments, currently not used. 
#' 
#' @return A list of classes \code{c("PostHocTest")}, with one component for
#' each term requested in \code{which}. Each component is a matrix with columns
#' \code{diff} giving the difference in the observed means, \code{lwr.ci}
#' giving the lower end point of the interval, \code{upr.ci} giving the upper
#' end point and \code{pval} giving the p-value after adjustment for the
#' multiple comparisons.
#' 
#' There are print and plot methods for class \code{"PostHocTest"}. The plot
#' method does not accept \code{xlab}, \code{ylab} or \code{main} arguments and
#' creates its own values for each plot.
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{pairwise.t.test}}, \code{\link{TukeyHSD}} 
#' @references Robert O. Kuehl, Steel R. (2000) \emph{Design of experiments}.
#' Duxbury
#' 
#' Steel R.G.D., Torrie J.H., Dickey, D.A. (1997) \emph{Principles and
#' Procedures of Statistics, A Biometrical Approach}. McGraw-Hill
#' @keywords htest
#' @examples
#' 
#' fm1 <- aov(breaks ~ wool + tension, data = warpbreaks)
#' scheffeTest(x=fm1)
#' scheffeTest(x=fm1, which="tension")
#' 
#' # some special contrasts
#' y <- c(7,33,26,27,21,6,14,19,6,11,11,18,14,18,19,14,9,12,6,
#'        24,7,10,1,10,42,25,8,28,30,22,17,32,28,6,1,15,9,15,
#'        2,37,13,18,23,1,3,4,6,2)
#' group <- factor(c(1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,3,3,3,3,3,
#'        3,3,3,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6))
#' 
#' r.aov <- aov(y ~ group)
#' scheffeTest(r.aov, contrasts=matrix( c(1,-0.5,-0.5,0,0,0,
#'                                        0,0,0,1,-0.5,-0.5), ncol=2) )
#' 
#' # just p-values:
#' scheffeTest(r.aov, conf.level=NA)
#' 


#' @rdname scheffeTest
#' @export
scheffeTest <- function (x, ...)
  UseMethod("scheffeTest")


#' @rdname scheffeTest
#' @export
scheffeTest.default <- function (x, g = NULL, which = NULL, contrasts = NULL, conf.level = 0.95, ...) {
  scheffeTest(x=aov(x ~ g), which=which, contrasts=contrasts, conf.level=conf.level, ...)
}


#' @rdname scheffeTest
#' @export
scheffeTest.formula <- function (formula, data, subset, na.action, ...) {
  scheffeTest(aov(formula, data, subset, na.action, ...))
}  


#' @rdname scheffeTest
#' @export
scheffeTest.aov <- function(x, which=NULL, contrasts = NULL, conf.level=0.95, ...){
  
  mm <- model.tables(x, "means")
  if (is.null(mm$n))
    stop("no factors in the fitted model")
  tabs <- mm$tables[-1L]
  
  if(is.null(which)) which <- seq_along(tabs)
  
  tabs <- tabs[which]
  nn <- mm$n[names(tabs)]
  nn_na <- is.na(nn)
  if (all(nn_na))
    stop("'which' specified no factors")
  if (any(nn_na)) {
    warning("'which' specified some non-factors which will be dropped")
    tabs <- tabs[!nn_na]
    nn <- nn[!nn_na]
  }
  out <- setNamesX(vector("list", length(tabs)), names(tabs))
  MSE <- sum(x$residuals^2)/x$df.residual
  
  autoContr <- is.null(contrasts)
  if(!is.null(contrasts)){
    contrasts <- data.frame(contrasts)
  }
  
  # nm <- "tension"
  for (nm in names(tabs)) {
    tab <- tabs[[nm]]
    means <- as.vector(tab)
    
    nms <- if (length(d <- dim(tab)) > 1L) {
      dn <- dimnames(tab)
      apply(do.call("expand.grid", dn), 1L, paste, collapse = ":")
    } else names(tab)
    
    n <- nn[[nm]]
    if (length(n) < length(means))
      n <- rep.int(n, length(means))
    
    if(autoContr) contrasts <- .contrasts(nms)
    
    psi <- apply(contrasts * means, 2, sum)
    sscoeff <- apply(contrasts * contrasts / n, 2, sum)
    
    # Corrected by Daniel Wollschlaeger 9.9.2014:
    #     psi <- contrasts %*% means
    #     sscoeff <- contrasts * contrasts %*% (1/n)
    
    dferr <- x$df.residual
    dfgrp <- length(x$residuals) - dferr - 1
    
    pval <- pf(psi^2/(MSE*sscoeff*dfgrp),
               df1=dfgrp, df2=dferr, lower.tail=FALSE)
    
    critvalue <- dfgrp * qf(1-conf.level, dfgrp, dferr, lower.tail=FALSE)
    
    lwr <- psi - sqrt(critvalue) * sqrt(MSE * sscoeff)
    upr <- psi + sqrt(critvalue) * sqrt(MSE * sscoeff)
    
    out[[nm]] <- cbind(diff=psi, lwr, upr, pval)
    colnames(out[[nm]]) <- c("diff","lwr.ci","upr.ci","pval")
    
    if(!autoContr) {
      # define contrasts rownames
      rownames(out[[nm]]) <-  apply(contrasts, 2, function(x)
        gettextf("%s-%s", paste(nms[x>0], collapse=","),
                 paste(nms[x<0], collapse=",")) )
      if(is.na(conf.level)) out[[nm]] <- out[[nm]][,-c(2:3)]
    }
    
    if(autoContr & is.na(conf.level)) {
      out[[nm]] <- matrix(NA, nrow=length(means), ncol=length(means))
      out[[nm]][lower.tri(out[[nm]], diag = FALSE)] <- pval
      dimnames(out[[nm]]) <- list(nms, nms)
      out[[nm]] <- out[[nm]][-1, -ncol(out[[nm]])]
    }
    
  }
  
  class(out) <- c("PostHocTest")
  attr(out, "orig.call") <- x$call
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- "Scheffe Test"
  attr(out, "method.str") <- gettextf("\n  Posthoc multiple comparisons of means: %s \n", attr(out, "method"))
  
  
  return(out)
  
}




# == internal helper functions ===============================================

.contrasts <- function (levs) {
  
  # A matrix with all possible pairwise contrasts, that can be built 
  # with the given levels.

  k = length(levs)
  M = data.frame(levs = levs)
  for (i in 1:(k - 1)) {
    for (j in (i + 1):k) {
      con = rep(0, k)
      con[i] = -1
      con[j] = 1
      nm = paste(levs[j], levs[i], sep = "-")
      M[[nm]] = con
    }
  }
  row.names(M) = levs
  
  return(M[-1])
  
}

