
#' Lorenz Curve
#' 
#' lc computes the (empirical) ordinary and generalized Lorenz curve of a
#' vector x. Desc calculates some key figures for a Lorenz curve and produces a
#' quick description.
#' 
#' \code{lc(x)} computes the empirical ordinary Lorenz curve of \code{x} as
#' well as the generalized Lorenz curve (= ordinary Lorenz curve * mean(x)).
#' The result can be interpreted like this: \code{p}*100 percent have
#' \code{L(p)}*100 percent of \code{x}.
#' 
#' If \code{n} is changed to anything but the default \code{x} is interpreted
#' as a vector of class means and \code{n} as a vector of class frequencies: in
#' this case \code{lc} will compute the minimal Lorenz curve (= no inequality
#' within each group).
#' 
#' @name Lc
#' @aliases lc Lorenz curve lc.default lc.formula plot.lc plot.lclist lines.lc
#' predict.lc
#' @param x a vector containing non-negative elements, or a lc-object for plot
#' and lines.
#' @param \dots further argument to be passed to methods.
#' 
#' @return A list of class \code{"lc"} with the following components:
#' \item{p}{vector of percentages} \item{L}{vector with values of the ordinary
#' Lorenz curve} \item{L.general}{vector with values of the generalized Lorenz
#' curve} \item{x}{the original x values (needed for computing confidence
#' intervals)} \item{n}{the original n values}
#' 
#' @note These functions were previously published as \code{lc()} in the
#' \pkg{ineq} package and have been integrated here without logical changes.
#' 
#' @author Achim Zeileis <Achim.Zeileis@@R-project.org>, extensions Andri
#' Signorell <andri@@signorell.net>
#' 
#' @seealso The original location \code{\link[ineq]{Lc}()}, \cr inequality
#' measures \code{\link{gini}()}, \code{\link{atkinson}()}
#' 
#' @references Arnold, B. C. (1987) Majorization and the Lorenz Order: A Brief
#' Introduction, \emph{Springer}
#' 
#' Cowell, F. A. (2000) Measurement of Inequality in Atkinson, A. B. /
#' Bourguignon, F. (Eds): \emph{Handbook of Income Distribution}. Amsterdam.
#' 
#' Cowell, F. A. (1995) Measuring Inequality \emph{Harvester Wheatshef:
#' Prentice Hall}.
#' 
#' @family topic.Inequality
#' @concept Inequality
#' @concept Lorenz Curve
#' 
#' @examples
#' 
#' priceCarpenter <- d.pizza$price[d.pizza$driver=="Carpenter"]
#' priceMiller <- d.pizza$price[d.pizza$driver=="Miller"]
#' 
#' # compute the Lorenz curves
#' lc.p <- lc(priceCarpenter, na.rm=TRUE)
#' lc.u <- lc(priceMiller, na.rm=TRUE)
#' plot(lc.p)
#' lines(lc.u, col=2)
#' 
#' # the picture becomes even clearer with generalized Lorenz curves
#' plot(lc.p, general=TRUE)
#' lines(lc.u, general=TRUE, col=2)
#' 
#' # inequality measures emphasize these results, e.g. Atkinson's measure
#' atkinson(priceCarpenter, na.rm=TRUE)
#' atkinson(priceMiller, na.rm=TRUE)
#' 
#' 
#' # income distribution of the USA in 1968 (in 10 classes)
#' # x vector of class means, n vector of class frequencies
#' x <- c(541, 1463, 2445, 3438, 4437, 5401, 6392, 8304, 11904, 22261)
#' n <- c(482, 825, 722, 690, 661, 760, 745, 2140, 1911, 1024)
#' 
#' # compute minimal Lorenz curve (= no inequality in each group)
#' lc.min <- lc(x, n=n)
#' plot(lc.min)
#' 
#' 
#' # input of frequency tables with midpoints of classes
#' fl <- c(2.5,7.5,15,35,75,150)   # midpoints
#' n  <- c(25,13,10,5,5,2)	        # frequencies
#' 
#' plot(lc(fl, n),                 # Lorenz-Curve
#'      panel.first=grid(10, 10),
#'      main="Lorenzcurve Farmers",
#'      xlab="Percent farmers (cumulative)",
#'      ylab="Percent of area (%)"
#' )
#' # add confidence band
#' lines(lc(fl, n), conf.level=0.95,
#'       args.cband=list(col=SetAlpha(DescToolsOptions("col")[2], 0.3)))
#' 
#' gini(x=fl, weights=n)
#' 
#' # find specific function values using predict
#' x <- c(1,1,4)
#' lx <- lc(x)
#' plot(lx)
#' 
#' # get interpolated function value at p=0.55
#' y0 <- predict(lx, newdata=0.55)
#' abline(v=0.55, h=y0$L, lty="dotted")
#' 
#' # and for the inverse question use approx
#' y0 <- approx(x=lx$L, y=lx$p, xout=0.6)
#' abline(h=0.6, v=y0$y, col="red")
#' 
#' text(x=0.1, y=0.65, label=expression(L^{-1}*(0.6) == 0.8), col="red")
#' text(x=0.65, y=0.2, label=expression(L(0.55) == 0.275))
#' 
#' # input of frequency tables with midpoints of classes
#' fl <- c(2.5,7.5,15,35,75,150)     # midpoints
#' n  <- c(25,13,10,5,5,2)           # frequencies
#' 
#' # the formula interface for lc
#' lst <- lc(count ~ cut(price, breaks=5), data=d.pizza)
#' 
#' # plot(lst, col=1:length(lst), panel.first=grid(), lwd=2)
#' # legend(x="topleft", legend=names(lst), fill=1:length(lst))
#' 


#' @rdname Lc
#' @export
lc <- function(x, ...)
  UseMethod("lc")


#' @inheritParams Formulas
#' @rdname Lc
#' @export
lc.formula <- function(formula, data, subset, na.action, ...) {
  
  # this is taken basically from wilcox.test.formula
  
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  #   mf$na.action <- substitute(na.action)
  #   DNAME <- paste(names(mf), collapse = " by ")
  #
  #   DATA <- list(table(mf))
  #   do.call("lc", c(DATA, list(...)))
  drop <- TRUE
  #   mf <- model.frame(x, data)
  x <- split(x = mf[,1], f = mf[,2], drop=drop, ...)
  
  res <- lapply(x, FUN = "lc", ...)
  class(res) <- "lclist"
  
  return(res)
  
}

#' @param n a vector of frequencies, must be same length as \code{x}.
#' @param na.rm logical. Should missing values be removed? Defaults to FALSE.

#' @rdname Lc
#' @export
lc.default <- function(x, n = rep(1, length(x)), na.rm = FALSE, ...) {
  
  xx <- x
  nn <- n
  
  g <- gini(x, weights=n, na.rm=na.rm)
  
  if(na.rm) x <- na.omit(x)
  if (any(is.na(x)) || any(x < 0)) return(NA_real_)
  
  k <- length(x)
  o <- order(x)
  x <- x[o]
  n <- n[o]
  x <- n*x
  p <- cumsum(n)/sum(n)
  L <- cumsum(x)/sum(x)
  p <- c(0,p)
  L <- c(0,L)
  L2 <- L * mean(x)
  lc <- list(p, L, L2, g, xx, nn)
  names(lc) <- c("p", "L", "L.general", "Gini", "x", "n")
  class(lc) <- "lc"
  
  # no plot anymore, we have plot(lc) and Desc(lc, plotit=TRUE)
  # if(plot) plot(lc)
  lc
}



#' @param object object of class inheriting from "lc"
#' @param newdata an optional vector of percentages p for which to predict. If
#' omitted, the original values of the object are used.
#' @param conf.level confidence level for the bootstrap confidence interval.
#' Set this to \code{NA}, if no confidence band should be plotted.  Default is
#' \code{NA}. 
#' @param general logical. If \code{TRUE} the empirical Lorenz curve will be
#' plotted.

#' @rdname Lc
#' @export
predict.lc <- function(object, newdata, conf.level=NA, general=FALSE, n=1000, ...){
  
  confint.lc <- function(object, conf.level = 0.95, general=FALSE, n=1000, ...){
    
    x <- rep(object$x, times=object$n)
    m <- replicate(n = n, sample(x, replace = TRUE))
    
    lst <- apply(m, 2, lc)
    
    list(x=lst[[1]]$p,
         lci=apply(do.call(rbind, lapply(lst, "[[", ifelse(general, "L.general", "L"))), 
                   2, quantile, probs=(1-conf.level)/2),
         uci=apply(do.call(rbind, lapply(lst, "[[", ifelse(general, "L.general", "L"))), 
                   2, quantile, probs=1-(1-conf.level)/2)
    )
  }
  
  if(!general)
    L <- object$L
  else
    L <- object$L.general
  
  
  if(missing(newdata)){
    newdata <- object$p
    res <- data.frame(p=object$p, L=L)
  } else {
    res <- do.call(data.frame, approx(x=object$p, y=L, xout=newdata))
    colnames(res) <- c("p", "L")
  }
  
  if(!identical(conf.level, NA)){
    
    ci <- confint.lc(object, conf.level=conf.level, general=general, n=n)
    
    lci <- approx(x=ci$x, y=ci$lci, xout=newdata)
    uci <- approx(x=ci$x, y=ci$uci, xout=newdata)
    
    res <- data.frame(res, lci=lci$y, uci=uci$y)
    
  }
  
  res
  
}


#' @rdname Lc
#' @export
plot.lc <- function(x, ...){
  .notThere(x)
}



#' @rdname Lc
#' @export
lines.lc <- function(x, ...){
  .notThere(x)
}



