
#' Bivariate (Two-Dimensional) Frequency Distribution
#' 
#' Calculate a frequency distribution for two continuous variables.
#' 
#' The exact number of bins is determined by the \code{\link{pretty}} function,
#' based on the value of \code{n}.
#' 
#' Padding the margins with zeros can be helpful for subsequent analysis, such
#' as smoothing.
#' 
#' @name freq2D
#' @aliases freq2D freq2D.default freq2D.formula
#' 
#' @param x a vector of x values, or a data frame whose first two columns
#' contain the x and y values
#' @param y a vector of y values
#' @param formula a \code{\link{formula}}, such as \code{y ~ x}
#' @param data a \code{data.frame}, \code{matrix}, or \code{list} from which
#' the variables in \code{formula} should be taken
#' @param subset an optional vector specifying a subset of observations to be
#' used
#' @param na.action a function which indicates what should happen when the data
#' contain missing values. Defaults to \code{getOption("na.action")}.
#' @param n the desired number of bins for the output, a scalar or a vector of
#' length 2
#' @param pad number of rows and columns to add to each margin, containing only
#' zeros
#' @param dnn names for the dimensions in the result
#' @param \dots named arguments passed to the default method
#' 
#' @return a frequency matrix whose rows represent the y bins in descending
#' order and whose columns represent the x bins
#' 
#' @note Based on code by Arni Magnusson, adapted to conform to package standards.
#' 
#' @seealso \code{\link{cut}}, \code{\link{table}}, and
#' \code{\link{print.table}} are the basic underlying functions.\cr
#' \code{\link{freq}}, \code{\link{percTable}}
#' @examples
#' 
#' freq2D(quakes$long, quakes$lat, dnn="")
#' freq2D(lat ~ long, quakes, n=c(10, 20), pad=1)
#' 
#' @rdname freq2D
#' @family frequency  
#' @concept frequency-table
#'
#' @export
freq2D <- function(x, ...)
{
  UseMethod("freq2D")
}


#' @rdname freq2D
#' @export
freq2D.formula <- function(formula, data, subset, na.action, 
                           n=20, pad=0, dnn=NULL, ...) {
  
  if (missing(formula) || length(formula) != 3L)
    stop("'formula' missing or incorrect")
  
  ## IMPORTANT!!
  ## --- capture subset / na.action HERE ---
  subset_expr <- if (!missing(subset)) substitute(subset) else NULL
  na_expr     <- if (!missing(na.action)) substitute(na.action) else NULL
  
  pf <- resolveFormula(
    formula   = formula,
    data      = data,
    subset    = subset_expr,
    na.action = na_expr,
    allowed   = "numeric-numeric"
  )
  
  y <- do.call(
    freq2D,
    c(
      list(x = pf$mf[2:1], n = n, pad = pad, dnn = dnn),
      list(...)
    )
  )
  attr(y, "data.name") <- pf$data.name

  y
  
}





#' @rdname freq2D
#' @export
freq2D.default <- function(x, y, n=20, pad=0, dnn=NULL, ...) {
  
  dnn <- if(!is.null(dnn)) rep(dnn,length.out=2) else NULL
  xname <- dnn[1]
  yname <- dnn[2]
  
  ## 1  Extract data
  if(is.matrix(x))
    x <- as.data.frame(x)
  if(is.list(x))  # data.frame or list
  {
    xname <- if(is.null(xname)) names(x)[1] else xname
    yname <- if(is.null(yname)) names(x)[2] else yname
    y <- x[[2]]
    x <- x[[1]]
  }
  
  ## 2  Create grid
  n <- rep(n, length.out=2)
  xmid <- pretty(x, n=n[1])
  xstep <- diff(xmid)[1]
  xgrid <- c(xmid-0.5*xstep, max(xmid)+0.5*xstep)
  ymid <- pretty(y, n=n[2])
  ystep <- diff(ymid)[1]
  ygrid <- c(ymid-0.5*ystep, max(ymid)+0.5*ystep)
  
  ## 3  Map data on grid
  xfac <- cut(x, xgrid, include.lowest=TRUE, labels=format(xmid))
  if(is.null(xname))
    xname <- deparse(substitute(x))
  yfac <- cut(y, ygrid, include.lowest=TRUE, labels=format(ymid))
  if(is.null(yname))
    yname <- deparse(substitute(y))
  z <- table(xfac, yfac, dnn=c(xname,yname))
  
  ## 4  Remove existing edges with only zeros
  z <- z[cumsum(rowSums(z))>0, cumsum(colSums(z))>0]
  z <- z[rev(cumsum(rev(rowSums(z))))>0, rev(cumsum(rev(colSums(z))))>0]
  
  ## 5  Add edges with only zeros
  for(i in seq_len(pad))
  {
    tmp <- cbind(0, rbind(0, z, 0), 0)
    rownames(tmp)[c(1,nrow(tmp))] <- as.numeric(rownames(z)[c(1,nrow(z))]) + c(-xstep,xstep)
    colnames(tmp)[c(1,ncol(tmp))] <- as.numeric(colnames(z)[c(1,ncol(z))]) + c(-ystep,ystep)
    names(dimnames(tmp)) <- names(dimnames(z))
    z <- tmp
  }
  
  # ## 5  Prepare output
  # xnum <- as.numeric(rownames(z))
  # ynum <- as.numeric(colnames(z))
  
  t(z)[ncol(z):1,]
  
  # if(layout == 1)
  # {
  #   output <- t(z)[ncol(z):1,]
  #   if(print)
  #   {
  #     print.table(output, zero.print=".")
  #     return(invisible(output))
  #   }
  #   else
  #   {
  #     return(output)
  #   }
  # }
  # else if(layout == 2)
  # {
  #   output <- list(x=xnum, y=ynum, z=z)
  #   return(output)
  # }
  # else  # layout 3
  # {
  #   output <- data.frame(x=rep(xnum,length(ynum)), y=rep(ynum,each=length(xnum)), z=c(z))
  #   names(output) <- make.names(c(xname,yname,"Freq"), unique=TRUE)
  #   return(output)
  # }
  
}
