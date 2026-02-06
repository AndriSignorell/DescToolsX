
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
#' The \code{print} logical flag only has an effect when \code{layout=1}.
#' 
#' @name freq2D
#' @aliases freq2D freq2D.default freq2D.formula
#' @param x a vector of x values, or a data frame whose first two columns
#' contain the x and y values.
#' @param y a vector of y values.
#' @param formula a \code{\link{formula}}, such as \code{y~x}.
#' @param data a \code{data.frame}, \code{matrix}, or \code{list} from which
#' the variables in \code{formula} should be taken.
#' @param subset an optional vector specifying a subset of observations to be
#' used.
#' @param n the desired number of bins for the output, a scalar or a vector of
#' length 2.
#' @param pad number of rows and columns to add to each margin, containing only
#' zeros.
#' @param dnn the names to be given to the dimensions in the result.
#' @param \dots named arguments to be passed to the default method.
#' 
#' @return The \code{layout} argument specifies one of the following formats
#' for the binned frequency output:
#' 
#' \enumerate{ \item\code{matrix} that is easy to read, aligned like a
#' scatterplot.  \item\code{list} with three elements (x, y, matrix) that can
#' be passed to various plotting functions.  \item\code{data.frame} with three
#' columns (x, y, frequency) that can be analyzed further.  }
#' @author Arni Magnusson <thisisarni@@gmail.com>>
#' @seealso \code{\link{cut}}, \code{\link{table}}, and
#' \code{\link{print.table}} are the basic underlying functions.\cr
#' \code{\link{Freq}}, \code{\link{PercTable}}
#' @keywords dplot manip distribution multivariate
#' @examples
#' 
#' freq2D(quakes$long, quakes$lat, dnn="")
#' freq2D(lat ~ long, quakes, n=c(10, 20), pad=1)
#' 
#' # range(freq2D(saithe, print=FALSE))
#' 
#' # Layout, plot
#' # freq2D(saithe, layout=2)
#' # freq2D(saithe, layout=3)
#' # contour(freq2D(saithe, layout=2))
#' # lattice::contourplot(Freq ~ Bio + HR, freq2D(saithe,layout=3))


#' @rdname freq2D
#' @export
freq2D <- function(x, ...)
{
  UseMethod("freq2D")
}


#' @rdname freq2D
#' @export
freq2D.formula <- function(formula, data, subset, ...) {
  
  m <- match.call(expand.dots=FALSE)
  if(is.matrix(eval(m$data,parent.frame())))
    m$data <- as.data.frame(data)
  m$... <- NULL
  m[[1L]] <- as.name("model.frame")
  mf <- eval(m, parent.frame())
  
  freq2D.default(mf[2:1], ...)
  
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
    colnames(tmp)[c(1,ncol(tmp))] <- as.numeric(colnames(z)[c(1,ncol(z))]) + c(-xstep,xstep)
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

