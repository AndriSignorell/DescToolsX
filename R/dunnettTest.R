
#' Dunnett's Test for Comparing Several Treatments With a Control
#' 
#' Performs Dunnett's test for comparing several treatments with a control.
#' 
#' \code{dunnettTest} does the post hoc pairwise multiple comparisons
#' procedure.
#' 
#' If \code{x} is a list, its elements are taken as the samples to be compared,
#' and hence have to be numeric data vectors.  In this case, \code{g} is
#' ignored, and one can simply use \code{dunnettTest(x)} to perform the test.
#' If the samples are not yet contained in a list, use
#' \code{dunnettTest(list(x, ...))}.
#' 
#' Otherwise, \code{x} must be a numeric data vector, and \code{g} must be a
#' vector or factor object of the same length as \code{x} giving the group for
#' the corresponding elements of \code{x}.
#' 
#' @name dunnettTest
#' @aliases dunnettTest dunnettTest.default dunnettTest.formula
#' @param x a numeric vector of data values, or a list of numeric data vectors.
#' @param g a vector or factor object giving the group for the corresponding
#' elements of \code{x}.  Ignored if \code{x} is a list.
#' @param control the level of the control group against which the others
#' should be tested. If there are multiple levels the calculation will be
#' performed for every one.
#' @param conf.level confidence level of the interval. %% ~~Describe
#' \code{conf.level} here~~
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} gives
#' the data values and \code{rhs} the corresponding groups.
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the formula
#' \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations to be
#' used.
#' @param na.action a function which indicates what should happen when the data
#' contain \code{NA}s.  Defaults to \code{getOption("na.action")}.
#' @param \dots further arguments to be passed to or from methods.
#' @return A list of class \code{c("PostHocTest")}, containing one matrix named
#' after the control with columns \code{diff} giving the difference in the
#' observed means, \code{lwr.ci} giving the lower end point of the interval,
#' \code{upr.ci} giving the upper end point and \code{pval} giving the p-value
#' after adjustment for the multiple comparisons.
#' 
#' There are print and plot methods for class \code{"PostHocTest"}. The plot
#' method does not accept \code{xlab}, \code{ylab} or \code{main} arguments and
#' creates its own values for each plot.
#' @author Andri Signorell <andri@@signorell.net>, the interface is based on
#' R-Core code
#' @seealso \code{\link{postHocTest}}
#' @references Dunnett C. W. (1955) A multiple comparison procedure for
#' comparing several treatments with a control, \emph{Journal of the American
#' Statistical Association}, 50:1096-1121.
#' @keywords htest
#' @examples
#' 
#' ## Hollander & Wolfe (1973), 116.
#' ## Mucociliary efficiency from the rate of removal of dust in normal
#' ##  subjects, subjects with obstructive airway disease, and subjects
#' ##  with asbestosis.
#' x <- c(2.9, 3.0, 2.5, 2.6, 3.2) # normal subjects
#' y <- c(3.8, 2.7, 4.0, 2.4)      # with obstructive airway disease
#' z <- c(2.8, 3.4, 3.7, 2.2, 2.0) # with asbestosis
#' 
#' dunnettTest(list(x, y, z))
#' 
#' ## Equivalently,
#' x <- c(x, y, z)
#' g <- factor(rep(1:3, c(5, 4, 5)),
#'             labels = c("Normal subjects",
#'                        "Subjects with obstructive airway disease",
#'                        "Subjects with asbestosis"))
#' 
#' dunnettTest(x, g)
#' 
#' ## Formula interface
#' boxplot(Ozone ~ Month, data = airquality)
#' dunnettTest(Ozone ~ Month, data = airquality)
#' 
#' dunnettTest(Ozone ~ Month, data = airquality, control="8", conf.level=0.9)
#' 

#' @rdname dunnettTest
#' @export
dunnettTest <- function (x, ...)
  UseMethod("dunnettTest")



#' @rdname dunnettTest
#' @export
dunnettTest.formula <- function (formula, data, subset, na.action, ...) {
  
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- quote(stats::model.frame)
  m$... <- NULL
  mf <- eval(m, parent.frame())
  if (length(mf) > 2L)
    stop("'formula' should be of the form response ~ group")
  DNAME <- paste(names(mf), collapse = " by ")
  names(mf) <- NULL
  response <- attr(attr(mf, "terms"), "response")
  y <- do.call("dunnettTest", c(as.list(mf), list(...)))
  y$data.name <- DNAME
  y
  
}



#' @rdname dunnettTest
#' @export
dunnettTest.default <- function (x, g, control = NULL
                                 , conf.level = 0.95, ...) {
  
  if (is.list(x)) {
    if (length(x) < 2L) 
      stop("'x' must be a list with at least 2 elements")
    if (!missing(g)) 
      warning("'x' is a list, so ignoring argument 'g'")
    DNAME <- deparse1(substitute(x))
    x <- lapply(x, function(u) u <- u[complete.cases(u)])
    if (!all(sapply(x, is.numeric))) 
      warning("some elements of 'x' are not numeric and will be coerced to numeric")
    k <- length(x)
    l <- lengths(x)
    if (any(l == 0L)) 
      stop("all groups must contain data")
    g <- factor(rep.int(seq_len(k), l))
    x <- unlist(x)
  }
  else {
    if (length(x) != length(g)) 
      stop("'x' and 'g' must have the same length")
    DNAME <- paste(deparse1(substitute(x)), "and", deparse1(substitute(g)))
    OK <- complete.cases(x, g)
    x <- x[OK]
    g <- g[OK]
    g <- factor(g)
    k <- nlevels(g)
    if (k < 2L) 
      stop("all observations are in the same group")
  }
  N <- length(x)
  if (N < 2L) 
    stop("not enough observations")
  
  
  # just organisational stuff so far, got a fine x and g now
  
  if (is.null(control)) control <- levels(g)[1]
  
  ctrls <- control
  out <- list()
  
  for(ii in seq_along(ctrls)){
    
    control <- ctrls[ii]
    
    ni <- tapply(x, g, length)
    
    means <- tapply(x, g, mean)
    meandiffs <- means[names(means) != control] - means[control]
    
    fittedn <- ni[names(ni) != control]
    controln <- ni[control]
    
    s <- sqrt( sum(tapply(x, g, function(x) sum((x - mean(x))^2) )) /
                 (N - k))
    
    Dj <- meandiffs / (s * sqrt((1/fittedn) + (1/controln)))
    Rij <- sqrt(fittedn/(fittedn + controln))
    
    R <- outer(Rij, Rij, "*")
    diag(R) <- 1
    
    # Michael Chirico suggests in https://github.com/AndriSignorell/DescTools/pull/102
    withr::with_seed(5, {
      qvt <- mvtnorm::qmvt((1 - (1 - conf.level)/2), df = N - k, sigma = R, tail = "lower.tail")$quantile
    })
    
    # replaced by Michael Chirico's elegant solution
    # # store the given seed
    # if(exists(".Random.seed")){
    #   # .Random.seed might not exist when launched as background job
    #   # so only store and reset if it exists 
    #   old.seed <- .Random.seed
    # }
    # set.seed(5)  # for getting consistent results every run
    # qvt <- mvtnorm::qmvt((1 - (1 - conf.level)/2), df = N - k, sigma = R, tail = "lower.tail")$quantile
    # 
    # # reset seed
    # if(exists("old.seed")){
    #   .Random.seed <<- old.seed
    # }
    
    lower <- meandiffs - s * sqrt((1/fittedn) + (1/controln)) * qvt
    upper <- meandiffs + s * sqrt((1/fittedn) + (1/controln)) * qvt
    
    pval <- c()
    for (i in 1:(k-1)){
      pval[i] <- 1 - mvtnorm::pmvt(-abs(Dj[i]), abs(Dj[i]), corr=R, delta=rep(0, k-1), df=N - k)[1]
    }
    
    out[[ii]] <- cbind(diff=meandiffs, lower, upper, pval)
    dimnames(out[[ii]]) <- list(paste(names(meandiffs), control, sep="-"), c("diff", "lwr.ci", "upr.ci","pval"))
  }
  
  names(out) <- ctrls
  
  class(out) <- c("PostHocTest")
  #  attr(out, "orig.call") <- NA
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- ""
  attr(out, "method.str") <- gettextf("\n  Dunnett's test for comparing several treatments with a control : %s \n", attr(out, "method"))
  
  return(out)
  
}

