
#' Dunn's Test of Multiple Comparisons
#' 
#' Performs Dunn's test of multiple comparisons using rank sums.
#' 
#' \code{dunnTest} performs the post hoc pairwise multiple comparisons
#' procedure appropriate to follow the rejection of a Kruskal-Wallis test. The
#' Kruskal-Wallis test, being a non-parametric analog of the one-way ANOVA, is
#' an omnibus test of the null hypothesis that none of k groups stochastically
#' dominate one another. Dunn's test is constructed in part by summing jointly
#' ranked data. The rank sum test, itself a non-parametric analog of the
#' unpaired t-test, is possibly intuitive, but inappropriate as a post hoc
#' pairwise test, because (1) it fails to retain the dependent ranking that
#' produced the Kruskal-Wallis test statistic, and (2) it does not incorporate
#' the pooled variance estimate implied by the null hypothesis of the
#' Kruskal-Wallis test.
#' 
#' If \code{x} is a list, its elements are taken as the samples to be compared,
#' and hence have to be numeric data vectors.  In this case, \code{g} is
#' ignored, and one can simply use \code{dunnTest(x)} to perform the test.  If
#' the samples are not yet contained in a list, use \code{dunnTest(list(x,
#' ...))}.
#' 
#' Otherwise, \code{x} must be a numeric data vector, and \code{g} must be a
#' vector or factor object of the same length as \code{x} giving the group for
#' the corresponding elements of \code{x}.
#' 
#' @name dunnTest
#' @aliases dunnTest dunnTest.default dunnTest.formula print.dunnTest
#' @param x a numeric vector of data values, or a list of numeric data vectors.
#' @param g a vector or factor object giving the group for the corresponding
#' elements of \code{x}.  Ignored if \code{x} is a list.
#' @param method the method for adjusting p-values for multiple comparisons.
#' The function is calling \code{\link{p.adjust}} and this parameter is
#' directly passed through.
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"greater"} or
#' \code{"less"}. You can specify just the initial letter.
#' @param out.list logical, indicating if the results should be printed in list
#' mode or as a square matrix. Default is list (TRUE).
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
#' @param digits controls the number of fixed digits to print.
#' @param \dots further arguments to be passed to or from methods.
#' @return A list with class \code{"dunnTest"} containing the following
#' components: \item{res}{an array containing the mean rank differencens and
#' the according p-values}
#' @author Andri Signorell <andri@@signorell.net>, the interface is based on
#' R-Core code
#' @seealso \code{\link{kruskal.test}}, \code{\link{wilcox.test}},
#' \code{\link{p.adjust}}
#' @references Dunn, O. J. (1961) Multiple comparisons among means
#' \emph{Journal of the American Statistical Association}, 56(293):52-64.
#' 
#' Dunn, O. J. (1964) Multiple comparisons using rank sums
#' \emph{Technometrics}, 6(3):241-252.
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
#' dunnTest(list(x, y, z))
#' 
#' ## Equivalently,
#' x <- c(x, y, z)
#' g <- factor(rep(1:3, c(5, 4, 5)),
#'             labels = c("Normal subjects",
#'                        "Subjects with obstructive airway disease",
#'                        "Subjects with asbestosis"))
#' 
#' # do the kruskal.test first
#' kruskal.test(x, g)
#' 
#' # ...and the pairwise test afterwards
#' dunnTest(x, g)
#' 
#' ## Formula interface.
#' boxplot(Ozone ~ Month, data = airquality)
#' dunnTest(Ozone ~ Month, data = airquality)
#' 

#' @rdname dunnTest
#' @export
dunnTest <- function (x, ...)
  UseMethod("dunnTest")



#' @rdname dunnTest
#' @export
dunnTest.formula <- function (formula, data, subset, na.action, ...) {
  
  # if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
  #                                                                 "term.labels")) != 1L))
  #   stop("'formula' missing or incorrect")
  # m <- match.call(expand.dots = FALSE)
  # if (is.matrix(eval(m$data, parent.frame())))
  #   m$data <- as.data.frame(data)
  # m[[1L]] <- quote(stats::model.frame)
  # m$... <- NULL
  # mf <- eval(m, parent.frame())
  # if (length(mf) > 2L)
  #   stop("'formula' should be of the form response ~ group")
  # DNAME <- paste(names(mf), collapse = " by ")
  # names(mf) <- NULL
  # response <- attr(attr(mf, "terms"), "response")
  # y <- DoCall("dunnTest", c(as.list(mf), list(...)))
  # y$data.name <- DNAME
  # y
  
  if (missing(formula) || length(formula) != 3L)
    stop("'formula' missing or incorrect")
  
  ## IMPORTANT!!
  ## --- capture subset / na.action HERE ---
  subset_expr <- if (!missing(subset)) substitute(subset) else NULL
  na_expr     <- if (!missing(na.action)) substitute(na.action) else NULL
  
  ## --- parse formula (n.sample only) ---
  pf <- .parse_formula(
    formula   = formula,
    data      = data,
    subset    = subset_expr,
    na.action = na_expr,
    allowed   = "n.sample"
  )
  
  ## --- defensive checks ---
  if (pf$type != "n.sample")
    stop("Dunn test requires an unpaired n-sample design")
  
  ## --- call default method ---
  y <- dunnTest(
    x = pf$x,
    g = pf$group,
    ...
  )
  
  ## --- align with stats:::*.formula behaviour ---
  y$data.name <- pf$data.name
  y
  
  
  
}



#' @rdname dunnTest
#' @export
dunnTest.default <- function (x, g, method = c("holm","hochberg","hommel","bonferroni","BH","BY","fdr","none"),
                              alternative = c("two.sided", "less", "greater"),
                              out.list = TRUE, ...) {
  
  alternative <- match.arg(alternative)
  
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
  
  method <- match.arg(method)
  
  nms <- levels(g)
  
  n <- tapply(g, g, length)
  rnk <- rank(x)
  mrnk <- tapply(rnk, g, mean)
  
  tau <- table(rnk[allDuplicated(rnk)])
  tiesadj <- sum(tau^3 - tau) / (12*(N-1))
  mrnkdiff <- outer(mrnk, mrnk, "-")
  
  z <- mrnkdiff / sqrt( ((N*(N+1)/12) - tiesadj) * outer(1/n, 1/n, "+"))
  
  # extension for alternative in v. 0.99.16:
  if (alternative == "less") {
    pvals <- pnorm(abs(z))
  }
  else if (alternative == "greater") {
    pvals <- pnorm(abs(z), lower.tail=FALSE)
  }
  else {
    pvals <- 2 * pnorm(abs(z), lower.tail=FALSE)
  }
  
  
  keep <- lower.tri(pvals)
  pvals <- pvals[keep]
  m <- sum(keep)
  
  out <- list()
  
  pvals <- p.adjust(pvals, method=method)
  method.str <- method
  
  # p-values matrix
  pmat <- matrix(NA, nrow=length(nms), ncol=length(nms))
  pmat[lower.tri(pmat, diag = FALSE)] <- pvals
  dimnames(pmat) <- list(nms, nms)
  
  if(out.list){
    dnames <- list(NULL, c("mean rank diff", "pval"))
    if (!is.null(nms))
      dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
    out[[1]] <- array(c(mrnkdiff[keep], pvals), c(length(mrnkdiff[keep]), 2L), dnames)
    
  } else {
    out[[1]] <- pmat[-1, -ncol(pmat)]
  }
  
  
  # make symmetric matrix from lower diagonal
  pmatxt <- pmat
  pmatxt[upper.tri(pmatxt)] <- t(pmatxt)[upper.tri(pmatxt)]
  diag(pmatxt) <- 1
  out[["pmat"]] <- pmatxt
  attr(out[["pmat"]], "lbl") <- apply(pmatxt, 1, 
                                      function(x) paste(rownames(pmatxt)[x<0.05], collapse=","))
  
  class(out) <- c("dunnTest")
  attr(out, "main") <- gettextf("Dunn's test of multiple comparisons using rank sums : %s ", method.str)
  attr(out, "method") <- method.str
  attr(out, "out.list") <- out.list
  
  return(out)
  
}




#' @rdname dunnTest
#' @export
print.dunnTest <- function (x, digits = getOption("digits", 3), ...) {
  
  cat("\n", attr(x, "main"), "\n\n")
  xx <- unclass(x)
  
  if(attr(x, "out.list")==TRUE) {
    xx <- data.frame(x[1])
    xx$" " <- fm(xx$"pval", fmt="*")
    xx$"pval" <- format.pval(xx$"pval", digits=2, nsmall=4)
    
    print.data.frame(xx, digits=digits, ...)
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  } else {
    xx[[1]][] <- format.pval(xx[[1]], 2, na.form = "-")
    #     attributes(pp) <- attributes(x$p.value)
    print(xx[[1]], digits=digits, quote = FALSE, ...)
  }
  cat("\n")
  
  invisible(x)
}

