
#' Post-Hoc Tests
#' 
#' A convenience wrapper for computing post-hoc test after having calculated an
#' ANOVA. 
#' 
#' The function is designed to consolidate a couple of post-hoc tests with the
#' same interface for input and output.
#' 
#' \bold{Choosing Tests.} \verb{ } Different post hoc tests use different
#' methods to control familywise (FW) and per experiment error rate (PE). Some
#' tests are very conservative. Conservative tests go to great lengths to
#' prevent the user from committing a type 1 error.  They use more stringent
#' criterion for determining significance. Many of these tests become more and
#' more stringent as the number of groups increases (directly limiting the FW
#' and PE error rate). Although these tests buy you protection against type 1
#' error, it comes at a cost. As the tests become more stringent, you loose
#' power (1-B).  More liberal tests, buy you power but the cost is an increased
#' chance of type 1 error.  There is no set rule for determining which test to
#' use, but different researchers have offered some guidelines for choosing.
#' Mostly it is an issue of pragmatics and whether the number of comparisons
#' exceeds k-1.
#' 
#' \bold{The Fisher's LSD} \verb{ } (Least Significant Different) sets alpha
#' level per comparison. alpha = .05 for every comparison. df = df error (i.e.
#' df within). This test is the most liberal of all post hoc tests. The
#' critical t for significance is unaffected by the number of groups. This test
#' is appropriate when you have 3 means to compare. In general the alpha is
#' held at .05 because of the criterion that you can't look at LSD's unless the
#' ANOVA is significant. This test is generally not considered appropriate if
#' you have more than 3 means unless there is reason to believe that there is
#' no more than one true null hypothesis hidden in the means.
#' 
#' \bold{Dunn's (Bonferroni) t-test} \verb{ } is sometimes referred to as the
#' Bonferroni t because it used the Bonferroni PE correction procedure in
#' determining the critical value for significance. In general, this test
#' should be used when the number of comparisons you are making exceeds the
#' number of degrees of freedom you have between groups (e.g. k-1). This test
#' sets alpha per experiment; alpha = (.05)/c for every comparison. df = df
#' error (c = number of comparisons (k(k-1))/2) This test is extremely
#' conservative and rapidly reduces power as the number of comparisons being
#' made increase.
#' 
#' \bold{Newman-Keuls} \verb{ } is a step down procedure that is not as
#' conservative as Dunn's t test. First, the means of the groups are ordered
#' (ascending or descending) and then the largest and smallest means are tested
#' for significant differences. If those means are different, then test
#' smallest with next largest, until you reach a test that is not significant.
#' Once you reach that point then you can only test differences between means
#' that exceed the difference between the means that were found to be
#' non-significant. Newman-Keuls is perhaps one of the most common post hoc
#' test, but it is a rather controversial test. The major problem with this
#' test is that when there is more than one true null hypothesis in a set of
#' means it will overestimate the FW error rate. In general we would use this
#' when the number of comparisons we are making is larger than k-1 and we don't
#' want to be as conservative as the Dunn's test is.
#' 
#' \bold{Tukey's HSD} \verb{ } (Honestly Significant Difference) is essentially
#' like the Newman-Keuls, but the tests between each mean are compared to the
#' critical value that is set for the test of the means that are furthest apart
#' (rmax e.g. if there are 5 means we use the critical value determined for the
#' test of X1 and X5). This method corrects for the problem found in the
#' Newman-Keuls where the FW is inflated when there is more than one true null
#' hypothesis in a set of means. It buys protection against type 1 error, but
#' again at the cost of power. It tends to be the most common and preferred
#' test because it is very conservative with respect to type 1 error when the
#' null hypothesis is true. In general, HSD is preferred when you will make all
#' the possible comparisons between a large set of means (6 or more means).
#' 
#' \bold{The Scheffe test} \verb{ } is designed to protect against a type 1
#' error when all possible complex and simple comparisons are made. That is we
#' are not just looking the possible combinations of comparisons between pairs
#' of means. We are also looking at the possible combinations of comparisons
#' between groups of means. Thus Scheffe is the most conservative of all tests.
#' Because this test does give us the capacity to look at complex comparisons,
#' it essentially uses the same statistic as the linear contrasts tests.
#' However, Scheffe uses a different critical value (or at least it makes an
#' adjustment to the critical value of F). This test has less power than the
#' HSD when you are making pairwise (simple) comparisons, but it has more power
#' than HSD when you are making complex comparisons. In general, only use this
#' when you want to make many post hoc complex comparisons (e.g. more than
#' k-1).
#' 
#' \bold{Tables} \verb{ } For tables pairwise chi-square test can be performed,
#' either without correction or with correction for multiple testing following
#' the logic in \code{\link{p.adjust}}. 
#' 
#' @name postHoc
#' @aliases postHocTest postHocTest.aov postHocTest.table postHocTest.matrix
#' print.postHocTest plot.postHocTest
#' @param x an aov object. 
#' @param method one of \code{"hsd"}, \code{"bonf"}, \code{"lsd"},
#' \code{"scheffe"}, \code{"newmankeuls"}, defining the method for the pairwise
#' comparisons.\cr For the post hoc test of tables the methods of
#' \code{\link{p.adjust}} can be supplied. See the detail there. 
#' @param which a character vector listing terms in the fitted model for which
#' the intervals should be calculated. Defaults to all the terms. 
#' @param conf.level a numeric value between zero and one giving the
#' family-wise confidence level to use.  If this is set to NA, just a matrix
#' with the p-values will be returned. 
#' @param ordered a logical value indicating if the levels of the factor should
#' be ordered according to increasing average in the sample before taking
#' differences. If ordered is \code{TRUE} then the calculated differences in
#' the means will all be positive. The significant differences will be those
#' for which the lower end point is positive. \cr This argument will be ignored
#' if method is not either \code{hsd} or \code{newmankeuls}.
#' @param digits controls the number of fixed digits to print.
#' @param \dots further arguments, not used so far.
#'  
#' @return an object of type "postHocTest", which will either be \cr A) a list
#' of data.frames containing the mean difference, lower ci, upper ci and the
#' p-value, if a conf.level was defined (something else than NA) or \cr B) a
#' list of matrices with the p-values, if conf.level has been set to NA. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{TukeyHSD}}, \code{\link{aov}},
#' \code{\link{pairwise.t.test}}, \code{\link{scheffeTest}} 
#' @keywords htest
#' @examples
#' 
#' postHocTest(aov(breaks ~ tension, data = warpbreaks), method = "lsd")
#' postHocTest(aov(breaks ~ tension, data = warpbreaks), method = "hsd")
#' postHocTest(aov(breaks ~ tension, data = warpbreaks), method = "scheffe")
#' 
#' r.aov <- aov(breaks ~ tension, data = warpbreaks)
#' 
#' # compare p-values:
#' round(cbind(
#'     lsd= postHocTest(r.aov, method="lsd")$tension[,"pval"]
#'   , bonf=postHocTest(r.aov, method="bonf")$tension[,"pval"]
#' ), 4)
#' 
#' # only p-values by setting conf.level to NA
#' postHocTest(aov(breaks ~ tension, data = warpbreaks), method = "hsd",
#'             conf.level=NA)
#' 


#' @rdname postHoc
#' @export
postHocTest <- function (x, ...)
  UseMethod("postHocTest")



#' @rdname postHoc
#' @export
postHocTest.aov <- function (x, which = NULL,
                             method=c("hsd","bonferroni","lsd","scheffe","newmankeuls","duncan"),
                             conf.level = 0.95, ordered = FALSE, ...) {
  
  method <- match.arg(method)
  
  if(method=="scheffe"){
    out <- scheffeTest(x=x, which=which, conf.level=conf.level, ...)
    
  } else {
    
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
    for (nm in names(tabs)) {
      tab <- tabs[[nm]]
      means <- as.vector(tab)
      nms <- if (length(d <- dim(tab)) > 1L) {
        dn <- dimnames(tab)
        apply(do.call("expand.grid", dn), 1L, paste, collapse = ":")
      }
      else names(tab)
      n <- nn[[nm]]
      if (length(n) < length(means))
        n <- rep.int(n, length(means))
      
      # this will be ignored for bonferroni, lsd
      if (method %in% c("hsd", "newmankeuls", "duncan") & as.logical(ordered)) {
        ord <- order(means)
        means <- means[ord]
        n <- n[ord]
        if (!is.null(nms))
          nms <- nms[ord]
      }
      
      center <- outer(means, means, "-")
      keep <- lower.tri(center)
      center <- center[keep]
      
      switch(method
             ,"bonferroni" = {
               width <-  qt(1 - (1 - conf.level)/(length(means) * (length(means) - 1)), x$df.residual) *
                 sqrt(MSE * outer(1/n, 1/n, "+"))[keep]
               est <- center/sqrt(MSE * outer(1/n, 1/n, "+")[keep])
               
               pvals <- pmin(2 * pt(abs(est), df = x$df.residual, lower.tail = FALSE)
                             * ((length(means)^2 - length(means))/2), 1)
               method.str <- "Bonferroni"
               
             }
             ,"lsd" = {
               width <-  qt(1 - (1 - conf.level)/2, x$df.residual) *
                 sqrt(MSE * outer(1/n, 1/n, "+"))[keep]
               est <- center/sqrt(MSE * outer(1/n, 1/n, "+")[keep])
               pvals <- 2 * pt(abs(est), df = x$df.residual, lower.tail = FALSE)
               method.str <- "Fisher LSD"
             }
             ,"hsd" = {
               width <- qtukey(conf.level, length(means), x$df.residual) *
                 sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
               est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
               pvals <- ptukey(abs(est), length(means), x$df.residual,
                               lower.tail = FALSE)
               method.str <- "Tukey HSD"
               
             }
             ,"newmankeuls" ={
               nmean <- (abs(outer(rank(means), rank(means), "-")) + 1)[keep]
               
               width <- qtukey(conf.level, nmean, x$df.residual) *
                 sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
               
               est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
               
               pvals <- ptukey(abs(est), nmean, x$df.residual, lower.tail = FALSE)
               method.str <- "Newman-Keuls"
               
             }
             ,"duncan" = {
               # same as newmankeuls, but with bonferroni corrected alpha
               nmean <- (abs(outer(rank(means), rank(means), "-")) + 1)[keep]
               
               width <- qtukey(conf.level^(nmean-1), nmean, x$df.residual) *
                 sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep]
               
               est <- center/(sqrt((MSE/2) * outer(1/n, 1/n, "+"))[keep])
               pvals <- 1-(1-ptukey(abs(est), nmean, x$df.residual,
                                    lower.tail = FALSE))^(1/(nmean - 1))
               
               method.str <- "Duncan's new multiple range test"
               
             }
             ,"dunnett" = {
               method.str <- "Dunnett"
             }
             ,"scottknott" = {
               method.str <- "Scott Knott"
             }
             ,"waller" = {
               method.str <- "Waller"
             }
             ,"gabriel" = {
               method.str <- "Gabriel"
             }
      )
      
      if(!is.na(conf.level)){
        dnames <- list(NULL, c("diff", "lwr.ci", "upr.ci", "pval"))
        if (!is.null(nms))
          dnames[[1L]] <- outer(nms, nms, paste, sep = "-")[keep]
        out[[nm]] <- array(c(center, center - width,
                             center + width, pvals), c(length(width), 4L), dnames)
      } else {
        out[[nm]] <- matrix(NA, nrow=length(means), ncol=length(means))
        out[[nm]][lower.tri(out[[nm]], diag = FALSE)] <- pvals
        dimnames(out[[nm]]) <- list(nms, nms)
        out[[nm]] <- out[[nm]][-1, -ncol(out[[nm]])]
        
      }
    }
    
    class(out) <- c("PostHocTest")
    attr(out, "orig.call") <- x$call
    attr(out, "conf.level") <- conf.level
    attr(out, "ordered") <- ordered
    attr(out, "method") <- method.str
    attr(out, "method.str") <- gettextf("\n  Posthoc multiple comparisons of means : %s \n", attr(out, "method"))
    
  }
  
  return(out)
  
}


#' @rdname postHoc
#' @export
postHocTest.matrix <- function(x, method = c("none","fdr","BH","BY","bonferroni","holm","hochberg","hommel"),
                               conf.level = 0.95, ...) {
  
  # http://support.sas.com/resources/papers/proceedings14/1544-2014.pdf
  
  # no conf.level supported so far
  conf.level  <- NA
  
  method <- match.arg(method)
  
  pvals <- pairApply(t(as.matrix(x)), FUN = function(y1, y2) chisq.test(cbind(y1,y2))$p.value, symmetric=TRUE)
  pvals[upper.tri(pvals, diag=TRUE)] <- NA
  
  if(method != "none")
    pvals[] <- p.adjust(pvals, method=method)
  
  #  pvals[] <- format.pval(pvals, digits = 2, na.form = "-")
  pvals <- pvals[-1, -ncol(pvals)]
  out <- list()
  out[[deparse(substitute(x))]] <- pvals
  
  class(out) <- c("postHocTest")
  attr(out, "orig.call") <- "table"
  attr(out, "conf.level") <- conf.level
  attr(out, "ordered") <- FALSE
  attr(out, "method") <- method
  attr(out, "method.str") <- gettextf("\n  Posthoc multiple comparisons on chi-square test : %s \n", attr(out, "method"))
  
  return(out)
  
}


#' @rdname postHoc
#' @export
postHocTest.table <- function(x, method = c("none","fdr","BH","BY","bonferroni","holm","hochberg","hommel"),
                              conf.level = 0.95, ...) {
  class(x) <- "matrix"
  postHocTest(x, method=method, conf.level=conf.level, ...)
}




#' @rdname postHoc
#' @export
print.PostHocTest <- function (x, digits = getOption("digits", 3), ...) {
  
  cat(attr(x, "method.str"))
  if (!is.na(attr(x, "conf.level")))
    cat("    ", format(100 * attr(x, "conf.level"), 2), "% family-wise confidence level\n",
        sep = "")
  if (attr(x, "ordered"))
    cat("    factor levels have been ordered\n")
  if(!is.language(attr(x, "orig.call")) && !is.null(attr(x, "orig.call")))
    cat("\nFit: ", deparse(attr(x, "orig.call"), 500L), "\n\n", sep = "")
  else
    cat("\n")
  xx <- unclass(x)
  
  attr(xx, "orig.call") <- attr(xx, "conf.level") <-
    attr(xx, "ordered") <-  attr(xx, "method.str") <-  attr(xx, "method") <- NULL
  
  xx["data.name"] <- NULL
  
  if(!is.na(attr(x, "conf.level"))) {
    xx <- lapply(xx, as.data.frame)
    for(nm in names(xx)){
      xx[[nm]]$" " <- fm(xx[[nm]]$"pval", fmt="*")
      xx[[nm]]$"pval" <- fm(xx[[nm]]$"pval", fmt="p")
    }
    
    print.default(xx, digits=digits, ...)
    cat("---\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
  } else {
    for(nm in names(xx)){
      xx[[nm]][] <- fm(xx[[nm]], fmt="p", na.form = "-")
    }
    #     attributes(pp) <- attributes(x$p.value)
    print(xx, digits=digits, quote = FALSE, ...)
  }
  cat("\n")
  
  invisible(x)
}



#' @rdname postHoc
#' @export
plot.PostHocTest <- function(x, ...){
  # original:   stats:::plot.TukeyHSD(x, ...)
  
  # don't need that here..
  x$data.name <- NULL
  
  for (i in seq_along(x)) {
    xi <- x[[i]][, -4L, drop = FALSE]
    yvals <- nrow(xi):1L
    dev.hold()
    on.exit(dev.flush())
    plot(c(xi[, "lwr.ci"], xi[, "upr.ci"]), rep.int(yvals, 2L),
         type = "n", axes = FALSE, xlab = "", ylab = "", main = NULL,
         ...)
    axis(1, ...)
    axis(2, at = nrow(xi):1, labels = dimnames(xi)[[1L]],
         srt = 0, ...)
    abline(h = yvals, lty = 1, lwd = 0.5, col = "lightgray")
    abline(v = 0, lty = 2, lwd = 0.5, ...)
    segments(xi[, "lwr.ci"], yvals, xi[, "upr.ci"], yvals, ...)
    segments(as.vector(xi), rep.int(yvals - 0.1, 3L), as.vector(xi),
             rep.int(yvals + 0.1, 3L), ...)
    title(main = paste0(format(100 * attr(x, "conf.level"),
                               digits = 2L), "% family-wise confidence level\n"),
          xlab = paste("Differences in mean levels of", names(x)[i]))
    box()
    dev.flush()
    on.exit()
  }
  
}

