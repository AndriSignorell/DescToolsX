

#' Levene's Test for Homogeneity of Variance
#' 
#' Computes Levene's test for homogeneity of variance across groups.
#' 
#' Let \eqn{X_{ij}} be the jth observation of X for the ith group. 
#' Let \eqn{Z_{ij} = |X_{ij} - X_i|}, where \eqn{X_i} is the mean of X in the ith group. 
#' Levene’s test statistic is 
#' \deqn{ W_0 = \frac{ \sum_i n_i (\bar{Z}_i - \bar{Z})^2 / (g - 1) }{ \sum_i 
#' \sum_j (Z_{ij} - \bar{Z}_i)^2 / \sum_i (n_i - 1) } } 
#' where \eqn{n_i} is the number of observations in group i and g is the number of 
#' groups.

#' @aliases leveneTest leveneTest.formula leveneTest.default

#' @param x response variable for the default method, or a \code{lm} or
#' \code{formula} object. If \code{y} is a linear-model object or a formula,
#' the variables on the right-hand-side of the model must all be factors and
#' must be completely crossed.

#' @param g factor defining groups.

#' @param center The name of a function to compute the center of each group;
#' \code{mean} gives the original Levene's test; the default, \code{median},
#' provides a more robust test (Brown-Forsythe-Test).

#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} gives
#' the data values and \code{rhs} the corresponding groups.

#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the formula
#' \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.

#' @param subset an optional vector specifying a subset of observations to be used.
#' @param na.action a function which indicates what should happen 
#' when the data contain NAs. Defaults to \code{getOption("na.action")}.

#' @param ... arguments to be passed down, e.g., \code{data} for the
#' \code{formula}; can also be used to pass arguments to
#' the function given by \code{center} (e.g., \code{center=mean},
#' \code{trim=0.1} specify the 10% trimmed mean).

#' @return An object of class "htest" representing the result of the 
#' hypothesis test.

#' @note This function is rewritten using common R standards based on 
#' car::leveneTest() using the same calculation logic.

#' @author andri.signorell \email{andri@signorell.net}; original version 
#' written by John Fox \email{jfox@@mcmaster.ca} based on a generic version
#' contributed by Derek Ogle\cr adapted from a response posted by Brian Ripley
#' to the r-help email list.

#' @seealso \code{\link{fligner.test}} for a rank-based (nonparametric)
#' \eqn{k}-sample test for homogeneity of variances; \code{\link{mood.test}}
#' for another rank-based two-sample test for a difference in scale parameters;
#' \code{\link{var.test}} and \code{\link{bartlett.test}} for parametric tests
#' for the homogeneity in variance.
#' 
#' \code{\link[coin:ScaleTests]{ansari_test}} in package \pkg{coin} for exact
#' and approximate \emph{conditional} p-values for the Ansari-Bradley test, as
#' well as different methods for handling ties.
#' 
#' @references Fox, J. (2008) \emph{Applied Regression Analysis and Generalized
#' Linear Models}, Second Edition. Sage.
#' 
#' Fox, J. and Weisberg, S. (2011) \emph{An R Companion to Applied Regression},
#' Second Edition, Sage.
#' 
#' Levene, H. (1960) Robust tests for equality of variances. 
#' in Ingram, O., Hotelling, H. et al. (Hrsg.) (1960) Contributions 
#' to Probability and Statistics, \emph{Essays in Honor of Harold Hotelling}. 
#' Stanford University Press, 1960, ISBN 0-8047-0596-8, S. 278–292.

#' @keywords htest

#' @examples
#' 
#' ## example from ansari.test:
#' ## Hollander & Wolfe (1973, p. 86f):
#' ## Serum iron determination using Hyland control sera
#' serum <- toLong(data.frame(
#'           ramsay=c(111, 107, 100, 99, 102, 106, 109, 108, 104, 99,
#'                    101, 96, 97, 102, 107, 113, 116, 113, 110, 98),
#'           jung.parekh=c(107, 108, 106, 98, 105, 103, 110, 105, 104,
#'                         100, 96, 108, 103, 104, 114, 114, 113, 108, 106, 99)
#'           ))
#' 
#' leveneTest(x ~ grp, data=serum)
#' leveneTest(x ~ grp, data=serum, center=mean)
#' leveneTest(x ~ grp, data=serum, center=mean, trim=0.1)
#' 
#' leveneTest( c(rnorm(10), rnorm(10, 0, 2)), 
#'             factor(rep(c("A","B"), each=10)) )
#' 
#' leveneTest(Ozone ~ Month, data = airquality)
#' 
#' leveneTest(count ~ spray, data = InsectSprays)
#' # Compare this to fligner.test() and bartlett.test()
#' 



#' @export
leveneTest <- function (x, ...) 
  UseMethod("leveneTest")



#' @rdname leveneTest
#' @export
leveneTest.formula <- function(formula, data, subset, na.action, ...) {
  
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
    stop("Levene test requires an unpaired n-sample design")
  
  ## --- call default method ---
  y <- leveneTest(
    x = pf$x,
    g = pf$group,
    ...
  )
  
  ## --- align with stats:::*.formula behaviour ---
  y$data.name <- pf$data.name
  y
}



#' @rdname leveneTest
#' @export
leveneTest.default <- function (x, g, center=median, ...) {
  
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
  n <- length(x)
  if (n < 2L) 
    stop("not enough observations")
  
  meds <- tapply(x[OK], g[OK], center, ...)
  resp <- abs(x - meds[g])
  ANOVA_TAB <- anova(lm(resp ~ g))
  
  rownames(ANOVA_TAB)[2] <- " "
  dots <- deparse(substitute(...))
  
  dots <- unlist(match.call(expand.dots=FALSE)$...)
  center_x <- deparse(substitute(center))
  if(!is.null(dots))
    center_x <- paste0(center_x, 
                       gettextf("(%s)", 
                                paste(gettextf("%s=%s", 
                                               names(dots), dots), 
                                      collapse = ", ")))
  
  STATISTIC <- ANOVA_TAB$`F value`[1]
  PARAMETER <- ANOVA_TAB$Df
  PVAL <- ANOVA_TAB$`Pr(>F)`[1]
  
  names(STATISTIC) <- "F"
  
  names(PARAMETER) <- c("num df", "denom df")
  
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
               p.value = PVAL, 
               method = gettextf(
                 "Levene's Test for Homogeneity of Variance (center = %s)",
                 center_x), 
               data.name = DNAME, anova_tab=ANOVA_TAB)
  
  class(RVAL) <- "htest"
  return(RVAL)
  
}



# https://www.stata.com/manuals13/rsdtest.pdf
# stay <- haven::read_dta("http://www.stata-press.com/data/r13/stay.dta")
# 
# leveneTest(lengthstay ~ factor(sex), stay)
# leveneTest(lengthstay ~ factor(sex), stay, center=mean)
# leveneTest(lengthstay ~ factor(sex), stay, center=mean, trim=0.1)
# 



# 
# with(carData::Moore, leveneTest(conformity, fcategory))
# 
# with(carData::Moore, 
#      leveneTest(conformity, interaction(fcategory, partner.status)))
# 
# leveneTest(lm(conformity ~ fcategory*partner.status, data = carData::Moore))
# 
# leveneTest(conformity ~ fcategory * partner.status, data = carData::Moore)
# leveneTest(conformity ~ fcategory * partner.status, data = Moore, center = mean)
# leveneTest(conformity ~ fcategory * partner.status, data = Moore, center = mean, trim = 0.1)
# 

# 
# leveneTest(lm(conformity ~ fcategory*partner.status, data = Moore))
# 

