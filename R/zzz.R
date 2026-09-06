

.onLoad <- function(libname, pkgname) {

  # presetting DescToolsX options not already defined by the user
  op <- options()
  pkg.op <- list(
    
    DescToolsX.palette   = c("#8296C4", "#9A0941", "#F08100", "#FED037",
                             "#CAB790", "#B3BA12", "#D35186", "#8FAE8C",  
                             "#5F6F9A", "#E6E2D3", "#6E5A3C", "#5B2A45"),
    DescToolsX.digits    = 3,
    DescToolsX.fixedfont = structure(list(name = "Consolas", size = 7), 
                                    class = "Font"),
    DescToolsX.footnote  = c("\u00B9","\u00B2","\u00B3","\u2074",
                            "\u2075","\u2076","\u2077","\u2078","\u2079"), 
    DescToolsX.lang      = "en",
    DescToolsX.plotit    = TRUE,
    DescToolsX.stamp     = expression(gettextf("%s / %s", Sys.getenv("USERNAME"),
                                              pharos::fm(DescToolsX::today(), 
                                                             fmt = "yyyy-MM-dd"))),
    DescToolsX.linesep   = cli::col_yellow("\u2500"),
    
    DescToolsX.lastWrd   = NULL,
    DescToolsX.lastXL    = NULL,
    DescToolsX.lastPP    = NULL,
    
    abs.sty   = structure(list(digits = 0, bigMark = "",
                               label = "Number format for counts"), 
                          class = "Style"),
    per.sty   = structure(list(digits = 1, fmt = "%",
                               name = "per", label = "Percentage number format"),
                          class = "Style"),
    num.sty   = structure(list(digits = 3, bigMark = "",
                               label = "Number format for numeric values"), 
                          class = "Style"),
    pval.sty   = structure(list(fmt="p", pThreshold = 1e-3,
                                label = "Number format for p-values"),
                           class = "Style")
  )
  
  toset <- !(names(pkg.op) %in% names(op))
  if (any(toset)) options(pkg.op[toset])

}


# if starting environment is somwhere needed
# .DescToolsEnv <- new.env(parent = emptyenv())


#' @useDynLib DescToolsX, .registration = TRUE
#' @importFrom Rcpp evalCpp
#' @importFrom RcppParallel RcppParallelLibs
#' 
#' @importFrom stats na.omit filter is.ts mad median pbinom qbinom qchisq qnorm qt quantile sd t.test var runif binom.test complete.cases addmargins chisq.test ftable terms dbinom qbeta uniroot IQR approx ppois dchisq pchisq var.test frequency lm optimize relevel anova na.pass pnorm p.adjust rnorm cor ppoints model.tables pt ptukey qtukey aov pf qf filter relevel fisher.test mcnemar.test coefficients confint confint.default formula model.frame model.matrix model.response nobs predict family as.formula xtabs end start AIC embed residuals kruskal.test reshape contr.helmert contr.poly contr.sum contr.treatment poisson.test contrasts drop1 integrate splinefun cov2cor vcov model.extract na.pass na.omit na.exclude na.fail optim optimise nlm aggregate logLik AIC BIC model.frame predict glm loess cooks.distance cor.test density shapiro.test cov deviance fitted weights acf Box.test coef time fivenum
#'             
#' @importFrom graphics hist abline barplot box grid layout par points rect segments strwidth text title axis mosaicplot spineplot arrows boxplot cdplot legend lines mtext polygon
#'             
#' @importFrom grDevices dev.flush dev.hold
#' 
#' @importFrom utils head tail capture.output object.size str combn find getAnywhere lsf.str write.table getFromNamespace stack browseURL help.search modifyList getS3method
#'             
#' @importFrom bedrock %)(% %[]% abind combPairs maxDec setNamesX label naIf naReplace isZero isWholeLike isDichotomous %][% pairApply appendX sortX revX sampleX untable coalesceX columnWrap splitAt moveAvg removeAttr combLevels mergeArgs printCharMatrix dummy isEuclid collapseTable callIf resolveFormula extractArgs getDotsArg recycle checkConfLevel checkFlag applySides
#'             
#' @importFrom pharos plotFdist strAlign strTrim strTrunc fm style lineSep strTrim strPad plotDens2D plotBag plotHexbin plotDens plotAssoc plotHeatmap plotTimeSeries plotCatDist plotPropCI addOpacity plotDensBox band canvas fade pal plotBox plotXY mar plot.Desc.qn plot.Desc.table
#'             
#' @importFrom lumen scores adfTest gTest kpssTest mantelTrendTest varTest meanCI binomCI binomDiffCI bootCI leveneTest cochranArmitageTest bpTest corCI fisherZ fisherZInv
#'             
#' @importFrom stringi stri_replace_all_fixed
#' 
#' @importFrom mvtnorm pmvnorm
#'              
NULL
