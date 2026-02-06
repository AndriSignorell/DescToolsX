

.onLoad <- function(libname, pkgname) {

  # presetting DescTools options not already defined by the user
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
                                              DescToolsX::fm(DescToolsX::Today(), 
                                                             fmt = "yyyy-mm-dd"))),
    DescToolsX.linesep   = cli::col_yellow("\u2500"),
    
    DescToolsX.lastWrd   = NULL,
    DescToolsX.lastXL    = NULL,
    DescToolsX.lastPP    = NULL,
    
    abs.sty   = structure(list(digits = 0, big.mark = "",
                               label = "Number format for counts"), 
                          class = "Style"),
    per.sty   = structure(list(digits = 1, fmt = "%",
                               name = "per", label = "Percentage number format"),
                          class = "Style"),
    num.sty   = structure(list(digits = 3, big.mark = "",
                               label = "Number format for numeric values"), 
                          class = "Style"),
    pval.sty   = structure(list(fmt="p", eps=1e-3,
                                label = "Number format for p-values"),
                           class = "Style")
  )
  
  toset <- !(names(pkg.op) %in% names(op))
  if (any(toset)) options(pkg.op[toset])

}


.DescToolsEnv <- new.env(parent = emptyenv())


#' @useDynLib DescToolsX, .registration = TRUE
#' 
#' @importFrom Rcpp sourceCpp
#' @importFrom data.table frankv
#' @importFrom haven read_spss
#' @importFrom stats na.omit filter is.ts mad median pbinom qbinom 
#'             qchisq qnorm qt quantile sd t.test var runif 
#'             binom.test complete.cases addmargins chisq.test ftable 
#'             terms dbinom qbeta uniroot IQR approx ppois dchisq pchisq 
#'             var.test frequency lm optimize relevel anova na.pass pnorm
#'             p.adjust rnorm cor ppoints model.tables pt ptukey qtukey
#'             aov pf qf 
#'             
#' @importFrom graphics hist
#'             abline barplot box grid layout par points rect 
#'             segments strwidth text title axis
#' @importFrom grDevices dev.flush dev.hold
#' 
#' @importFrom utils readRegistry head tail capture.output object.size
#'             str combn
#' @importFrom gld fit.fkml dgl pgl
#' @importFrom DescToolsGraphics plotFdist
NULL
