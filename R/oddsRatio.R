
#' Compute Odds Ratios
#'
#' Generic function to compute odds ratios from different object types.
#'
#' @param x An object for which an odds ratio can be computed.
#' @param ... Additional arguments passed to methods.
#'
#' @return An object of class `"OddsRatio"`.
#'
#' @examples
#' # 2x2 table
#' tab <- matrix(c(10, 20, 5, 30), 2)
#' oddsRatio(tab)
#'
#' # logistic regression
#' fit <- glm(vs ~ am, data = mtcars, family = binomial)
#' oddsRatio(fit)
#'

#' @export
oddsRatio <- function (x, ...) {
  UseMethod("oddsRatio")
}



#' @rdname oddsRatio
#' @method oddsRatio default
#' @export
#'
#' @param y Optional second variable to form a 2x2 table.
#' @param conf.level Confidence level for interval estimation.
#' @param sides Type of alternative hypothesis.
#' @param method Method for estimation. One of `"wald"`, `"exact"`, `"midp"`.
#' @param interval Interval for root finding (mid-p method).

oddsRatio.default <- function(x, y = NULL, 
                              conf.level = NA, sides=c("two.sided", "left", "right"), 
                              method = c("wald", "exact", "midp"), 
                              interval = c(0, 1000), ...) {
  
  if(!is.null(y)) x <- table(x, y, ...)
  
  if(is.null(conf.level)) conf.level <- NA
  
  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L || p != d[2L] || p != 2L)
    stop("'x' is not a 2x2 numeric matrix")
  
  switch( match.arg(arg = method)
          , "wald" = {
            if (any(x == 0)) x <- x + 0.5
            lx <- log(x)
            or <- exp(lx[1, 1] + lx[2, 2] - lx[1, 2] - lx[2, 1])
            
            if(is.na(conf.level)){
              res <- or
            } else {
              # Agresti Categorical Data Analysis, 3.1.1
              sigma2lor <- sum(1/x)
              ci <- or * exp(c(1,-1) * qnorm((1-conf.level)/2) * sqrt(sigma2lor))
              res <- c("odds ratio"=or, lwr.ci=ci[1], upr.ci=ci[2])
            }
          }
          , "exact" = {
            if(is.na(conf.level)){
              res <- unname(fisher.test(x, conf.int=FALSE)$estimate)
            } else {
              res <- fisher.test(x, conf.level=conf.level)
              res <- c(res$estimate, lwr.ci=res$conf.int[1], upr.ci=res$conf.int[2])
            }
          }
          , "midp" = {
            
            # based on code from Tomas J. Aragon Developer <aragon at berkeley.edu>
            
            a1 <- x[1,1]; a0 <- x[1,2]; b1 <- x[2,1]; b0 <- x[2,2]; or <- 1
            
            # median-unbiased estimate function
            mue <- function(a1, a0, b1, b0, or){
              mm <- matrix(c(a1,a0,b1,b0), 2, 2, byrow=TRUE)
              fisher.test(mm, or=or, alternative="l")$p-fisher.test(x=x, or=or, alternative="g")$p
            }
            # mid-p function
            midp <- function(a1, a0, b1, b0, or = 1){
              mm <- matrix(c(a1,a0,b1,b0),2,2, byrow=TRUE)
              lteqtoa1 <- fisher.test(mm,or=or,alternative="l")$p.val
              gteqtoa1 <- fisher.test(mm,or=or,alternative="g")$p.val
              0.5*(lteqtoa1-gteqtoa1+1)
            }
            
            # root finding
            EST <- uniroot(
              function(or){ mue(a1, a0, b1, b0, or)},
              interval = interval)$root
            
            if(is.na(conf.level)){
              res <- EST
            } else {
              
              alpha <- 1 - conf.level
              LCL <- uniroot(function(or){
                1-midp(a1, a0, b1, b0, or)-alpha/2
              },  interval = interval)$root
              UCL <- 1/uniroot(function(or){
                midp(a1, a0, b1, b0, or=1/or)-alpha/2
              },  interval = interval)$root
              
              res <- c("odds ratio" = EST, lwr.ci=LCL, upr.ci= UCL)
            }
          }
  )
  return(res)
}



#' @rdname oddsRatio
#' @method oddsRatio glm
#' @export
#'
#' @param conf.level Confidence level for interval estimation.
#' @param digits Number of digits for printing.
#' @param method = c("wald", "profile") use wald or profile likelihood confidence intervals.
#' 
oddsRatio.glm <- function(x,
                          conf.level = 0.95,
                          method = c("wald", "profile"),
                          sides = c("two.sided", "left", "right"),
                          ...) {
  
  if (!inherits(x, "glm"))
    stop("Object must be of class 'glm'.")
  
  if (family(x)$family != "binomial")
    stop("Model must use binomial family.")
  
  method <- match.arg(method)
  sides  <- match.arg(sides)
  
  alpha <- 1 - conf.level
  
  coef_table <- summary(x)$coefficients
  coef_names <- rownames(coef_table)
  
  beta  <- coef_table[, "Estimate"]
  se    <- coef_table[, "Std. Error"]
  pval  <- coef_table[, "Pr(>|z|)"]
  
  OR <- exp(beta)
  
  # ---------- Confidence intervals ----------
  
  if (method == "wald") {
    
    if (sides == "two.sided") {
      z <- qnorm(1 - alpha/2)
      lci <- beta - z * se
      uci <- beta + z * se
      
    } else if (sides == "right") {
      z <- qnorm(1 - alpha)
      lci <- rep(-Inf, length(beta))
      uci <- beta + z * se
      
    } else { # left
      z <- qnorm(1 - alpha)
      lci <- beta - z * se
      uci <- rep(Inf, length(beta))
    }
    
    lci <- exp(lci)
    uci <- exp(uci)
    
  } else {
    
    if (sides != "two.sided")
      warning("Profile likelihood intervals are always two-sided.")
    
    ci  <- exp(confint(x, level = conf.level))
    ci  <- ci[coef_names, , drop = FALSE]
    lci <- ci[, 1]
    uci <- ci[, 2]
  }
  
  result <- data.frame(
    term      = coef_names,
    estimate  = beta,
    std.error = se,
    p.value   = pval,
    OR        = OR,
    OR.lci    = lci,
    OR.uci    = uci,
    row.names = NULL
  )
  
  res <- list(
    coefficients = result,
    call = x$call,
    method = method,
    conf.level = conf.level,
    sides = sides,
    nobs = nobs(x)
  )
  
  class(res) <- "OddsRatio"
  return(res)
}



#' @rdname oddsRatio
#' @export
print.OddsRatio <- function(x, digits = 3, ...) {
  
  cat("\nCall:\n")
  print(x$call)
  
  cat("\nOdds Ratios (",
      x$conf.level * 100,
      "% ",
      x$sides,
      " CI, method = ",
      x$method,
      "):\n\n", sep = "")
  
  tab <- x$coefficients
  
  tab_print <- data.frame(
    OR      = round(tab$OR, digits),
    LCI     = round(tab$OR.lci, digits),
    UCI     = round(tab$OR.uci, digits),
    p.value = signif(tab$p.value, digits)
  )
  
  rownames(tab_print) <- tab$term
  
  print(tab_print)
  cat("\n")
  
  invisible(x)
}




#' @rdname oddsRatio
#' @export
#' 
#' @param intercept Confidence level for interval estimation.
plot.OddsRatio <- function(x, intercept=FALSE, ...){
  
  # , group=NULL, subset = NULL
  
  if(!intercept)
    # x$res <- x$res[rownames(x$res)!="(Intercept)", ]
    x$res <- x$res[!grepl("(Intercept)", rownames(x$res)), ]
  
  args <- list(...)
  
  # here the defaults
  args.errbars1 <- list(from=cbind(x$res$or, x$res$or.lci, x$res$or.uci))
  
  # overwrite with userdefined values
  if (!is.null(args[["args.errbars"]])) {
    args.errbars1[names(args[["args.errbars"]])] <- args[["args.errbars"]][]
    args[["args.errbars"]] <- NULL
  }
  
  # here the defaults for PlotDot
  args.plotdot1 <- list(x=x$res$or, args.errbars=args.errbars1, labels=rownames(x$res),
                        panel.first=quote(abline(v=1, col="grey")))
  
  if (!is.null(args)) {
    args.plotdot1[names(args)] <- args
  }
  
  do.call(DescToolsViz::plotDot, args=args.plotdot1)
  
}






# move to ModTools!
# oddsRatio.zeroinfl <- function (x, conf.level = NULL, digits = 3, ...) {
#   
#   if(is.null(conf.level)) conf.level <- 0.95
#   
#   d.res <- data.frame(summary(x)$coefficients$zero)
#   names(d.res)[c(2, 4)] <- c("Std. Error", "Pr(>|z|)")
#   
#   d.res$or <- exp(d.res$Estimate)
#   d.res$or.lci <- exp(d.res$Estimate + qnorm(0.025) * d.res$"Std. Error")
#   d.res$or.uci <- exp(d.res$Estimate + qnorm(0.975) * d.res$"Std. Error")
#   d.res["(Intercept)", c("or", "or.lci", "or.uci")] <- NA
#   d.res$sig <- format(as.character(cut(d.res$"Pr(>|z|)", breaks = c(0,
#                                                                     0.001, 0.01, 0.05, 0.1, 1), include.lowest = TRUE, labels = c("***",
#                                                                                                                                   "**", "*", ".", " "))), justify = "left")
#   d.res$"Pr(>|z|)" <- fm(d.res$"Pr(>|z|)", fmt="p")
#   d.res["(Intercept)", "Pr(>|z|)"] <- "NA"
#   d.res["(Intercept)", " "] <- ""
#   d.print <- data.frame(lapply(d.res[, 5:7], fm, digits=digits),
#                         p.value = d.res[,4], sig = d.res[, 8], stringsAsFactors = FALSE)
#   
#   rownames(d.print) <- rownames(d.res)
#   res <- list(or = d.print, call = x$call,
#               BrierScore = BrierScore(resp=(model.response(model.frame(x)) > 0) * 1L,
#                                       pred=predict(x, type="zero")),
#               PseudoR2 = PseudoR2(x, which="all"), res=d.res)
#   
#   class(res) <- "OddsRatio"
#   
#   return(res)
# }
# 



