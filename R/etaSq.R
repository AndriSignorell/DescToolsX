
#' Effect Size Calculations for ANOVAs
#' 
#' Calculates eta-squared, partial eta-squared and generalized eta-squared
#' 
#' Calculates the eta-squared, partial eta-squared, and generalized eta-squared
#' measures of effect size that are commonly used in analysis of variance. The
#' input \code{fit} should be the analysis of variance object itself. For
#' between-subjects designs, generalized eta-squared equals partial
#' eta-squared. The reported generalized eta-squared for repeated-measures
#' designs assumes that all factors are manipulated, i.e., that there are no
#' measured factors like gender (see references).
#' 
#' For unbalanced designs, the default in \code{etaSq} is to compute Type II
#' sums of squares (\code{type=2}), in keeping with the \code{Anova} function
#' in the \code{car} package. It is possible to revert to the Type I SS values
#' (\code{type=1}) to be consistent with \code{anova}, but this rarely tests
#' hypotheses of interest. Type III SS values (\code{type=3}) can also be
#' computed. \code{etaSq.aovlist} requires \code{type=1}.
#' 
#' @name etaSq
#' @aliases etaSq etaSq.lm etaSq.aovlist aovlDetails aovlErrorTerms
#' @param fit an analysis of variance object of class \code{"aov"} or
#' \code{"aovlist"}
#' @param type type of sums of squares to calculate.
#' \code{etaSq.aovlist()} requires \code{type = 1}.
#' @param anova logical; whether to include the full ANOVA table with the
#' effect sizes
#' 
#' @return for \code{etaSq.lm()}, a numeric matrix with one row per model term
#' and columns \code{eta.sq} and \code{eta.sq.part}; for
#' \code{etaSq.aovlist()}, a numeric matrix that additionally contains
#' \code{eta.sq.gen}. If \code{anova = TRUE}, ANOVA statistics are included in
#' additional columns. \code{aovlDetails()} returns a data frame of effect
#' terms and their ANOVA statistics, while \code{aovlErrorTerms()} returns a
#' list with components:
#' \describe{
#'   \item{\code{SS}}{error sums of squares}
#'   \item{\code{MS}}{error mean squares}
#'   \item{\code{DF}}{error degrees of freedom}
#' }
#' 
#' @note
#' Based on code by Danielle Navarro, and Daniel Wollschlaeger.
#' 
#' @seealso \code{\link{aov}}, \code{\link{anova}}, \code{\link[car]{Anova}}
#' 
#' @references Bakeman, R. (2005). Recommended effect size statistics for
#' repeated measures designs. Behavior Research Methods 37(3), 379-384.
#' 
#' Olejnik, S. and Algina, J. (2003). Generalized Eta and Omega Squared
#' Statistics: Measures of Effect Size for Some Common Research Designs.
#' Psychological Methods 8(4), 434-447.
#' 
#' 
#' @examples
#' 
#' #### Example 1: one-way ANOVA ####
#' 
#' outcome <- c(1.4,2.1,3.0,2.1,3.2,4.7,3.5,4.5,5.4)    # data
#' treatment1 <- factor(c(1,1,1,2,2,2,3,3,3))           # grouping variable
#' anova1 <- aov(outcome ~ treatment1)                  # run the ANOVA
#' summary(anova1)                                      # print the ANOVA table
#' etaSq(anova1)                                        # effect size
#' 
#' #### Example 2: two-way ANOVA ####
#' 
#' treatment2 <- factor(c(1,2,3,1,2,3,1,2,3))       # second grouping variable
#' anova2 <- aov(outcome ~ treatment1 + treatment2) # run the ANOVA
#' summary(anova2)                                  # print the ANOVA table
#' etaSq(anova2)                                    # effect size
#' 
#' #### Example 3: two-way ANOVA unbalanced cell sizes ####
#' #### data from Maxwell & Delaney, 2004              ####
#' #### Designing experiments and analyzing data       ####
#' 
#' dfMD <- data.frame(IV1=factor(rep(1:3, c(3+5+7, 5+6+4, 5+4+6))),
#'                    IV2=factor(rep(rep(1:3, 3), c(3,5,7, 5,6,4, 5,4,6))),
#'                    DV=c(c(41, 43, 50), c(51, 43, 53, 54, 46), c(45, 55, 56, 60, 58, 62, 62),
#'                         c(56, 47, 45, 46, 49), c(58, 54, 49, 61, 52, 62), c(59, 55, 68, 63),
#'                         c(43, 56, 48, 46, 47), c(59, 46, 58, 54), c(55, 69, 63, 56, 62, 67)))
#' 
#' # use contr.sum for correct sum of squares type 3
#' dfMD$IV1s <- C(dfMD$IV1, "contr.sum")
#' dfMD$IV2s <- C(dfMD$IV2, "contr.sum")
#' dfMD$IV1t <- C(dfMD$IV1, "contr.treatment")
#' dfMD$IV2t <- C(dfMD$IV2, "contr.treatment")
#' 
#' etaSq(aov(DV ~ IV1s*IV2s, data=dfMD), type=3)
#' etaSq(aov(DV ~ IV1t*IV2t, data=dfMD), type=1)
#' 
#' #### Example 4: two-way split-plot ANOVA -> etaSq.aovlist ####
#' 
#' set.seed(1)
#' DV_t1 <- round(rnorm(3*10, -0.5, 1), 2)
#' DV_t2 <- round(rnorm(3*10,  0,   1), 2)
#' DV_t3 <- round(rnorm(3*10,  0.5, 1), 2)
#' dfSPF <- data.frame(id=factor(rep(1:(3*10), times=3)),
#'                     IVbtw=factor(rep(LETTERS[1:3], times=3*10)),
#' 					IVwth=factor(rep(1:3, each=3*10)),
#' 					DV=c(DV_t1, DV_t2, DV_t3))
#' spf <- aov(DV ~ IVbtw*IVwth + Error(id/IVwth), data=dfSPF)
#' etaSq(spf, type=1, anova=TRUE)
#' 


#' @rdname etaSq
#' @family effect.size
#' @concept effect-size
#' @concept variance-analysis
#' @concept anova-effect-size
#' @export
etaSq <- function (fit, type = 2, anova = FALSE) {
  UseMethod("etaSq")
}

#' @rdname etaSq
#' @export
etaSq.lm <- function (fit, type = 2, anova = FALSE) {
  
  # file:    etaSquared.R
  # author:  Dan Navarro
  # contact: djnavarro@protonmail.com
  # changed: 13 November 2013
  # modified by Daniel Wollschlaeger 17.9.2014
  
  # etaSquared() calculates eta-squared and partial eta-squared for linear models
  # (usually ANOVAs). It takes an lm object as input and computes the effect size
  # for all terms in the model. By default uses Type II sums of squares to calculate
  # the effect size, but Types I and III are also possible. By default the output
  # only displays the effect size, but if requested it will also print out the full
  # ANOVA table.
  
  if (!is.logical(anova) || length(anova) != 1) {
    stop("\"anova\" must be a single logical value")
  }
  
  if (!is.numeric(type) || length(type) != 1) {
    stop("type must be equal to 1, 2 or 3")
  }
  
  if (is.null(fit$model)) {
    stop("'fit' does not contain the model frame - refit with model = TRUE")
  }
  
  # stats::anova() is still reachable in call position, but the logical
  # argument of the same name makes every anova(fit) below read like a
  # mistake; alias it once and use the alias.
  anovaTab <- stats::anova
  
  if (type == 1) {
    ss <- anovaTab(fit)[, "Sum Sq", drop = FALSE]
    ss.res <- ss[dim(ss)[1], ]
    ss.tot <- sum(ss)
    ss <- ss[-dim(ss)[1], , drop = FALSE]
    ss <- as.matrix(ss)
  }
  else {
    if (type == 2) {
      ss.tot <- sum((fit$model[, 1] - mean(fit$model[, 1]))^2)
      ss.res <- sum((fit$residuals)^2)
      terms <- attr(fit$terms, "factors")[-1, , drop = FALSE]
      l <- attr(fit$terms, "term.labels")
      ss <- matrix(NA, length(l), 1)
      rownames(ss) <- l
      for (i in seq_along(ss)) {
        vars.this.term <- which(terms[, i] != 0)
        dependent.terms <- which(apply(terms[vars.this.term, , drop = FALSE], 2, prod) > 0)
        m0 <- lm(fit$terms[-dependent.terms], fit$model)
        if (length(dependent.terms) > 1) {
          m1 <- lm(fit$terms[-setdiff(dependent.terms, i)], fit$model)
          ss[i] <- anovaTab(m0, m1)$`Sum of Sq`[2]
        }
        else {
          ss[i] <- anovaTab(m0, fit)$`Sum of Sq`[2]
        }
      }
    }
    else {
      if (type == 3) {
        ## check if model was fitted with sum-to-zero contrasts
        ## necessary for valid SS type 3 (e.g., contr.sum, contr.helmert)
        IVs <- names(attr(model.matrix(fit), "contrasts"))
        ## only relevant for more than one factor
        ## (and for unbalanced cell sizes and interactions, not tested here)
        if(length(IVs) > 1) {
          isSumToZero <- function(IV) {
            ## check if factor has directly associated contrasts
            if(!is.null(attr(fit$model[, IV], "contrasts"))) {
              cm <- contrasts(fit$model[, IV])
              all(colSums(cm) == 0)
            } else {
              ## check attributes from model matrix
              attr(model.matrix(fit), "contrasts")[[IV]] %in% c("contr.sum", "contr.helmert")
            }
          }
          
          valid <- vapply(IVs, isSumToZero, logical(1))
          
          if(!all(valid)) {
            warning(c(ifelse(sum(!valid) > 1, "Factors ", "Factor "),
                      paste(IVs[!valid], collapse=", "),
                      ifelse(sum(!valid) > 1, " are", " is"),
                      " not associated with sum-to-zero contrasts",
                      " necessary for valid SS type III",
                      " when cell sizes are unbalanced",
                      " and interactions are present.",
                      " Consider re-fitting the model after setting",
                      " options(contrasts=c(\"contr.sum\", \"contr.poly\"))"))
          }
        }
        
        mod <- drop1(fit, scope = fit$terms)
        ss <- mod[-1, "Sum of Sq", drop = FALSE]
        ss.res <- mod[1, "RSS"]
        ss.tot <- sum((fit$model[, 1] - mean(fit$model[, 1]))^2)
        ss <- as.matrix(ss)
      }
      else {
        stop("type must be equal to 1, 2 or 3")
      }
    }
  }
  if (anova == FALSE) {
    eta2 <- ss/ss.tot
    eta2p <- ss/(ss + ss.res)
    E <- cbind(eta2, eta2p)
    rownames(E) <- rownames(ss)
    colnames(E) <- c("eta.sq", "eta.sq.part")
  }
  else {
    ss <- rbind(ss, ss.res)
    eta2 <- ss/ss.tot
    eta2p <- ss/(ss + ss.res)
    k <- length(ss)
    eta2p[k] <- NA
    df <- anovaTab(fit)[, "Df"]
    ms <- ss/df
    Fval <- ms/ms[k]
    # lower.tail = FALSE rather than 1 - pf(): the complement loses all
    # precision once the p-value drops below about 1e-16
    p <- pf(Fval, df, rep.int(df[k], k), lower.tail = FALSE)
    E <- cbind(eta2, eta2p, ss, df, ms, Fval, p)
    E[k, 6:7] <- NA
    colnames(E) <- c("eta.sq", "eta.sq.part", "SS", "df", "MS", "F", "p")
    rownames(E) <- rownames(ss)
    rownames(E)[k] <- "Residuals"
  }
  return(E)
}


#' @rdname etaSq
#' @export
etaSq.aovlist <-  function (fit, type = 2, anova = FALSE) {
  
  # author:  Daniel Wollschlaeger
  # contact: contact@dwoll.de
  # changed: 13 October 2014
  
  # etaSq.aovlist() calculates partial eta-squared and generalized eta-squared
  # for aovlists
  
  if (!is.logical(anova) || length(anova) != 1) {
    stop("\"anova\" must be a single logical value")
  }
  
  if (!is.numeric(type) || length(type) != 1) {
    stop("type must be equal to 1, 2 or 3")
  }

  ## alternative: check design has balanced cell sizes
  if (type != 1) {
    stop("type must be equal to 1")
  }
  
  details <- aovlDetails(fit)
  ss      <- details$Sum.Sq             # effect SS
  ss.res  <- sum(aovlErrorTerms(fit)$SS)  # total error SS
  ss.tot  <- sum(ss) + sum(ss.res)
  
  # eta squared
  eta2 <- ss / ss.tot
  
  # partial eta squared
  # cf. Bakeman, R. (2005) Behavior Research Methods. 37(3), 379-384. Tables 1, 2
  eta2p <- ss / (ss + details$SSE)
  
  # generalized eta squared
  # if all factors are manipulated
  # cf. Bakeman, R. (2005) Behavior Research Methods. 37(3), 379-384. Tables 1, 2
  geta2 <- ss / (ss + sum(ss.res))
  
  if (anova == FALSE) {
    E <- cbind(eta2, eta2p, geta2)
    rownames(E) <- details$tt
    colnames(E) <- c("eta.sq", "eta.sq.part", "eta.sq.gen")
  } else {
    E <- data.frame(eta2=eta2,
                    eta2p=eta2p,
                    geta2=geta2,
                    ss=ss,
                    df=details$Df,
                    ms=details$Mean.Sq,
                    sse=details$SSE,
                    dfe=details$dfE,
                    Fval=details$F.value,
                    p=details$Pr..F.)
    colnames(E) <- c("eta.sq", "eta.sq.part", "eta.sq.gen", "SS", "df", "MS", "SSE", "dfE", "F", "p")
    rownames(E) <- details$tt
  }
  return(E)
}



#' @rdname etaSq
#' @export 
aovlDetails <- function(fit) {
  
  # author:  Daniel Wollschlaeger
  
  aovSum  <- summary(fit)
  etNames <- names(aovSum)  # error terms
  
  getOneRes <- function(tt, tab) {  # tab=anova table, tt = tested term
    ttIdx <- which(strTrim(rownames(tab)) == tt)
    list(df=tab[ttIdx,       "Df"],
         SS=tab[ttIdx,       "Sum Sq"],
         MS=tab[ttIdx,       "Mean Sq"],
         dfE=tab["Residuals", "Df"],
         SSE=tab["Residuals", "Sum Sq"],
         MSE=tab["Residuals", "Mean Sq"],
         F=tab[ttIdx, "F value"],
         p=tab[ttIdx, "Pr(>F)"])
  }
  
  getTermRes <- function(et) { # et = error term
    tab <- aovSum[[et]][[1]]
    at  <- strTrim(rownames(tab)) # all terms
    tt  <- at[-which(at == "Residuals")]     # tested terms only
    
    if(length(tt) > 0)
    {
      # error terms
      etRes <- list(df=tab["Residuals", "Df"],
                    SS=tab["Residuals", "Sum Sq"],
                    MS=tab["Residuals", "Mean Sq"])
      ttRes <- lapply(tt, getOneRes, tab=tab)
      ttRes <- setNamesX(ttRes, tt)
      ttIdx <- which(strTrim(rownames(tab)) %in% tt)
      return(data.frame(tt=tt, et=et,
                        tab[ttIdx, , drop=FALSE],
                        dfE=etRes$df, SSE=etRes$SS, MSE=etRes$MS,
                        stringsAsFactors=FALSE))
    } else {
      emptyDf <- data.frame(matrix(ncol=10, nrow=0))
      return(setNamesX(emptyDf, c("tt", "et", "Df", "Sum.Sq", "Mean.Sq", "F.value",
                                 "Pr..F.", "dfE", "SSE", "MSE")))
    }
  }
  
  detailsL  <- setNamesX(lapply(etNames, getTermRes), etNames)
  detailsDf <- do.call("rbind", detailsL)
  rownames(detailsDf) <- NULL
  return(detailsDf)
}



#' @rdname etaSq
#' @export
aovlErrorTerms <- function(fit) {
  aovSum  <- summary(fit)
  etNames <- names(aovSum)
  getSS <- function(z) {
    aovSum[[z]][[1]]["Residuals", "Sum Sq"]
  }
  
  getMS <- function(z) {
    aovSum[[z]][[1]]["Residuals", "Mean Sq"]
  }
  
  getDF <- function(z) {
    aovSum[[z]][[1]]["Residuals", "Df"]
  }
  
  SS <- vapply(etNames, getSS, numeric(1))
  MS <- vapply(etNames, getMS, numeric(1))
  DF <- vapply(etNames, getDF, numeric(1))
  return(list(SS=SS, MS=MS, DF=DF))
}
