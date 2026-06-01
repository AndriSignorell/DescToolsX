
#' Compare multiple statistical models
#'
#' @description
#' Creates a unified comparison object for multiple fitted models by
#' extracting coefficients, confidence intervals, and model statistics.
#'
#' @details
#' The function standardizes model output across different model classes
#' (e.g. \code{lm}, \code{glm}, \code{coxph}, \code{gam}, \code{lmer}) using
#' S3 methods implemented via \code{tmodSummary()}. This enables direct
#' comparison of model coefficients and fit statistics in tabular and
#' graphical form.
#'
#' Model names are automatically derived from the call. If unavailable,
#' default names are assigned.
#'
#' @param ... Fitted model objects
#' @param FUN Formatting function applied to coefficients
#' @param order Optional ordering of models
#' @param verbose Logical; if \code{TRUE}, show extended statistics
#'
#' @return
#' An object of class \code{"TMod"} with components:
#' \itemize{
#'   \item \code{m}: formatted coefficient table
#'   \item \code{mm}: model statistics
#'   \item \code{mall}: array of estimates and confidence intervals
#'   \item \code{terms}: mapping of model terms
#' }
#'
#' @seealso \code{\link{tmodSummary}}
#'
#' @family topic.modelComparison
#' @concept regression
#' @concept model comparison
#' @concept coefficient analysis
#'
#' @examples
#' # --- Linear models ---
#' m1 <- lm(mpg ~ wt, data = mtcars)
#' m2 <- lm(mpg ~ wt + hp, data = mtcars)
#' tMod(m1, m2)
#'
#' # --- Generalized linear models ---
#' g1 <- glm(am ~ wt, data = mtcars, family = binomial)
#' g2 <- glm(am ~ wt + hp, data = mtcars, family = binomial)
#' tMod(g1, g2)
#'
#' # --- Survival models ---
#' if(requireNamespace("survival", quietly = TRUE)){
#'   library(survival)
#'   s1 <- coxph(Surv(time, status) ~ age, data = lung)
#'   s2 <- coxph(Surv(time, status) ~ age + sex, data = lung)
#'   tMod(s1, s2)
#' }



#' @export
tMod <- function(..., FUN = NULL, order = NA, verbose = FALSE){
  

  if(is.null(FUN)){
    FUN <- function(est, se, tval, pval, lci, uci){
      res <- gettextf("%s %s",
                      fm(est, fmt=style("num.sty")),
                      fm(pval, fmt="*"))
      replace(res, is.na(est), NA)
    }
  }
  
  lmod <- list(...)
  lst  <- lapply(lmod, tmodSummary)
  

  mc <- match.call(expand.dots = FALSE)
  
  modname <- names(lmod)
  
  # important: convert NULL to NA
  if(is.null(modname))
    modname <- rep(NA_character_, length(lmod))
  
  # detect missing names (NA OR “”)
  missingName <- is.na(modname) | modname == ""
  
  # get call labels 
  callNames <- vapply(mc$..., .callToLabel, character(1))
  
  # replace
  modname[missingName] <- callNames[missingName]
  
  # finaler fallback
  stillMissing <- is.na(modname) | modname == ""
  modname[stillMissing] <- paste0("Model_", which(stillMissing))
  

  
  # -------------------------
  # COEFFICIENT TABLE
  # -------------------------
  coefOrder <- unique(unlist(lapply(lst, function(x) x$coef$name)))
  
  m <- data.frame(coef = coefOrder)
  
  for(i in seq_along(lst)){
    tmp <- lst[[i]]$coef
    
    vals <- FUN(tmp$est, tmp$se, tmp$stat, tmp$p, tmp$lci, tmp$uci)
    
    m[[modname[i]]] <- vals[match(coefOrder, tmp$name)]
  }
  
  # -------------------------
  # STATS TABLE
  # -------------------------
  allStats <- unique(unlist(lapply(lst, function(x) names(x$statsx))))
  
  mm <- data.frame(stat = allStats)
  
  for(i in seq_along(lst)){
    tmp <- lst[[i]]$statsx
    mm[[modname[i]]] <- tmp[match(allStats, names(tmp))]
  }
  
  # -------------------------
  # ARRAY (mall)
  # -------------------------
  est <- .getArray(lst, "est", coefOrder)
  lci <- .getArray(lst, "lci", coefOrder)
  uci <- .getArray(lst, "uci", coefOrder)
  
  mall <- array(
    NA_real_,
    dim = c(length(coefOrder), length(lst), 3),
    dimnames = list(
      coefOrder,
      modname,
      c("est","lci","uci")
    )
  )
  
  mall[,,1] <- est
  mall[,,2] <- lci
  mall[,,3] <- uci
  
  # -------------------------
  # TERMS
  # -------------------------
  mterms <- lapply(lmod, function(m){
    res <- lapply(labels(terms(m)), function(x)
      colnames(model.matrix(as.formula(paste0("~0+", x)), data=model.frame(m))))
    names(res) <- labels(terms(m))
    res
  })
  
  names(mterms) <- modname
  
  structure(
    list(m, mm, lcoef = lapply(lst, `[[`, "coef"),
         mall = mall, terms = mterms, verbose = verbose),
    class = "TMod"
  )
  
}



#' @rdname tMod
#' @param x A \code{"TMod"} object
#' @param digits Number of digits for printing
#' @param naForm String used for missing values
#' @export
print.TMod <- function(x, digits=3, naForm = "-", verbose = NULL, ...){
  
  verbose <- if(!is.null(verbose)) verbose else x$verbose
  
  if(!verbose){
    x[[2]] <- x[[2]][match(
      c("adj.r.squared","AIC","N","NAs","n vars","n coef","MAE","RMSE","McFadden"),
      x[[2]]$stat, nomatch = 0), ]
  }
  
  colnames(x[[1]])[-1] <- paste0(colnames(x[[1]])[-1], strrep(" ", 4))
  x[[1]][, -1] <- fm(x[[1]][, -1], digits=digits, naForm = naForm)
  
  x2 <- x[[2]]
  x[[2]][, -1] <- fm(x[[2]][, -1], digits=digits, naForm = naForm)
  
  idx <- x[[2]]$stat %in% c("numdf","dendf","N","NAs","n vars","n coef")
  x[[2]][idx, -1] <- fm(x2[idx, -1], digits=0, naForm=naForm)
  
  mm <- x[[2]]
  colnames(mm) <- colnames(x[[1]])
  
  m <- rbind(
    x[[1]],
    setNamesX(c("---", rep("", ncol(x[[1]]) -1)), colnames(x[[1]])),
    mm
  )
  
  m[, -1] <- lapply(m[, -1, drop=FALSE], strAlign, sep=".")
  row.names(m) <- NULL
  
  print(m, ...)
}


#' @param terms Optional character vector specifying which model terms
#'   should be displayed in the plot. If \code{NULL}, all terms are shown.
#' @param intercept Logical; if \code{FALSE} (default), intercept terms are
#'   excluded from the plot.
#'   
#' @rdname tMod
#' @export
plot.TMod <- function(x, terms=NULL, intercept=FALSE, ...){
  
  if(length(dim(x$mall)) > 2)
    xx <- aperm(x$mall, perm = c(2, 1, 3))
  else
    xx <- x$mall
  
  if(!is.null(terms)){
    coefnames <- unique(unlist(lapply(x$terms, function(tt)
      unlist(tt[names(tt) %in% terms])
    )))
    xx <- xx[, dimnames(xx)[[2]] %in% coefnames, , drop=FALSE]
  }
  
  if(!intercept)
    xx <- xx[, !grepl("intercept", dimnames(xx)[[2]], ignore.case = TRUE), , drop=FALSE]
  
  args.plotdot1 <- list(
    x = xx[,,1],
    pch = 21,
    bg  = "white",
    args.errbars = list(
      from = xx[,,2],
      to   = xx[,,3],
      mid  = xx[,,1]
    )
  )
  
  dots <- match.call(expand.dots = FALSE)$`...`
  if (!is.null(dots))
    args.plotdot1[names(dots)] <- dots
  
  do.call(aurora::plotDot, args.plotdot1)
}




# == internal helper functions =========================================



.getArray <- function(lst, var, coefOrder){
  
  mat <- sapply(lst, function(x){
    
    v <- x$coef[[var]]
    names(v) <- x$coef$name
    
    v[match(coefOrder, names(v))]
  })
  
  if(is.null(dim(mat)))
    mat <- matrix(mat, ncol = 1)
  
  rownames(mat) <- coefOrder
  mat
}



.callToLabel <- function(expr){
  lab <- deparse(expr)
  
  if(length(lab) > 1)
    lab <- lab[1]
  
  lab <- gsub("\n", "", lab)
  
  if(nchar(lab) > 30)
    lab <- substr(lab, 1, 30)
  
  lab
}

