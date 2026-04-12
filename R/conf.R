
#' Confusion Matrix and Classification Metrics
#'
#' Computes confusion matrices and a wide range of performance metrics
#' for classification models or predicted vs. observed labels.
#'
#' This is a generic function with methods for tables, vectors, and several
#' model objects (e.g., \code{glm}, \code{rpart}, \code{randomForest}, \code{svm}).
#'
#' @param x Object containing predictions. Can be:
#' \itemize{
#'   \item A factor/vector of predicted classes
#'   \item A confusion matrix (table or matrix)
#'   \item A fitted model object (e.g., \code{glm}, \code{rpart})
#' }
#' @param ref Optional reference (true labels). Required for default method.
#' @param pos Optional character specifying the positive class (for binary classification).
#' If \code{NULL}, the first level is used.
#' @param cutoff Numeric cutoff for probabilistic models (e.g., \code{glm}).
#' @param na.rm Logical; remove missing values before computation.
#' @param ... Further arguments passed to specific methods.
#'
#' @details
#' The function computes:
#' \itemize{
#'   \item Accuracy (with confidence interval)
#'   \item No Information Rate (NIR) and p-value
#'   \item Cohen's Kappa
#'   \item McNemar test p-value
#' }
#'
#' Additionally, class-wise statistics include:
#' \itemize{
#'   \item Sensitivity (Recall)
#'   \item Specificity
#'   \item Positive Predictive Value (Precision)
#'   \item Negative Predictive Value
#'   \item Prevalence
#'   \item Detection Rate and Detection Prevalence
#'   \item Balanced Accuracy
#'   \item F-value (harmonic mean of precision and recall)
#'   \item Matthews Correlation Coefficient (MCC)
#' }
#'
#' For multiclass problems, statistics are computed one-vs-all.
#'
#' @return
#' An object of class \code{"Conf"} containing:
#' \itemize{
#'   \item \code{table} Confusion matrix
#'   \item \code{pos} Positive class (if applicable)
#'   \item \code{diag} Number of correct predictions
#'   \item \code{n} Total number of observations
#'   \item \code{acc}, \code{acc.lci}, \code{acc.uci} Accuracy and CI
#'   \item \code{nri} No Information Rate
#'   \item \code{acc.pval} p-value for Accuracy > NIR
#'   \item \code{kappa} Cohen's Kappa
#'   \item \code{mcnemar.pval} McNemar test p-value
#'   \item \code{byclass} Matrix of class-wise metrics
#' }
#'
#' @examples
#' # Example with vectors
#' pred <- factor(c("A","B","A","A","B"))
#' ref  <- factor(c("A","A","A","B","B"))
#' conf(pred, ref)
#'
#' # Example with table
#' tab <- table(pred, ref)
#' conf(tab)
#'

#' @export
conf <- function(x, ...) UseMethod("conf")



#' @rdname conf
#' @export
conf.table <- function(x, pos = NULL, ...) {
  
  
  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L || p != d[2L]) {    # allow nxn!  || p != 2L)
    stop("'x' is not a nxn numeric matrix.")
    # print(x)
    # invisible()
  }
  
  # observed in columns, predictions in rows
  if(!identical(rownames(x), colnames(x)))
    stop("rownames(x) and colnames(x) must be identical")
  
  if(is.null(pos)) pos <- rownames(x)[1]
  if(nrow(x)!=2) {
    # ignore pos for nxn tables, pos makes only sense for sensitivity
    # and that is not defined for n-dim tables
    pos <- NULL
    
  } else {
    # order 2x2-confusion table so
    # that the positive class is the first and the others keep their position
    # fixed=TRUE as we might run into problems with columnnames like (8-9] ...
    ord <- c(pos, rownames(x)[-grep(pos, rownames(x), fixed=TRUE)])
    # the columnnames must be the same as the rownames
    x <- as.table(x[ord, ord])
  }
  
  # overall statistics first
  res <- list(
    table   = x,
    pos     = pos,
    diag    = sum(diag(x)),
    n       = sum(x)
  )
  res <- c(res,
           acc     = lumen::binomCI(x=res$diag, n=res$n),
           sapply(binom.test(x=res$diag, n=res$n,
                             p=max(apply(x, 2, sum) / res$n),
                             alternative = "greater")[c("null.value", "p.value")], unname),
           kappa   = DescToolsX::cohenKappa(x),
           mcnemar = mcnemar.test(x)$p.value
  )
  names(res) <- c("table","pos","diag","n","acc","acc.lci","acc.uci",
                  "nri","acc.pval","kappa","mcnemar.pval")
  
  # byclass
  lst <- list()
  for(i in 1L:nrow(x)){
    
    z <- .collapseConfTab(x=x, pos=rownames(x)[i])
    z[] <- as.double(z)
    A <- z[1, 1]; B <- z[1, 2]; C <- z[2, 1]; D <- z[2, 2]
    
    lst[[i]] <- rbind(
      sens    = A / (A + C),                 # sensitivity
      spec    = D / (B + D),                 # specificity
      ppv     = A / (A + B),                 # positive predicted value
      npv     = D / (C + D),                 # negative predicted value
      prev    = (A + C) / (A + B + C + D),   # prevalence
      detprev = (A + B) / (A + B + C + D),   # detection prevalence
      detrate = A / (A + B + C + D),         # detection rate
      bacc    = mean(c(A / (A + C), D / (B + D)) ),  # balanced accuracy
      fval    = hmean(c(A / (A + B), A / (A + C)), conf.level = NA), # guetemass wollschlaeger s. 150
      #   this would overflow for already small frequencies if we don't cast z to double ..
      mcc     = (A*D-B*C) / sqrt((A+B)*(A+C)*(D+B)*(D+C))  # Matthews correlation coefficient (=Phi(x) with sign!)
    )
  }
  
  res <- c(res, byclass=list(do.call(cbind, lst)))
  colnames(res[["byclass"]]) <- rownames(x)
  
  if(nrow(x)==2) res[["byclass"]] <- res[["byclass"]][, res[["pos"]], drop=FALSE]
  
  class(res) <- "Conf"
  
  return(res)
  
}


#' @rdname conf
#' @export
conf.default <-  function(x, ref, pos = NULL, na.rm = TRUE, ...) {
  if(na.rm) {
    idx <- complete.cases(data.frame(x, ref))
    x <- x[idx]
    ref <- ref[idx]
  }
  clvl <- combLevels(x, ref)
  
  conf.table(table(Prediction=factor(x, levels=clvl),
                   Reference=factor(ref, levels=clvl)), pos = pos, ...)
  
}

#' @rdname conf
#' @export
conf.matrix <- function(x, pos = NULL, ...) {
  conf.table(as.table(x), pos=pos, ...)
}


#' @rdname conf
#' @export
conf.rpart <- function(x, ...){
  # y <- attr(x, "ylevels")
  conf(x=attr(x,"ylevels")[x$frame$yval[x$where]],
       ref=attr(x,"ylevels")[x$y], ...)
}

#' @rdname conf
#' @export
conf.multinom <- function(x, ...){
  if(is.null(x$model)) stop("x does not contain model. Run multinom with argument model=TRUE!")
  resp <- model.extract(x$model, "response")
  
  # attention: this will not handle correctly responses defined as dummy codes
  # adapt for that!!  ************************************************************
  # resp <- x$response[,1]
  
  pred <- predict(x, type="class")
  conf(x=pred, resp, ... )
}


#' @rdname conf
#' @export
conf.glm <- function(x, cutoff = 0.5, pos=NULL, ...){
  resp <- model.extract(x$model, "response")
  if(is.factor(resp)){
    pred <- levels(resp)[(predict(x, type="response") > cutoff)+1]
    if(is.null(pos)) pos <- levels(resp)[2]
  } else {
    lvl <- levels(factor(resp))
    pred <- lvl[(predict(x, type="response") > cutoff)+1]
    if(is.null(pos)) pos <- lvl[2]
  }
  conf(x=pred, ref=resp, pos=pos, ... )
}


#' @rdname conf
#' @export
conf.randomForest <- function(x, ...){
  conf(x=x$predicted, ref=x$y, ... )
}


#' @rdname conf
#' @export
conf.svm <- function(x, ...){
  
  # old:  Conf(x=predict(x), ref=model.extract(model.frame(x), "response"), ... )
  conf(x=predict(x, type="class"), ref=model.response(model.frame(x)), ... )
}


#' @rdname conf
#' @export
conf.lda <- function(x, ...){
  
  # extract response from the model
  
  conf(x=predict(x)$class,
       ref=model.extract(model.frame(x), "response") , ... )
}

#' @rdname conf
#' @export
conf.qda <- function(x, ...){
  conf(x=predict(x)$class,
       ref=model.extract(model.frame(x), "response") , ... )
}



#' @rdname conf
#' @export
conf.regr <- function(x, ...){
  NextMethod()
  # Conf(x=Predict(x, type="class"), reference=x$response[,], ... )
}


#' @rdname conf
#' @param main Plot title.
#' @export
plot.Conf <- function(x, main="Confusion Matrix", ...){
  mosaicplot(t(x$table), shade=TRUE, main=main, col=c("red", "green"), ...)
}


#' @rdname conf
#' @param digits Number of digits for printing.
#' @export
print.Conf <- function(x, digits = max(3, getOption("digits") - 3), ...) {
  cat("\nConfusion Matrix and Statistics\n\n")
  
  if(all(names(attr(x$table, "dimnames")) == ""))
    names(attr(x$table, "dimnames")) <- c("Prediction","Reference")
  print(x$table, ...)
  
  if(nrow(x$table)!=2) cat("\nOverall Statistics\n")
  
  txt <- gettextf("
                Total n : %s
               Accuracy : %s
                 95%s CI : (%s, %s)
    No Information Rate : %s
    P-Value [Acc > NIR] : %s

                  Kappa : %s
 Mcnemar's Test P-Value : %s\n\n",
                  fm(x$n, digits=0, big.mark="'"),
                  fm(x$acc, digits=digits), "%",
                  fm(x$acc.lci, digits=digits), fm(x$acc.uci, digits=digits),
                  fm(x$nri, digits=digits), fm(x$acc.pval, fmt="p", na.form="NA"),
                  fm(x$kappa, digits=digits), fm(x$mcnemar.pval, fmt="p", na.form="NA")
  )
  cat(txt)
  
  rownames(x$byclass) <- c("Sensitivity", "Specificity", "Pos Pred Value", "Neg Pred Value", "Prevalence",
                           "Detection Rate", "Detection Prevalence", "Balanced Accuracy","F-val Accuracy", "Matthews Cor.-Coef")
  
  if(nrow(x$table)==2){
    cat(
      paste(strPad(paste(rownames(x$byclass), ":"), width=25, adj = "right"),
            fm(x$byclass, digits=digits))
      , sep="\n")
    
    txt <- gettextf("\n       'Positive' Class : %s\n\n", x$pos)
    cat(txt)
    
  } else {
    
    cat("\nStatistics by Class:\n\n")
    print(fm(x$byclass, digits = digits, na.form="NA"), quote = FALSE)
    cat("\n")
    
  }
  
}



#' Extract Sensitivity
#'
#' Convenience function to extract sensitivity from a confusion matrix.
#'
#' @inheritParams conf
#' @return Numeric vector of sensitivities.
#' @export
sens <- function(x, ...) {
  conf(x, ...)[["byclass"]]["sens",]
}

#' Extract Specificity
#'
#' Convenience function to extract specificity from a confusion matrix.
#'
#' @inheritParams conf
#' @return Numeric vector of specificities.
#' @export
spec <- function(x, ...) {
  conf(x, ...)[["byclass"]]["spec",]
}





# == internal helper functions ===============================================

#' @keywords internal
.collapseConfTab <- function(x, pos = NULL, ...) {
  
  if(nrow(x) > 2) {
    names(attr(x, "dimnames")) <- c("pred", "obs")
    x <- collapseTable(x, obs=c("neg", pos)[(rownames(x)==pos)+1],
                       pred=c("neg", pos)[(rownames(x)==pos)+1])
  }
  
  # order confusion table so
  # that the positive class is the first and the others keep their position
  ord <- c(pos, rownames(x)[-grep(pos, rownames(x), fixed=TRUE)])
  # the columnnames must be the same as the rownames
  x <- as.table(x[ord, ord])
  return(x)
}


