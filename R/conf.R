
#' Confusion Matrix and Classification Metrics
#'
#' Computes confusion matrices and a wide range of performance metrics
#' for classification models or predicted vs. observed labels.
#'
#' This is a generic function with methods for tables, vectors, and several
#' model objects (e.g., \code{glm}, \code{rpart}, \code{randomForest},
#' \code{svm}).
#'
#' @param x object containing predictions. Can be:
#'   \itemize{
#'     \item a factor or character vector of predicted classes
#'     \item a confusion matrix (\code{table} or \code{matrix})
#'     \item a fitted model object (e.g., \code{glm}, \code{rpart})
#'   }
#' @param ref optional reference (true labels). Required for the default
#'   method.
#' @param pos optional character specifying the positive class (binary
#'   classification only). If \code{NULL}, the second level is used and
#'   a message is issued.
#' @param cutoff numeric cutoff for probabilistic models (e.g., \code{glm}).
#'   Default \code{0.5}.
#' @param na.rm logical; remove missing values before computation.
#'   Default \code{TRUE}.
#' @param digits integer, number of decimal places for printing.
#' @param main character, plot title.
#' @param \dots further arguments passed to specific methods.
#'
#' @details
#' \strong{Overall statistics:}
#' \itemize{
#'   \item Accuracy with 95\% confidence interval
#'   \item No Information Rate (NIR) and p-value (Accuracy > NIR)
#'   \item Cohen's Kappa
#'   \item McNemar test p-value
#' }
#'
#' \strong{Class-wise statistics} (computed one-vs-all for multiclass):
#' \itemize{
#'   \item Sensitivity (Recall)
#'   \item Specificity
#'   \item Positive Predictive Value (Precision)
#'   \item Negative Predictive Value
#'   \item Prevalence
#'   \item Detection Rate and Detection Prevalence
#'   \item Balanced Accuracy
#'   \item F-value (harmonic mean of Precision and Recall)
#'   \item Matthews Correlation Coefficient (MCC)
#' }
#'
#' @return an object of class \code{"Conf"} containing:
#' \describe{
#'   \item{\code{table}}{confusion matrix}
#'   \item{\code{pos}}{positive class (binary only, else \code{NULL})}
#'   \item{\code{diag}}{number of correct predictions}
#'   \item{\code{n}}{total number of observations}
#'   \item{\code{acc}, \code{acc.lci}, \code{acc.uci}}{accuracy and CI}
#'   \item{\code{nir}}{No Information Rate}
#'   \item{\code{acc.pval}}{p-value for Accuracy > NIR}
#'   \item{\code{kappa}}{Cohen's Kappa}
#'   \item{\code{mcnemar.pval}}{McNemar test p-value}
#'   \item{\code{byclass}}{matrix of class-wise metrics}
#' }
#'
#' @examples
#' # vectors
#' pred <- factor(c("A", "B", "A", "A", "B"))
#' ref  <- factor(c("A", "A", "A", "B", "B"))
#' conf(pred, ref)
#'
#' # table
#' conf(table(pred, ref))
#'
#' # glm
#' m <- glm(am ~ hp + wt, data = mtcars, family = binomial)
#' conf(m)
#'
#' @family confusion
#' @concept classification-metrics
#' @concept descriptive-statistics
#' @concept table-manipulation
#'
#'
#' @export
conf <- function(x, ...) UseMethod("conf")



# -- conf.table ---------------------------------------------------------------

#' @rdname conf
#' @export
conf.table <- function(x, pos = NULL, ...) {
  
  p <- (d <- dim(x))[1L]
  if (!is.numeric(x) || length(d) != 2L || p != d[2L])
    stop("'x' must be a square numeric matrix")
  
  if (!identical(rownames(x), colnames(x)))
    stop("rownames(x) and colnames(x) must be identical")
  
  # -- positive class -----------------------------------------------------------
  if (nrow(x) != 2L) {
    pos <- NULL   # pos only meaningful for binary
  } else {
    if (is.null(pos)) {
      pos <- colnames(x)[2L]
      message(gettextf("'pos' not specified, using '%s' as positive class", pos))
    }
    ord <- c(pos, rownames(x)[-grep(pos, rownames(x), fixed = TRUE)])
    x   <- as.table(x[ord, ord])
  }
  
  # -- overall statistics -----------------------------------------------------------
  diag_n <- sum(diag(x))
  n      <- sum(x)
  
  ci     <- binomCI(x = diag_n, n = n)
  bt     <- binom.test(x    = diag_n,
                       n    = n,
                       p    = max(colSums(x) / n),
                       alternative = "greater")
  
  res <- list(
    table       = x,
    pos         = pos,
    diag        = diag_n,
    n           = n,
    acc         = unname(ci[1L]),
    acc.lci     = unname(ci[2L]),
    acc.uci     = unname(ci[3L]),
    nir         = unname(bt$null.value),
    acc.pval    = unname(bt$p.value),
    kappa       = cohenKappa(x),
    mcnemar.pval = tryCatch(mcnemar.test(x)$p.value, error = function(e) NA_real_)
  )
  
  # -- class-wise statistics -----------------------------------------------------------
  lst <- vector("list", nrow(x))
  
  for (i in seq_len(nrow(x))) {
    z <- .collapseConfTab(x = x, pos = rownames(x)[i])
    z[] <- as.double(z)
    A <- z[1L, 1L]; B <- z[1L, 2L]
    C <- z[2L, 1L]; D <- z[2L, 2L]
    
    den_mcc <- sqrt((A + B) * (A + C) * (D + B) * (D + C))
    
    lst[[i]] <- c(
      sens    = .safeDiv(A, A + C),
      spec    = .safeDiv(D, B + D),
      ppv     = .safeDiv(A, A + B),
      npv     = .safeDiv(D, C + D),
      prev    = .safeDiv(A + C, n),
      detrate = .safeDiv(A, n),
      detprev = .safeDiv(A + B, n),
      bacc    = .safeDiv(A, A + C) / 2 + .safeDiv(D, B + D) / 2,
      fval    = DescToolsX::hmean(c(.safeDiv(A, A + B), 
                                    .safeDiv(A, A + C)),
                      conf.level = NA),
      mcc     = if (den_mcc == 0) NA_real_ else (A * D - B * C) / den_mcc
    )
  }
  
  byclass           <- do.call(cbind, lst)
  colnames(byclass) <- rownames(x)
  
  # for binary: only show the positive class column
  if (nrow(x) == 2L)
    byclass <- byclass[, pos, drop = FALSE]
  
  res$byclass <- byclass
  class(res)  <- "Conf"
  res
}


# -- conf.default -----------------------------------------------------------

#' @rdname conf
#' @export
conf.default <- function(x, ref, pos = NULL, na.rm = TRUE, ...) {
  if (na.rm) {
    idx <- complete.cases(data.frame(x, ref))
    x   <- x[idx]
    ref <- ref[idx]
  }
  clvl <- combLevels(x, ref)
  conf.table(table(Prediction = factor(x,   levels = clvl),
                   Reference  = factor(ref, levels = clvl)),
             pos = pos, ...)
}


# -- conf.matrix -----------------------------------------------------------

#' @rdname conf
#' @export
conf.matrix <- function(x, pos = NULL, ...) {
  conf.table(as.table(x), pos = pos, ...)
}


# -- conf.rpart -----------------------------------------------------------

#' @rdname conf
#' @export
conf.rpart <- function(x, ...) {
  lvl <- attr(x, "ylevels")
  conf(x   = lvl[x$frame$yval[x$where]],
       ref = lvl[x$y], ...)
}


# -- conf.multinom -----------------------------------------------------------

#' @rdname conf
#' @export
conf.multinom <- function(x, ...) {
  if (is.null(x$model))
    stop("'x' does not contain model frame - refit with model = TRUE")
  resp <- model.extract(x$model, "response")
  pred <- predict(x, type = "class")
  conf(x = pred, ref = resp, ...)
}


# -- conf.glm -----------------------------------------------------------

#' @rdname conf
#' @export
conf.glm <- function(x, cutoff = 0.5, pos = NULL, ...) {
  
  resp <- model.extract(x$model, "response")
  lvl  <- if (is.factor(resp)) levels(resp) else levels(factor(resp))
  
  if (length(lvl) != 2L)
    stop("conf.glm requires a binary response - use conf.multinom() for multiclass")
  
  prob <- predict(x, type = "response")
  pred <- lvl[(prob > cutoff) + 1L]
  
  if (is.null(pos)) pos <- lvl[2L]
  
  conf(x = pred, ref = resp, pos = pos, ...)
}


# -- conf.randomForest -----------------------------------------------------------

#' @rdname conf
#' @export
conf.randomForest <- function(x, ...) {
  conf(x = x$predicted, ref = x$y, ...)
}


# -- conf.svm -----------------------------------------------------------

#' @rdname conf
#' @export
conf.svm <- function(x, ...) {
  conf(x   = predict(x, type = "class"),
       ref = model.response(model.frame(x)), ...)
}


# -- conf.lda -----------------------------------------------------------

#' @rdname conf
#' @export
conf.lda <- function(x, ...) {
  conf(x   = predict(x)$class,
       ref = model.extract(model.frame(x), "response"), ...)
}


# -- conf.qda -----------------------------------------------------------

#' @rdname conf
#' @export
conf.qda <- function(x, ...) {
  conf(x   = predict(x)$class,
       ref = model.extract(model.frame(x), "response"), ...)
}


# -- print.Conf -----------------------------------------------------------

#' @rdname conf
#' @export
print.Conf <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {
  
  cat("\nConfusion Matrix and Statistics\n\n")
  
  if (all(names(attr(x$table, "dimnames")) == ""))
    names(attr(x$table, "dimnames")) <- c("Prediction", "Reference")
  print(x$table, ...)
  
  if (nrow(x$table) != 2L) cat("\nOverall Statistics\n")
  
  cat(gettextf("
                Total n : %s
               Accuracy : %s
                 95%s CI : (%s, %s)
    No Information Rate : %s
    P-Value [Acc > NIR] : %s
                  Kappa : %s
 McNemar's Test P-Value : %s\n\n",
               fm(x$n,           digits = 0L, big.mark = "'"),
               fm(x$acc,         digits = digits), "%",
               fm(x$acc.lci,     digits = digits),
               fm(x$acc.uci,     digits = digits),
               fm(x$nir,         digits = digits),
               fm(x$acc.pval,    fmt = "p", naForm = "NA"),
               fm(x$kappa,       digits = digits),
               fm(x$mcnemar.pval, fmt = "p", naForm = "NA")
  ))
  
  rownames(x$byclass) <- c("Sensitivity", "Specificity",
                           "Pos Pred Value", "Neg Pred Value",
                           "Prevalence", "Detection Rate",
                           "Detection Prevalence", "Balanced Accuracy",
                           "F-Value", "Matthews Cor.-Coef.")
  
  if (nrow(x$table) == 2L) {
    cat(paste(strPad(paste0(rownames(x$byclass), " :"),
                     width = 25L, adj = "right"),
              fm(x$byclass, digits = digits)),
        sep = "\n")
    cat(gettextf("\n       'Positive' Class : %s\n\n", x$pos))
    
  } else {
    cat("\nStatistics by Class:\n\n")
    print(fm(x$byclass, digits = digits, naForm = "NA"), quote = FALSE)
    cat("\n")
  }
  
  invisible(x)
}


# -- plot.conf -----------------------------------------------------------

#' @rdname conf
#' @export
plot.Conf <- function(x, main = "Confusion Matrix", ...) {
  mosaicplot(t(x$table), shade = TRUE, main = main, ...)
}


# -- Convenience extractors -----------------------------------------------------------

#' Extract Sensitivity from a Confusion Matrix
#'
#' @inheritParams conf
#' @return named numeric vector of sensitivities.
#' @seealso \code{\link{conf}}, \code{\link{spec}}
#' @family classification
#' @export
sens <- function(x, ...) conf(x, ...)[["byclass"]]["sens", ]

#' Extract Specificity from a Confusion Matrix
#'
#' @inheritParams conf
#' @return named numeric vector of specificities.
#' @seealso \code{\link{conf}}, \code{\link{sens}}
#' @family classification
#' @export
spec <- function(x, ...) conf(x, ...)[["byclass"]]["spec", ]


# == internal helper functions==================================================


# Safe division - returns NA instead of NaN/Inf when denominator is 0
.safeDiv <- function(a, b) ifelse(b == 0, NA_real_, a / b)



#' @keywords internal
.collapseConfTab <- function(x, pos = NULL, ...) {
  if (nrow(x) > 2L) {
    names(attr(x, "dimnames")) <- c("pred", "obs")
    x <- collapseTable(x,
                       obs  = c("neg", pos)[(rownames(x) == pos) + 1L],
                       pred = c("neg", pos)[(rownames(x) == pos) + 1L])
  }
  ord <- c(pos, rownames(x)[-grep(pos, rownames(x), fixed = TRUE)])
  as.table(x[ord, ord])
}

