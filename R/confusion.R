
#' Confusion Matrix and Classification Metrics
#'
#' Computes confusion matrices and a wide range of performance metrics
#' for classification models or predicted vs. observed labels.
#'
#' This is a generic function with methods for tables, vectors, and several
#' model objects (e.g., `glm`, `rpart`, `randomForest`,
#' `svm`).
#'
#' `sensitivity()` and `specificity()` are convenience extractors for the
#' sensitivity and specificity values computed by `confusion()`.
#'
#' @name confusion
#' @aliases sensitivity specificity
#' 
#' @param x object containing predictions; one of:
#'   \itemize{
#'     \item a factor or character vector of predicted classes
#'     \item a confusion matrix (`table` or `matrix`) with
#'       **predictions in the rows and references in the columns**
#'     \item a fitted model object (e.g., `glm`, `rpart`)
#'   }
#' @param ref optional reference (true labels). Required for the default
#'   method.
#' @param pos optional character specifying the positive class (binary
#'   classification only). If `NULL`, the second level is used and
#'   a message is issued.
#' @param cutoff numeric cutoff for probabilistic models (e.g., `glm`).
#'   Default `0.5`.
#' @param conf.level confidence level for the accuracy interval; defaults
#'   to 0.95
#' @param na.rm logical; if `TRUE`, pairs with a missing prediction or
#'   reference are removed before computation. With the default `FALSE`
#'   such pairs make all statistics `NA`; the table of the complete pairs
#'   is still returned.
#' @param digits integer; number of decimal places for printing
#' @param main character string specifying the plot title
#' @param \dots further arguments passed to specific methods
#'
#' @details
#' The orientation of the table matters: rows are read as predictions and
#' columns as references, so the no-information rate is taken from the
#' column margin. `confusion.default()` builds the table accordingly.
#'
#' **Overall statistics:**
#' \itemize{
#'   \item Accuracy with confidence interval
#'   \item No Information Rate (NIR) and p-value (Accuracy > NIR)
#'   \item Cohen's Kappa
#'   \item McNemar test p-value
#' }
#'
#' **Class-wise statistics** (computed one-vs-all for multiclass):
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
#' @return `confusion()` returns an object of class `"Confusion"` containing:
#' \describe{
#'   \item{`table`}{confusion matrix}
#'   \item{`pos`}{positive class (binary only, else `NULL`)}
#'   \item{`diag`}{number of correct predictions}
#'   \item{`n`}{total number of observations}
#'   \item{`acc`, `accLci`, `accUci`}{accuracy and CI}
#'   \item{`conf.level`}{confidence level used for the accuracy CI}
#'   \item{`nir`}{no-information rate}
#'   \item{`accPValue`}{p-value for accuracy greater than the
#'     no-information rate}
#'   \item{`kappa`}{Cohen's kappa}
#'   \item{`mcnemarPValue`}{McNemar test p-value}
#'   \item{`byclass`}{matrix of class-wise metrics}
#' }
#'
#' `sensitivity()` and `specificity()` return a named numeric vector containing
#' the sensitivity or specificity, respectively, for each reported class.
#'
#' @examples
#' # vectors
#' pred <- factor(c("A", "B", "A", "A", "B"))
#' ref  <- factor(c("A", "A", "A", "B", "B"))
#' confusion(pred, ref)
#'
#' # table
#' confusion(table(pred, ref))
#'
#' # glm
#' m <- glm(am ~ hp + wt, data = mtcars, family = binomial)
#' confusion(m)
#'
#' @family model.classification
#' @concept model-evaluation
#' @concept confusion-matrix
#' @concept classification
#' @export
confusion <- function(x, ...) UseMethod("confusion")



# -- confusion.table ---------------------------------------------------------------

#' @rdname confusion
#' @export
confusion.table <- function(x, pos = NULL, conf.level = 0.95, ...) {

  p <- (d <- dim(x))[1L]
  if (!is.numeric(x) || length(d) != 2L || p != d[2L])
    stop("'x' must be a square numeric matrix")

  if (!identical(rownames(x), colnames(x)))
    stop("rownames(x) and colnames(x) must be identical")

  checkConfLevel(conf.level, allowNA = FALSE)

  # -- positive class -----------------------------------------------------------
  if (nrow(x) != 2L) {
    pos <- NULL   # pos only meaningful for binary
  } else {
    if (is.null(pos)) {
      pos <- colnames(x)[2L]
      message(gettextf("'pos' not specified, using '%s' as positive class", pos))
    }
    if (!pos %in% rownames(x))
      stop(gettextf("'pos' (\"%s\") is not one of the class labels", pos),
           domain = NA)
    x <- as.table(x[.posFirst(rownames(x), pos), .posFirst(rownames(x), pos)])
  }

  # -- overall statistics -----------------------------------------------------------
  diag_n <- sum(diag(x))
  n      <- sum(x)

  ci     <- binomCI(x = diag_n, n = n, conf.level = conf.level)
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
    accLci      = unname(ci[2L]),
    accUci      = unname(ci[3L]),
    conf.level  = conf.level,
    nir         = unname(bt$null.value),
    accPValue   = unname(bt$p.value),
    kappa       = cohenKappa(x),
    mcnemarPValue = tryCatch(mcnemar.test(x)$p.value, error = function(e) NA_real_)
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
      # hmean() lives in this package - the DescToolsX:: prefix made the
      # namespace self-referencing and would break on a rename
      fval    = hmean(c(.safeDiv(A, A + B),
                        .safeDiv(A, A + C)),
                      conf.level = NA),
      mcc     = if (!is.finite(den_mcc) || den_mcc == 0) NA_real_
                else (A * D - B * C) / den_mcc
    )
  }

  byclass           <- do.call(cbind, lst)
  colnames(byclass) <- rownames(x)

  # for binary: only show the positive class column
  if (nrow(x) == 2L)
    byclass <- byclass[, pos, drop = FALSE]

  res$byclass <- byclass
  class(res)  <- "Confusion"
  res
}


# -- confusion.default -----------------------------------------------------------

#' @rdname confusion
#' @export
confusion.default <- function(x, ref, pos = NULL, na.rm = FALSE, ...) {

  if (missing(ref))
    stop("'ref' must be provided for the default method")

  if (length(x) != length(ref))
    stop("'x' and 'ref' must have the same length")

  checkFlag(na.rm)

  # prediction and reference are paired: a pair is missing if either is
  ok  <- complete.cases(data.frame(x, ref))
  x   <- x[ok]
  ref <- ref[ok]

  clvl <- combLevels(x, ref)
  res  <- confusion.table(table(Prediction = factor(x,   levels = clvl),
                                Reference  = factor(ref, levels = clvl)),
                          pos = pos, ...)

  # NA policy: without na.rm, missing values make the statistics NA. The
  # table of the complete pairs is kept - table() would have left the
  # missing ones out without a word, and every figure below with them.
  if (!all(ok) && !na.rm)
    res <- .naConfusion(res)

  res
}


# all statistics of a "Confusion" object set to NA, structure and table kept
.naConfusion <- function(x) {

  for (nm in c("acc", "accLci", "accUci", "nir", "accPValue", "kappa",
               "mcnemarPValue"))
    x[[nm]] <- NA_real_

  x$byclass[] <- NA_real_

  x
}


# -- confusion.matrix -----------------------------------------------------------

#' @rdname confusion
#' @export
confusion.matrix <- function(x, pos = NULL, ...) {
  confusion.table(as.table(x), pos = pos, ...)
}


# -- confusion.rpart -----------------------------------------------------------

#' @rdname confusion
#' @export
confusion.rpart <- function(x, ...) {
  lvl <- attr(x, "ylevels")
  confusion(x   = lvl[x$frame$yval[x$where]],
       ref = lvl[x$y], ...)
}


# -- confusion.multinom -----------------------------------------------------------

#' @rdname confusion
#' @export
confusion.multinom <- function(x, ...) {
  if (is.null(x$model))
    stop("'x' does not contain model frame - refit with model = TRUE")
  resp <- model.extract(x$model, "response")
  pred <- predict(x, type = "class")
  confusion(x = pred, ref = resp, ...)
}


# -- confusion.glm -----------------------------------------------------------

#' @rdname confusion
#' @export
confusion.glm <- function(x, cutoff = 0.5, pos = NULL, ...) {

  if (is.null(x$model))
    stop("'x' does not contain the model frame - refit with model = TRUE")

  resp <- model.extract(x$model, "response")
  lvl  <- if (is.factor(resp)) levels(resp) else levels(factor(resp))

  if (length(lvl) != 2L)
    stop("confusion.glm requires a binary response - use confusion.multinom() for multiclass")

  prob <- predict(x, type = "response")
  pred <- lvl[(prob > cutoff) + 1L]

  if (is.null(pos)) pos <- lvl[2L]

  confusion(x = pred, ref = resp, pos = pos, ...)
}


# -- confusion.randomForest -----------------------------------------------------------

#' @rdname confusion
#' @export
confusion.randomForest <- function(x, ...) {
  confusion(x = x$predicted, ref = x$y, ...)
}


# -- confusion.svm -----------------------------------------------------------

#' @rdname confusion
#' @export
confusion.svm <- function(x, ...) {
  # predict.svm() has no 'type' argument - it was silently swallowed by
  # its dots and had no effect
  confusion(x   = predict(x),
       ref = model.response(model.frame(x)), ...)
}


# -- confusion.lda / confusion.qda ---------------------------------------------------

#' @rdname confusion
#' @export
confusion.lda <- function(x, ...) {
  confusion(x   = predict(x)$class,
       ref = model.extract(model.frame(x), "response"), ...)
}


#' @rdname confusion
#' @export
confusion.qda <- function(x, ...) confusion.lda(x, ...)


# -- print.Confusion -----------------------------------------------------------

#' @rdname confusion
#' @export
print.Confusion <- function(x, digits = max(3L, getOption("digits") - 3L), ...) {

  cat("\nConfusion Matrix and Statistics\n\n")

  if (all(names(attr(x$table, "dimnames")) == ""))
    names(attr(x$table, "dimnames")) <- c("Prediction", "Reference")
  print(x$table, ...)

  if (nrow(x$table) != 2L) cat("\nOverall Statistics\n")

  # the level was hard-coded as 95% in the label while the interval itself
  # came from binomCI()'s default
  cat(gettextf("
                Total n : %s
               Accuracy : %s
                %s%s CI : (%s, %s)
    No Information Rate : %s
    P-Value [Acc > NIR] : %s
                  Kappa : %s
 McNemar's Test P-Value : %s\n\n",
               fm(x$n,           digits = 0L, bigMark = "'"),
               fm(x$acc,         digits = digits),
               fm(100 * coalesceX(x$conf.level, 0.95), digits = 0L), "%",
               fm(x$accLci,      digits = digits),
               fm(x$accUci,      digits = digits),
               fm(x$nir,         digits = digits),
               fm(x$accPValue,   fmt = "p", naForm = "NA"),
               fm(x$kappa,       digits = digits),
               fm(x$mcnemarPValue, fmt = "p", naForm = "NA")
  ))

  rownames(x$byclass) <- c("Sensitivity", "Specificity",
                           "Pos Pred Value", "Neg Pred Value",
                           "Prevalence", "Detection Rate",
                           "Detection Prevalence", "Balanced Accuracy",
                           "F-Value", "Matthews Cor.-Coef.")

  if (nrow(x$table) == 2L) {
    cat(paste(strPad(paste0(rownames(x$byclass), " :"),
                     width = 25L, align = "right"),
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


# -- plot.Confusion -----------------------------------------------------------

#' @rdname confusion
#' @export
plot.Confusion <- function(x, main = "Confusion Matrix", ...) {
  mosaicplot(t(x$table), shade = TRUE, main = main, ...)
}


# -- Convenience extractors -----------------------------------------------------------

#' @rdname confusion
#' @export
sensitivity <- function(x, ...) confusion(x, ...)[["byclass"]]["sens", ]


#' @rdname confusion
#' @export
specificity <- function(x, ...) confusion(x, ...)[["byclass"]]["spec", ]


# == internal helper functions==================================================


# Safe division - returns NA instead of NaN/Inf when denominator is 0
.safeDiv <- function(a, b) ifelse(b == 0, NA_real_, a / b)


# Reorder class labels so that 'pos' comes first.
#
# This replaces `c(pos, rownames(x)[-grep(pos, rownames(x), fixed = TRUE)])`.
# grep() matches SUBSTRINGS: with labels c("A", "AB") and pos = "A" it hit
# both, the negative index dropped both, and the table silently collapsed
# to 1x1 - every statistic downstream was then computed from a single
# cell, with no error anywhere. setdiff() matches whole strings.
#' @noRd
.posFirst <- function(labels, pos) c(pos, setdiff(labels, pos))


.collapseConfTab <- function(x, pos = NULL, ...) {
  if (nrow(x) > 2L) {
    names(attr(x, "dimnames")) <- c("pred", "obs")
    x <- collapseTable(x,
                       obs  = c("neg", pos)[(rownames(x) == pos) + 1L],
                       pred = c("neg", pos)[(rownames(x) == pos) + 1L])
  }
  ord <- .posFirst(rownames(x), pos)
  as.table(x[ord, ord])
}
