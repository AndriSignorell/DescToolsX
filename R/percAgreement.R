
#' Percent Agreement with Design-Based SE and CI
#'
#' Computes the proportion of agreement (percent agreement) along with
#' design-based standard errors and confidence intervals following
#' Klein and Gwet.
#'
#' @param x A confusion matrix or a matrix/data.frame of ratings 
#'   (subjects x raters).
#' @param y Optional second rating vector to construct a confusion matrix.
#'
#' @param input Character string specifying the input format:
#'   \code{"auto"}, \code{"confusion"}, or \code{"ratings"}.
#'
#' @param conf.level Confidence level for the interval.
#' @param fpc Finite population correction (default = 0).
#' @param verbose Logical; if TRUE, return detailed output.
#' @param ... Reserved for future extensions.
#'
#' @return Either a numeric vector (estimate and CI) or a list.
#'
#' @family assoc.agreement
#' @concept agreement
#' @concept descriptive-statistics
#' @concept confidence-intervals
#'
#'
#' @export
percAgreement <- function(x, y = NULL,
                          input = c("auto", "confusion", "ratings"),
                          conf.level = 0.95,
                          fpc = 0,
                          verbose = FALSE,
                          ...) {
  
  # --- resolve input ---
  tmp <- .resolveInput(x, y, input, sys.function())
  x <- tmp$x
  input <- tmp$input
  
  # ===============================
  # CONFUSION MATRIX
  # ===============================
  if (input == "confusion") {
    
    if (!isConfusionTable(x)) {
      stop("Input does not appear to be a valid confusion matrix.")
    }
    
    n <- sum(x)
    a <- sum(diag(x))
    Po <- a / n
    
    var_hat <- (1 - fpc) / (n * (n - 1)) *
      sum(c(a, n - a) * (c(1, 0) - Po)^2)
    
    n0 <- NA_integer_
    
  } else {
    
    if (is.list(x)) x <- as.data.frame(x)
    
    n <- nrow(x)
    
    poi <- apply(x, 1, function(row) {
      v <- row[!is.na(row)]
      m <- length(v)
      if (m < 2) return(NA_real_)
      tab <- table(v)
      sum(tab * (tab - 1)) / (m * (m - 1))
    })
    
    n0 <- sum(!is.na(poi))
    Po <- if (n0 > 0) mean(poi, na.rm = TRUE) else NA_real_
    
    ki <- numeric(n)
    if (n0 > 0) ki[!is.na(poi)] <- (n / n0) * poi[!is.na(poi)]
    
    var_hat <- (1 - fpc) / (n * (n - 1)) * sum((ki - Po)^2)
  }
  
  # ===============================
  # SE + CI
  # ===============================
  if (is.na(Po) || n <= 1) {
    se <- NA_real_
    ci <- c(NA_real_, NA_real_)
  } else {
    se <- sqrt(var_hat)
    alpha <- 1 - conf.level
    tcrit <- qt(1 - alpha/2, df = n - 1)
    ci <- c(
      max(0, Po - tcrit * se),
      min(1, Po + tcrit * se)
    )
  }
  
  if (!verbose) {
    return(c(est = Po, lci = ci[1], uci = ci[2]))
  }
  
  list(
    estimate = Po,
    se = se,
    conf.int = ci,
    n = n,
    nPairable = n0,
    method = "Percent agreement (design-based; Klein/Gwet)"
  )
}



# == internal helper functions ===============================================

.resolveInput <- function(x, y, input, fun) {
  
  # --- default handling (DescToolsX style) ---
  if (missing(input)) {
    input <- formals(fun)$input[[1]]
  } else {
    input <- tolower(input[1])
  }
  
  # --- y supplied => force confusion matrix ---
  if (!is.null(y)) {
    x <- normalizeToConfusion(x, y)
    input <- "confusion"
  }
  
  # --- auto detection ---
  if (input == "auto") {
    input <- if (isConfusionTable(x)) "confusion" else "ratings"
  }
  
  # --- validation ---
  if (!(input %in% c("confusion", "ratings"))) {
    stop("Invalid 'input' specification. Must be 'auto', 'confusion', or 'ratings'.")
  }
  
  return(list(x = x, input = input))
}

