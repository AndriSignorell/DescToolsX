
#' Parse and classify formula input for rank-based tests
#'
#' Internal helper function to parse a model formula and construct a
#' \code{model.frame} suitable for rank-based tests such as Wilcoxon,
#' Kruskal–Wallis, or Friedman tests.
#'
#' The function supports the following formula types:
#'
#' \itemize{
#'   \item \code{y ~ 1} or \code{y} (one-sample test)
#'   \item \code{y ~ g} (two- or n-sample test)
#'   \item \code{y ~ trt | block} (Friedman test for paired samples)
#' }
#'
#' Depending on the structure of the formula and the \code{allowed}
#' argument, the function determines the appropriate test type and
#' returns a structured list describing the design.
#'
#' @param formula A model formula. Supported forms are:
#'   \describe{
#'     \item{\code{y ~ 1}}{one-sample test}
#'     \item{\code{y ~ g}}{group comparison (two- or n-sample)}
#'     \item{\code{y ~ trt | block}}{paired design (Friedman test)}
#'   }
#' @param data An optional data frame (or matrix coercible to data frame)
#'   containing the variables in the formula.
#' @param subset An optional expression indicating which observations
#'   should be used. Must be captured in the calling function via
#'   \code{substitute()}.
#' @param na.action A function specifying how missing values are handled.
#'   Defaults to \code{na.pass}.
#' @param allowed A character vector specifying which test types are
#'   permitted. Possible values are:
#'   \code{"one.sample"}, \code{"paired"},
#'   \code{"two.sample"}, \code{"n.sample"}.
#'
#' @return A list describing the detected design. The list always contains:
#'   \itemize{
#'     \item \code{type}: character string indicating the test type
#'     \item \code{method}: suggested rank-based method
#'     \item \code{mf}: the constructed model frame
#'     \item \code{data.name}: descriptive name of the data
#'   }
#'
#'   Additional components depend on the design:
#'   \describe{
#'     \item{One-sample}{\code{x}}
#'     \item{Paired}{\code{x}, \code{y} (or \code{response}, \code{group}, \code{block} for Friedman)}
#'     \item{Two-sample}{\code{x}, \code{y}, \code{group}}
#'     \item{n-sample}{\code{x}, \code{group}}
#'   }
#'
#' @details
#' The function internally calls \code{stats::model.frame} to evaluate
#' the formula. Special care is required when passing \code{subset}
#' and \code{na.action}, as these must be captured as expressions in
#' the calling environment to avoid name collisions (e.g. with the
#' \code{subset()} function).
#'
#' Matrix inputs for \code{data} are coerced to data frames.
#'
#' This function does not perform any statistical test itself; it only
#' parses and classifies the formula input.
#'
#' @keywords internal
#' @noRd

.parseFormula <- function(
    formula, data, subset, na.action = na.pass,
    allowed = c("one.sample", "paired", "two.sample", "n.sample")
) {
  if (missing(formula))
    stop("'formula' missing")
  
  ## -------------------------------
  ## 1) Friedman-Formel erkennen
  ## y ~ trt | block
  ## -------------------------------
  if (length(formula) == 3L &&
      is.call(formula[[3L]]) &&
      formula[[3L]][[1L]] == as.name("|")) {
    
    if (!"paired" %in% allowed)
      stop("paired samples not allowed")
    
    f2 <- formula
    f2[[3L]][[1L]] <- as.name("+")
    
    m <- match.call(expand.dots = FALSE)
    m$formula <- f2
    m$allowed <- NULL
    m$... <- NULL   # <<< GANZ WICHTIG
    
    if (is.matrix(eval(m$data, parent.frame())))
      m$data <- as.data.frame(data)
    
    m[[1L]] <- quote(stats::model.frame)
    
    # ## >>> IMPORTANT: Treat subset correctly due to collision with 
    # ##                the subset function.
    # ##
    # ## --- capture subset / na.action in CALLING FUNCTION as follows ---
    # ## subset_expr <- if (!missing(subset)) substitute(subset) else NULL
    # ## na_expr     <- if (!missing(na.action)) substitute(na.action) else NULL
    
    ## subset is then already an expression or NULL
    m$subset <- subset
    m$na.action <- na.action
    
    mf <- eval(m, parent.frame())
    
    if (ncol(mf) != 3L)
      stop("incorrect specification for 'formula'")
    
    return(list(
      type     = "paired",
      method   = "friedman",
      mf       = mf,
      response = mf[[1L]],
      group    = mf[[2L]],
      block    = mf[[3L]],
      data.name = paste(names(mf), collapse = " and ")
    ))
  }
  
  ## -------------------------------
  ## 2) Alle anderen: y ~ x
  ## -------------------------------
  if (length(formula) != 3L)
    stop("'formula' missing or incorrect")
  
  m <- match.call(expand.dots = FALSE)
  m$allowed <- NULL
  m$... <- NULL   # <<< GANZ WICHTIG
  
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  
  m[[1L]] <- quote(stats::model.frame)
  
  # ## >>> IMPORTANT: Treat subset correctly due to collision with 
  # ##                the subset function.
  # ##
  # ## --- capture subset / na.action in CALLING FUNCTION as follows ---
  # ## subset_expr <- if (!missing(subset)) substitute(subset) else NULL
  # ## na_expr     <- if (!missing(na.action)) substitute(na.action) else NULL
  
  ## subset is then already an expression or NULL
  m$subset <- subset
  m$na.action <- na.action
  
  mf <- eval(m, parent.frame())
  
  if (ncol(mf) > 2L)
    stop("'formula' should be of the form response ~ group")
  
  response <- mf[[1L]]
  
  ## -------------------------------
  ## 2a) One-sample oder gepaart
  ## -------------------------------
  if (ncol(mf) == 1L || formula[[3L]] == 1L) {
    
    if (!("one.sample" %in% allowed || "paired" %in% allowed))
      stop("one-sample / paired tests not allowed")
    
    if (inherits(response, "Pair")) {
      return(list(
        type     = "paired",
        method   = "wilcox",
        mf       = mf,
        x        = response[, 1L],
        y        = response[, 2L],
        data.name = names(mf)
      ))
    } else {
      return(list(
        type     = "one.sample",
        method   = "wilcox",
        mf       = mf,
        x        = response,
        data.name = names(mf)
      ))
    }
  }
  
  ## -------------------------------
  ## 2b) Gruppierte Stichproben
  ## -------------------------------
  g <- factor(mf[[2L]])
  k <- nlevels(g)
  
  ## --------------------------------
  ## grouped samples
  ## --------------------------------
  if (k >= 2L) {
    
    ## prefer two-sample ONLY if explicitly allowed
    if (k == 2L && "two.sample" %in% allowed) {
      
      DATA <- split(response, g)
      
      return(list(
        type      = "two.sample",
        method    = "wilcox",
        mf        = mf,
        x         = DATA[[1L]],
        y         = DATA[[2L]],
        group     = g,
        data.name = paste(names(mf), collapse = " by ")
      ))
    }
    
    ## otherwise treat as n-sample (also valid for k == 2)
    if ("n.sample" %in% allowed) {
      
      return(list(
        type      = "n.sample",
        method    = "kruskal",
        mf        = mf,
        x         = response,
        group     = g,
        data.name = paste(names(mf), collapse = " by ")
      ))
    }
    
    stop("grouped tests not allowed")
  }
  
  if (k > 2L) {
    if (!"n.sample" %in% allowed)
      stop("n-sample tests not allowed")
    
    return(list(
      type     = "n.sample",
      method   = "kruskal",
      mf       = mf,
      x        = response,
      group    = g,
      data.name = paste(names(mf), collapse = " by ")
    ))
  }
  
  stop("invalid grouping structure")
}

# base was:
#   
#   stats:::wilcox.test.formula
# function (formula, data, subset, na.action = na.pass, ...) 
# {
#   if (missing(formula) || (length(formula) != 3L)) 
#     stop("'formula' missing or incorrect")
#   if ("paired" %in% ...names()) 
#     stop("cannot use 'paired' in formula method")
#   oneSampleOrPaired <- FALSE
#   if (length(attr(terms(formula[-2L]), "term.labels")) != 1L) 
#     if (formula[[3L]] == 1L) 
#       oneSampleOrPaired <- TRUE
#   else stop("'formula' missing or incorrect")
#   m <- match.call(expand.dots = FALSE)
#   if (is.matrix(eval(m$data, parent.frame()))) 
#     m$data <- as.data.frame(data)
#   m[[1L]] <- quote(stats::model.frame)
#   m$... <- NULL
#   mf <- eval(m, parent.frame())
#   DNAME <- paste(names(mf), collapse = " by ")
#   names(mf) <- NULL
#   response <- attr(attr(mf, "terms"), "response")
#   if (!oneSampleOrPaired) {
#     g <- factor(mf[[-response]])
#     if (nlevels(g) != 2L) 
#       stop("grouping factor must have exactly 2 levels")
#     DATA <- split(mf[[response]], g)
#     y <- wilcox.test(x = DATA[[1L]], y = DATA[[2L]], ...)
#   }
#   else {
#     respVar <- mf[[response]]
#     if (inherits(respVar, "Pair")) {
#       y <- wilcox.test(x = respVar[, 1L], y = respVar[, 
#                                                       2L], paired = TRUE, ...)
#     }
#     else {
#       y <- wilcox.test(x = respVar, ...)
#     }
#   }
#   y$data.name <- DNAME
#   y
# }
# 
# stats:::kruskal.test.formula
# function (formula, data, subset, na.action, ...) 
# {
#   if (missing(formula) || (length(formula) != 3L)) 
#     stop("'formula' missing or incorrect")
#   m <- match.call(expand.dots = FALSE)
#   if (is.matrix(eval(m$data, parent.frame()))) 
#     m$data <- as.data.frame(data)
#   m[[1L]] <- quote(stats::model.frame)
#   mf <- eval(m, parent.frame())
#   if (length(mf) > 2L) 
#     stop("'formula' should be of the form response ~ group")
#   DNAME <- paste(names(mf), collapse = " by ")
#   y <- kruskal.test(x = mf[[1L]], g = mf[[2L]])
#   y$data.name <- DNAME
#   y
# }
# 
# stats:::friedman.test.formula
# function (formula, data, subset, na.action, ...) 
# {
#   if (missing(formula)) 
#     stop("formula missing")
#   if ((length(formula) != 3L) || (length(formula[[3L]]) != 
#                                   3L) || (formula[[3L]][[1L]] != as.name("|")) || (length(formula[[3L]][[2L]]) != 
#                                                                                    1L) || (length(formula[[3L]][[3L]]) != 1L)) 
#     stop("incorrect specification for 'formula'")
#   formula[[3L]][[1L]] <- as.name("+")
#   m <- match.call(expand.dots = FALSE)
#   m$formula <- formula
#   if (is.matrix(eval(m$data, parent.frame()))) 
#     m$data <- as.data.frame(data)
#   m[[1L]] <- quote(stats::model.frame)
#   mf <- eval(m, parent.frame())
#   DNAME <- paste(names(mf), collapse = " and ")
#   y <- friedman.test(mf[[1L]], mf[[2L]], mf[[3L]])
#   y$data.name <- DNAME
#   y
# }
