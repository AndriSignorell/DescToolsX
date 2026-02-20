

# internal functions

.ChisqWarning <- function(){
  cat(cli::col_red("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n\n"))
}



.notThere <- function(object, ...){
  warning(gettextf('Sorry, no method implemented for class "%s",', 
                   paste(class(object), collapse=", ")))
  NA_real_
}



# internal getOption wrapper for DescToolsX options
.getOption <- function(name, default = NULL) {
  getOption(paste0("DescToolsX.", name), default)
}


# check if the user system supports colors

.has_color <- function() {
  .rstudio_with_ansi_support <- function() {
    if (Sys.getenv("RSTUDIO", "") == "") {
      return(FALSE)
    }
    if ((cols <- Sys.getenv("RSTUDIO_CONSOLE_COLOR", "")) != "" &&
        !is.na(as.numeric(cols))) {
      return(TRUE)
    }
    requireNamespace("rstudioapi", quietly = TRUE) &&
      rstudioapi::isAvailable() &&
      rstudioapi::hasFun("getConsoleHasColor")
  }
  
  .inside_emacs <- function() {
    Sys.getenv("EMACS") != "" || Sys.getenv("INSIDE_EMACS") != ""
  }
  
  .emacs_version <- function() {
    ver <- Sys.getenv("INSIDE_EMACS")
    if (ver == "") {
      return(NA_integer_)
    }
    ver <- gsub("'", "", ver)
    ver <- strsplit(ver, ",", fixed = TRUE)[[1]]
    ver <- strsplit(ver, ".", fixed = TRUE)[[1]]
    as.numeric(ver)
  }
  
  
  ## this is verbatim from crayon
  ## but it's just this function we use, so don't import...
  
  enabled <- getOption("crayon.enabled")
  if (!is.null(enabled)) {
    return(isTRUE(enabled))
  }
  if (.rstudio_with_ansi_support() && sink.number() == 0) {
    return(TRUE)
  }
  if (!isatty(stdout())) {
    return(FALSE)
  }
  if (.Platform$OS.type == "windows") {
    if (Sys.getenv("ConEmuANSI") == "ON") {
      return(TRUE)
    }
    if (Sys.getenv("CMDER_ROOT") != "") {
      return(TRUE)
    }
    return(FALSE)
  }
  if (.inside_emacs() &&
      !is.na(.emacs_version()[1]) &&
      .emacs_version()[1] >= 23) {
    return(TRUE)
  }
  if ("COLORTERM" %in% names(Sys.getenv())) {
    return(TRUE)
  }
  if (Sys.getenv("TERM") == "dumb") {
    return(FALSE)
  }
  grepl("^screen|^xterm|^vt100|color|ansi|cygwin|linux",
        Sys.getenv("TERM"),
        ignore.case = TRUE, perl = TRUE
  )
}



# print routine for charactermatrix

.print.charmatrix <- function(x, quote = FALSE, print.gap = 2,
                              right = TRUE, ...) {
  # prints a character matrix without rownames, by default right aligned and
  # with gap = 2
  # this is used by the print.Desc routines
  
  rownames(x) <- rep("", nrow(x))
  print(x, quote = quote, print.gap = print.gap, right = right, ...)
}



.CaptOut <- function(..., file = NULL, append = FALSE, width=150) {
  
  opt <- options(width=width)
  
  args <- substitute(list(...))[-1L]
  rval <- NULL
  closeit <- TRUE
  if (is.null(file))
    file <- textConnection("rval", "w", local = TRUE)
  else if (is.character(file))
    file <- file(file, if (append)
      "a"
      else "w")
  else if (inherits(file, "connection")) {
    if (!isOpen(file))
      open(file, if (append)
        "a"
        else "w")
    else closeit <- FALSE
  }
  else stop("'file' must be NULL, a character string or a connection")
  sink(file)
  on.exit({
    sink()
    if (closeit) close(file)
    options(opt)
  })
  pf <- parent.frame()
  evalVis <- function(expr) withVisible(eval(expr, pf))
  for (i in seq_along(args)) {
    expr <- args[[i]]
    tmp <- switch(mode(expr), expression = lapply(expr, evalVis),
                  call = , name = list(evalVis(expr)), stop("bad argument"))
    for (item in tmp) if (item$visible)
      print(item$value)
  }
  on.exit(options(opt))
  sink()
  if (closeit)
    close(file)
  if (is.null(rval))
    invisible(NULL)
  else rval
  
}



# Confidence interval for the noncentrality parameter (lambda)
# of a chi-square statistic
# Author: cleaned-up version of Smithson (2001)

.ncp_ci_chisq <- function(chisq, df, conf = 0.95,
                          tol = 1e-6,
                          max_ncp = 1e6) {
  
  if (chisq < 0) stop("chisq must be >= 0")
  if (chisq == 0) return(c(lower = 0, upper = NA))
  
  alpha <- 1 - conf
  target_lower <- alpha / 2
  target_upper <- 1 - alpha / 2
  
  cdf_diff <- function(lambda, target) {
    pchisq(chisq, df = df, ncp = lambda) - target
  }
  
  lower <- tryCatch(
    uniroot(
      cdf_diff,
      interval = c(0, max_ncp),
      target = target_upper,
      tol = tol
    )$root,
    error = function(e) 0
  )
  
  upper <- tryCatch(
    uniroot(
      cdf_diff,
      interval = c(0, max_ncp),
      target = target_lower,
      tol = tol
    )$root,
    error = function(e) NA
  )
  
  c(lower = lower, upper = upper)
}




.normalizeToConfusion <- function(
    x,
    y = NULL,
    levels = NULL,
    useNA = "no",
    mode = c("agreement", "association")
) {
  
  
  # Normalize Input to a Contingency / Agreement Table
  #
  # Normalizes diverse input formats into a contingency table.
  # Supports both agreement measures (e.g. Cohen's Kappa)
  # and association measures (e.g. Cramer's V).
  #
  # @param x Input object (table, matrix, data.frame, list, or vector).
  # @param y Optional second vector of observations.
  # @param levels Optional category levels.
  #   - agreement: atomic vector of common levels
  #   - association: list(x_levels, y_levels)
  # @param useNA Passed to table()
  # @param mode Either "agreement" or "association"
  #
  # @return Numeric matrix (contingency table)
  #
  # @keywords internal
  
  
  mode <- match.arg(mode)
  
  #--------------------------------------------------
  # Helper: build table from two vectors
  #--------------------------------------------------
  two_vec_to_tab <- function(a, b, levels, useNA, mode) {
    
    if (mode == "agreement") {
      
      if (is.null(levels)) {
        levels <- sort(unique(c(a, b)))
      }
      
      a <- factor(a, levels = levels)
      b <- factor(b, levels = levels)
      
    } else { # association
      
      if (is.null(levels)) {
        
        a <- factor(a)
        b <- factor(b)
        
      } else {
        
        if (!is.list(levels) || length(levels) != 2L) {
          stop("For mode='association', 'levels' must be a list(x_levels, y_levels).")
        }
        
        a <- factor(a, levels = levels[[1]])
        b <- factor(b, levels = levels[[2]])
      }
    }
    
    as.matrix(table(a, b, useNA = useNA))
  }
  
  #--------------------------------------------------
  # 1) table input
  #--------------------------------------------------
  if (inherits(x, c("table", "matrix", "array")) && length(dim(x)) == 2L) {
    
    tab <- as.matrix(x)
    
    if (mode == "agreement") {
      
      if (nrow(tab) != ncol(tab)) {
        stop("Agreement measures require a square table.")
      }
      
      if (!do.call(identical, unname(dimnames(tab)))) {
        stop("For agreement measures, row and column names must match.")
      }
      
      if (!is.null(levels)) {
        if (length(levels) != nrow(tab)) {
          stop("'levels' must match table dimensions.")
        }
        dimnames(tab) <- list(levels, levels)
      }
      
    } else { # association
      
      if (!is.null(levels)) {
        if (!is.list(levels) || length(levels) != 2L) {
          stop("For mode='association', 'levels' must be list(x_levels, y_levels).")
        }
        dimnames(tab) <- list(levels[[1]], levels[[2]])
      }
    }
    
    return(tab)
  }
  
  #--------------------------------------------------
  # 2) confusion-like matrix
  #--------------------------------------------------
  if (is.matrix(x) && isConfusionTable(x, require_dimnames = FALSE)) {
    
    tab <- as.matrix(x)
    
    if (mode == "agreement") {
      
      if (nrow(tab) != ncol(tab)) {
        stop("Agreement measures require a square matrix.")
      }
      
      if (!is.null(levels)) {
        dimnames(tab) <- list(levels, levels)
      }
      
    } else {
      
      if (!is.null(levels)) {
        if (!is.list(levels) || length(levels) != 2L) {
          stop("For mode='association', 'levels' must be list(x_levels, y_levels).")
        }
        dimnames(tab) <- list(levels[[1]], levels[[2]])
      }
    }
    
    return(tab)
  }
  
  #--------------------------------------------------
  # 3) Two vectors explicitly provided
  #--------------------------------------------------
  if (!is.null(y)) {
    return(two_vec_to_tab(x, y, levels, useNA, mode))
  }
  
  #--------------------------------------------------
  # 4) matrix with exactly 2 columns
  #--------------------------------------------------
  if (is.matrix(x)) {
    
    if (ncol(x) != 2L) {
      stop("Matrix input must have exactly 2 columns.")
    }
    
    return(two_vec_to_tab(x[, 1], x[, 2], levels, useNA, mode))
  }
  
  #--------------------------------------------------
  # 5) list with exactly 2 elements
  #--------------------------------------------------
  if (is.list(x)) {
    
    if (length(x) != 2L) {
      stop("List input must contain exactly 2 elements.")
    }
    
    return(two_vec_to_tab(x[[1]], x[[2]], levels, useNA, mode))
  }
  
  stop("Unsupported input type or missing second variable.")
}



