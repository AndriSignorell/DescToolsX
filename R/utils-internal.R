

# internal utilities for the specific package of the DescToolsX ecosystem


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
.hasColor <- function() {
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



.captOut <- function(..., file = NULL, append = FALSE, width=150) {
  
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

.chisqNcpCI <- function(chisq, df, conf = 0.95,
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


