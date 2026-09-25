

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



# does the console show ANSI colours? cli::num_ansi_colors() does the
# detection that used to be copied verbatim from crayon here (RStudio,
# Emacs, ConEmu, TERM, ...) - and keeps it current, and honours NO_COLOR
# and options(cli.num_colors = ). cli is imported anyway.
.hasColor <- function() {
  cli::num_ansi_colors() > 1L
}



# capture.output() with a wider console, so that long test statistics are
# not wrapped before a caller picks out a line. The former hand-rolled
# sink()/textConnection() version was a copy of capture.output() itself,
# with two on.exit() handlers of which the second silently replaced the
# first.
.captOut <- function(..., file = NULL, append = FALSE, width = 150) {
  opt <- options(width = width)
  on.exit(options(opt))
  # list(...) first: capture.output() evaluates its arguments in ITS
  # parent frame, which would be this function, not the caller - handing
  # the dots through would look up x$chisq.test in the wrong place
  objs <- list(...)
  capture.output(for (o in objs) print(o), file = file, append = append)
}



.makeEstimateResult <- function(
    est,
    lci = NULL,
    uci = NULL,
    attrs = NULL
){
  
  # unname() throughout: values arriving from quantile() carry names such
  # as "2.5%", and c(res, lci = <named scalar>) would compose these into
  # "lci.2.5%", breaking the binding est/lci/uci output convention.
  res <- c(est = unname(est))
  
  if(!is.null(lci))
    res <- c(res, lci = unname(lci))
  
  if(!is.null(uci))
    res <- c(res, uci = unname(uci))
  
  # Set attributes individually rather than assigning the whole
  # attributes() list: a wholesale assignment would drop or overwrite
  # 'names' if attrs ever carried an entry of that name.
  if(!is.null(attrs) && length(attrs)) {
    
    for(nm in names(attrs))
      attr(res, nm) <- attrs[[nm]]
    
  }
  
  res
  
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

  # pchisq(chisq, df, lambda) falls in lambda, so its value at lambda = 0
  # is the largest it can take. Where even that is below the target, the
  # bound is 0 - decided here, not inferred from a failing uniroot().
  p0 <- pchisq(chisq, df = df)

  # The roots lie near chisq - df. A fixed bracket of 1e6 did not reach
  # them for larger statistics: uniroot() failed, and the error handler
  # turned the lower bound into 0.
  hi <- max(max_ncp, 10 * chisq)

  # pchisq() with ncp is documented as reliable up to about 1e5; beyond
  # that pnchisq() stops with "not converged in 1000000 iter." and a
  # warning per call. There the noncentral chi-squared is a normal
  # distribution with mean df + ncp and variance 2 (df + 2 ncp) to a
  # skewness below 0.01, which is ample for locating a quantile.
  cdf <- function(lambda) {
    if (lambda <= 1e5)
      pchisq(chisq, df = df, ncp = lambda)
    else
      pnorm((chisq - df - lambda) / sqrt(2 * (df + 2 * lambda)))
  }

  root <- function(target) {
    if (p0 <= target)
      return(0)
    uniroot(function(lambda) cdf(lambda) - target,
            interval = c(0, hi), tol = tol)$root
  }

  c(lower = root(1 - alpha / 2), upper = root(alpha / 2))
}
