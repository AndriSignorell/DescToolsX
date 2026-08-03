
# Shared result shaping for the ordinal association wrappers
# (kendallTauA, kendallTauB, somersDelta, stuartTauC, gkGamma).
#
# All of them read ordAssocs()' first list element and, when an interval was
# requested, label it est/lci/uci. The length check lives here so that a
# helper returning something of unexpected length is reported as such,
# instead of surfacing as a setNamesX() error about attribute lengths in
# whichever wrapper happened to be called.
#
# In its own file rather than appended to one of the callers: a top-level
# name defined inside a function's file is easy to lose when that file is
# reorganised, and easy to define twice when the next caller needs it.

.ordAssocResult <- function(res, conf.level) {
  
  est <- unname(res[[1L]])
  
  if(is.na(conf.level)) {
    
    if(length(est) != 1L)
      stop(gettextf("ordAssocs() returned %d values where a single estimate was expected.",
                    length(est)), domain = NA)
    
    est
    
  } else {
    
    if(length(est) != 3L)
      stop(gettextf("ordAssocs() returned %d values where estimate and interval were expected.",
                    length(est)), domain = NA)
    
    setNamesX(est, c("est", "lci", "uci"))
    
  }
}


