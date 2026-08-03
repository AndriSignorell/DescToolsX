
# == internal: open the irrelevant side of a confidence interval ===============
#
# One implementation, because five hand-written copies of the same three
# lines produced four different bugs across this review: cohenD had it
# inverted and mis-indexed, contCoef inverted, gini ignored it after
# doubling alpha, hmean returned NA where a boundary belongs.
#
# design_rules.md 4.1: 'sides' names the side carrying the FINITE bound.
#   "left"  -> [lci, hi)   the informative bound is the lower one
#   "right" -> (lo, uci]   the informative bound is the upper one
#
# lo/hi are the parameter's range, not infinities. A bounded statistic
# reports the open side at its boundary (ccc: 1 / -1, kappa likewise, a
# contingency coefficient 0 / sqrt((m-1)/m)); an unbounded one passes
# +/-Inf and gets the usual half-line. Cronbach's alpha is the case that
# needs both: hi = 1, lo = -Inf.
#
# The two-sided interval is clamped to [lo, hi] as well, so an interval
# can never claim a value the statistic cannot take.
#' @noRd
.applySides <- function(ci, sides = "two.sided",
                        lo = -Inf, hi = Inf) {
  
  if (!is.numeric(ci) || length(ci) != 2L)
    stop("'ci' must be a numeric vector of length 2")
  
  if (!is.numeric(lo) || length(lo) != 1L || is.na(lo) ||
      !is.numeric(hi) || length(hi) != 1L || is.na(hi) ||
      lo > hi)
    stop("'lo' and 'hi' must define a valid parameter range")
  
  if (!anyNA(ci) && ci[[1L]] > ci[[2L]])
    stop("'ci' must contain the lower and upper bound in that order")
  
  lci <- min(max(ci[[1L]], lo), hi)
  uci <- min(max(ci[[2L]], lo), hi)
  
  switch(
    sides,
    "two.sided" = NULL,
    "left"      = uci <- hi,
    "right"     = lci <- lo,
    stop("'sides' must be one of \"two.sided\", \"left\", \"right\"")
  )
  
  c(lci = unname(lci), uci = unname(uci))
}
