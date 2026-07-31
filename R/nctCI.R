
# == internal: noncentral t confidence limits ==================================
#
# THE single definition in the package. Until now there were two, in
# coefVar.R and in glassDelta.R:
#
#   coefVar.R    .nctCI(tval, df, conf.level = 0.95, tol = 1e-8)
#   glassDelta.R .nctCI(tObs, df, conf.level, sides = "two.sided")
#
# Same name, same namespace, different signatures and different return
# values (named vs unnamed). R keeps whichever file is collated last -
# glassDelta.R, alphabetically - so coefVar's calls were silently served
# by glassDelta's version, and any caller using ci[["lci"]] on the result
# hit "subscript out of bounds". Merged here: glassDelta's root finder
# (more robust bracketing, explicit sides, accuracy warning) with named
# elements, which is what the rest of the package expects.
#
# Confidence limits for the noncentrality parameter of the t-distribution,
# obtained by root-finding on pt(): the limits are the ncp values for which
# the observed statistic tObs sits at the required tail probability.
# pt(tObs, df, ncp) is strictly decreasing in ncp, hence a unique root.
# One-sided intervals put the full alpha on one tail: "left" yields a
# lower limit only, "right" an upper limit only; the other limit is +/-Inf.
# Replaces the two-method optimize()/nlm() search from MBESS
# (verified to agree to < 1e-7 across ncp in [-8, 12], df in [2, 120]).

# warnLimit: R's pt() loses accuracy for |ncp| > 37.62, which is worth
# saying for an effect size, where such a value means something has gone
# wrong. coefVar() inverts sqrt(n)/CV, so a large ncp is the normal case
# there - a CV of 0.1 at n = 100 already gives 100 - and the warning
# fired on every ordinary call. Hence opt-out rather than unconditional.
#' @noRd
.nctCI <- function(tval, df, conf.level = 0.95, sides = "two.sided",
                   tol = 1e-9, warnLimit = TRUE) {

  alpha <- 1 - conf.level

  # sides names the side on which the finite bound lies: "left" gives
  # [lci, Inf), "right" gives (-Inf, uci].
  lim <- switch(sides,
    "two.sided" = c(.nctRoot(tval, df, prob = 1 - alpha / 2, tol = tol),
                    .nctRoot(tval, df, prob = alpha / 2,     tol = tol)),
    "left"      = c(.nctRoot(tval, df, prob = 1 - alpha, tol = tol), Inf),
    "right"     = c(-Inf, .nctRoot(tval, df, prob = alpha, tol = tol)),
    stop("'sides' must be one of \"two.sided\", \"left\", \"right\""))

  if (warnLimit && any(abs(lim[is.finite(lim)]) > 37.62))
    warning("a confidence limit for the noncentrality parameter exceeds ",
            "37.62 in magnitude, R's limit for accurate noncentral t ",
            "probabilities; confidence limits may be inaccurate")

  c(lci = lim[1L], uci = lim[2L])
}


#' @noRd
.nctRoot <- function(tval, df, prob, tol = 1e-9) {

  f <- function(d) suppressWarnings(pt(tval, df = df, ncp = d)) - prob

  uniroot(f,
          interval  = c(min(-150, 5 * tval - 5), max(150, 5 * tval + 5)),
          extendInt = "downX",
          tol       = tol)$root
}
