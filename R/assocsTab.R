
#' Association measures for a contingency table (internal)
#'
#' Report layer for \code{desc.table()}: collects the association measures
#' phi, contingency coefficient, Cramer's V, Goodman Kruskal's Gamma,
#' Kendall's Tau-b, Stuart's Tau-c, Somers' Delta, Pearson and Spearman
#' correlation, Guttman's Lambda, Theil's Uncertainty Coefficient and the
#' mutual information into a single matrix.
#'
#' Performs no computation of its own: the concordance-based ordinal
#' measures come from \code{ordAssocs()}, the remaining ones from their
#' respective exported functions. Its job is to collect them, stage the
#' level of detail via \code{verbose}, and format the result.
#'
#' Not exported - the measures are individually available through
#' \code{gkGamma()}, \code{kendallTauA()}, \code{kendallTauB()},
#' \code{stuartTauC()}, \code{somersDelta()} and \code{cStat()}. This
#' function exists only to assemble the block \code{desc.table()} prints,
#' and its shape (row selection via \code{verbose}, column names varying
#' with it) is tied to that output rather than to a stable public contract.
#'
#' @param x a two-dimensional contingency table or matrix
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   no confidence interval will be calculated. Default is 0.95.
#' @param verbose integer from 1 to 3 controlling how many measures are included.
#'   Levels 1 and 2 return the contingency coefficient, Cramer's V and
#'   Kendall's Tau-b; level 3 adds the remaining measures.
#'
#' @return numeric matrix of class \code{AssocsTab}, one row per measure and
#'   the columns \code{est}, \code{lci}, and \code{uci}
#'
#' @noRd
.assocsTab <- function(x, conf.level = 0.95, verbose = 2){

  verbose <- .checkVerbose(verbose)

  # all association measures combined for table description
  ords <- ordAssocs(x, conf.level = conf.level)

  # ordAssocs() returns bare scalars when conf.level is NA and c(est, lci,
  # uci) otherwise; rbind() would recycle a scalar across all three
  # columns, so pad explicitly. Keeps every row 3 wide and the column
  # names fixed, independent of conf.level and verbose.
  .row <- function(z) if(length(z) == 3L) unname(z) else c(unname(z[1L]), NA, NA)

  # conf.level = NA is a legal value for every measure below and simply
  # suppresses the interval, so the two former branches (one passing
  # conf.level, one omitting it) were the same call twice.
  res <- rbind("Contingency Coeff." = .row(contCoef(x, conf.level = conf.level)),
               "Cramer V"           = .row(cramerV(x, conf.level = conf.level)),
               "Kendall Tau-b"      = .row(ords$tauB))

  if(verbose == 3) {
    res <- rbind(res
                 , "Goodman Kruskal Gamma" = .row(ords$gamma)
                 , "Stuart Tau-c"          = .row(ords$tauC)
                 , "Somers D R|C"          = .row(ords$somers)
                 , "Pearson Correlation"   = .row(pearsonCor(x, conf.level = conf.level))
                 , "Spearman Correlation"  = .row(spearmanCor(x, conf.level = conf.level))
                 , "Lambda R|C"            = .row(lambda(x, direction = "row", conf.level = conf.level))
                 , "Lambda sym"            = .row(lambda(x, direction = "sym", conf.level = conf.level))
                 , "Uncertainty Coeff. R|C"  = .row(uncertCoef(x, direction = "row", conf.level = conf.level))
                 , "Uncertainty Coeff. sym"  = .row(uncertCoef(x, direction = "sym", conf.level = conf.level))
                 , "Mutual Information"    = .row(mutInf(x))
    )
  }

  colnames(res) <- c("est", "lci", "uci")

  class(res) <- c("AssocsTab", "matrix")
  return(res)
}

#' Print method for the internal AssocsTab class
#'
#' Registered (not user-facing): the class is produced only by
#' \code{.assocsTab()}, but S3 dispatch requires the method to be in the
#' namespace's method table, so it keeps \code{@exportS3Method}.
#'
#' @noRd
#' @exportS3Method print AssocsTab
print.AssocsTab <- function(x, digits = 4, ...){

  out <- fm(unclass(x), digits = digits)
  dimnames(out) <- dimnames(x)
  print(data.frame(out), quote = FALSE, ...)

  invisible(x)
}


#' Concordance-based ordinal association measures
#'
#' Computes Goodman-Kruskal \eqn{\gamma}, Kendall's \eqn{\tau_a},
#' \eqn{\tau_b}, \eqn{\tau_c}, Somers' \eqn{D} and the c-statistic for a
#' pair of ordinal vectors or a contingency table, optionally with
#' confidence intervals.
#'
#' This is the shared calculation core: it counts the concordant and
#' discordant pairs once and derives every measure from them, so requesting
#' several measures at once is cheaper than calling the single-measure
#' functions (\code{\link{gkGamma}}, \code{\link{kendallTauA}},
#' \code{\link{kendallTauB}}, \code{\link{stuartTauC}},
#' \code{\link{somersDelta}}) separately - each of those wraps this
#' function for one measure. Use \code{ordAssocs()} directly when a model or
#' report needs a bundle of them.
#'
#' \code{\link{cStat}} is a related but separate estimator: it returns the
#' AUC of a score against a binary outcome with half-weighted ties and
#' bootstrap intervals, which differs from the \code{(somers + 1) / 2}
#' reported here.
#'
#' Two computational paths are used:
#'
#' \describe{
#'   \item{\code{y} supplied}{A compiled C++ routine (\code{assoc_cpp})
#'     processes the raw paired observations directly.  This is
#'     substantially faster than the table path for large vectors.}
#'   \item{\code{y = NULL}}{\code{x} is treated as a contingency table;
#'     concordant and discordant pairs are counted via
#'     \code{\link{conDisPairs}}.  Confidence intervals are obtained from
#'     \code{.tableAssocCI}.}
#' }
#'
#' Both paths return the same measures - the choice is one of input
#' format and speed, not of method.
#'
#' @param x either a numeric vector when \code{y} is supplied or a
#'   matrix or table representing a contingency table when \code{y} is
#'   \code{NULL}
#' @param y optional numeric vector of the same length as \code{x}
#' @param which character string selecting which measure to return. One of
#'   \code{"all"} (default), \code{"gamma"}, \code{"tauA"},
#'   \code{"tauB"}, \code{"tauC"}, \code{"somers"} or \code{"cstat"} -
#'   the same spellings as the names of the returned list.
#' @param conf.level numeric scalar in \eqn{(0, 1)}. If supplied,
#'   confidence intervals are appended to each measure. Default \code{NA}
#'   suppresses intervals.
#' @param direction character string controlling the direction for Somers'
#'   \eqn{D}: \code{"row"} (default) treats row totals as the dependent
#'   variable, \code{"column"} the column totals. Ignored by the symmetric
#'   measures. Only available in table mode; the vector path always
#'   reports \code{"row"} and rejects any other value rather than
#'   returning a differently defined estimate under the requested name.
#'
#' @return a named list with one element per requested measure
#'   (\code{gamma}, \code{tauA}, \code{tauB}, \code{tauC},
#'   \code{somers}, \code{cstat}). Each element is an unnamed numeric
#'   scalar containing only the point estimate, or, when
#'   \code{conf.level} is supplied, a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#'
#' @seealso \code{\link{gkGamma}}, \code{\link{kendallTauA}},
#'   \code{\link{kendallTauB}}, \code{\link{stuartTauC}},
#'   \code{\link{somersDelta}}, \code{\link{cStat}},
#'   \code{\link{conDisPairs}}
#'
#' @examples
#' # A bundle of measures from raw vectors, computed in one pass
#' ord <- ordAssocs(swiss$Fertility, swiss$Agriculture, conf.level = 0.95)
#' ord$somers
#' ord$cstat
#'
#' # A single measure
#' ordAssocs(swiss$Fertility, swiss$Agriculture, which = "gamma")
#'
#' # From a contingency table
#' tab <- table(cut(swiss$Fertility, 3), cut(swiss$Education, 3))
#' ordAssocs(tab)
#'
#' @family assoc.ordinal
#' @concept association-measure
#' @concept ordinal
#' @export
ordAssocs <- function(x, y = NULL,
                   which = c("all", "gamma", "tauA", "tauB", "tauC", "somers", "cstat"),
                   conf.level = NA,
                   direction = c("row", "column")) {

  direction <- match.arg(direction)
  # 'which' uses the same spelling as the names of the returned list, so
  # no mapping between user-facing and internal names is needed. This is
  # a deliberate exception to the kebab-case rule for match.arg() enums
  # (design_rules.md 3.1.1): the strings here are selectors *for* the
  # result elements, and two spellings for one measure invite drift.
  which     <- match.arg(which)

  if(length(conf.level) != 1L || !is.numeric(conf.level) && !is.logical(conf.level))
    stop("'conf.level' must be a single number in (0, 1) or NA")
  if(!is.na(conf.level) && (conf.level <= 0 || conf.level >= 1))
    stop("'conf.level' must lie in (0, 1)")

  # ============================
  # XY MODE (use C++)
  # ============================
  if(!is.null(y)){

    # assoc_cpp() has no direction switch - it always treats the first
    # argument as the dependent variable. Silently returning the "row"
    # result under a "column" request would be a wrong answer, so refuse.
    if(direction != "row")
      stop("direction = \"column\" is only supported in table mode; ",
           "swap 'x' and 'y', or pass a contingency table")

    cl <- if(is.na(conf.level)) NA_real_ else conf.level
    z  <- assoc_cpp(x, y, cl)

    resAll <- list(
      gamma  = if(is.na(conf.level)) z["gamma"]  else z[c("gamma",  "gamma_l",  "gamma_u")],
      tauA   = if(is.na(conf.level)) z["tau_a"]  else z[c("tau_a",  "tau_a_l",  "tau_a_u")],
      tauB   = if(is.na(conf.level)) z["tau_b"]  else z[c("tau_b",  "tau_b_l",  "tau_b_u")],
      tauC   = if(is.na(conf.level)) z["tau_c"]  else z[c("tau_c",  "tau_c_l",  "tau_c_u")],
      somers = if(is.na(conf.level)) z["somers"] else z[c("somers", "somers_l", "somers_u")],
      cstat  = if(is.na(conf.level)) z["cstat"]  else z[c("cstat",  "cstat_l",  "cstat_u")]
    )

    if(is.na(conf.level))
      resAll <- lapply(resAll, unname)
    else
      resAll <- lapply(resAll, setNamesX, names = c("est", "lci", "uci"))

  } else {

    # ============================
    # TABLE MODE
    # ============================
    tab <- as.table(x)

    if(is.na(conf.level)){

      # Point estimates only - count the pairs once, here.
      cd <- conDisPairs(tab)

      C  <- unname(cd[["C"]])
      D  <- unname(cd[["D"]])
      Tx <- unname(cd[["Ties_X"]])
      Ty <- unname(cd[["Ties_Y"]])

      n  <- sum(tab)
      n0 <- n * (n - 1) / 2
      S  <- C - D

      m      <- min(dim(tab))
      ni     <- if(direction == "row") colSums(tab) else rowSums(tab)
      denom  <- n0 - sum(ni * (ni - 1) / 2)
      somers <- S / denom

      resAll <- list(
        gamma  = S / (C + D),
        tauA   = S / n0,
        tauB   = S / sqrt((n0 - Tx) * (n0 - Ty)),
        tauC   = 2 * S * m / (n^2 * (m - 1)),
        somers = somers,
        cstat  = (somers + 1) / 2
      )

    } else {

      # .tableAssocCI() recomputes the point estimates alongside the
      # variances from the same pair counts, so calling conDisPairs()
      # here as well would count every pair twice.
      resAll <- .tableAssocCI(tab, conf.level, direction)[
        c("gamma", "tauA", "tauB", "tauC", "somers", "cstat")]
      resAll <- lapply(resAll, setNamesX, names = c("est", "lci", "uci"))
    }
  }

  # ============================
  # SELECT WHICH
  # ============================
  if(which != "all")
    resAll <- resAll[which]

  return(resAll)
}


# == internal helper ==========================================================

# Delta-method variance from an influence contribution psi that is already
# scaled by 1/n (as every psi in .tableAssocCI() is):
#
#   Var(theta.hat) = ( E psi^2 - (E psi)^2 ) / n
#                  = sum(n_ij psi_ij^2) - sum(n_ij psi_ij)^2 / n
#
# For gamma and Somers' D the centring term is algebraically zero
# (sum(n_ij psi_ij) == 0), which is why it could be omitted there without
# harm. For tau-a and tau-c it is NOT zero: sum(n_ij psi_ij) equals
# 2 * tau, so dropping it inflates the variance - most visibly at the
# boundary, where a perfectly concordant table produced a variance of
# 4/n instead of 0.
#' @noRd
.influenceVar <- function(tab, psi, n)
  max(sum(tab * psi^2) - sum(tab * psi)^2 / n, 0)


#' @noRd
.tableAssocCI <- function(tab, conf.level = 0.95, direction = "row"){

  cd <- condis_pairs_tab_cpp(tab)

  piC <- cd$pi.c
  piD <- cd$pi.d

  nr <- nrow(tab)
  nc <- ncol(tab)

  n  <- sum(tab)
  S  <- cd$C - cd$D
  n0 <- n * (n - 1) / 2

  rowSum <- rowSums(tab)
  colSum <- colSums(tab)

  # ============================
  # SOMERS
  # ============================
  # The cell term needs the marginal total of the *independent* variable
  # at cell (i, j), i.e. a matrix that is constant along the dependent
  # dimension. Recycling the bare margin vector against the piC/piD
  # matrices fills column-major, which is only correct when the margin
  # has nrow(tab) elements - so direction = "column" happened to work
  # while the default direction = "row" silently used a transposed term.
  if(direction == "row"){
    ni    <- colSum
    niMat <- matrix(ni, nrow = nr, ncol = nc, byrow = TRUE)
  } else {
    ni    <- rowSum
    niMat <- matrix(ni, nrow = nr, ncol = nc)
  }

  tDen   <- n0 - sum(ni * (ni - 1) / 2)
  psiS   <- (tDen * (piC - piD) - S * (n - niMat)) / tDen^2
  varS   <- .influenceVar(tab, psiS, n)
  somers <- S / tDen

  # ============================
  # GAMMA
  # ============================
  denomG <- cd$C + cd$D
  psiG   <- ((piC - piD) * denomG - S * (piC + piD)) / denomG^2
  varG   <- .influenceVar(tab, psiG, n)
  gamma  <- S / denomG

  # ============================
  # TAU-A
  # ============================
  psiA <- (piC - piD) / n0
  varA <- .influenceVar(tab, psiA, n)
  tauA <- S / n0

  # ============================
  # TAU-C
  # ============================
  m    <- min(dim(tab))
  k    <- 2 * m / (n^2 * (m - 1))
  psiC <- k * (piC - piD)
  varC <- .influenceVar(tab, psiC, n)
  tauC <- k * S

  # ============================
  # TAU-B
  # ============================
  ti <- rowSum
  uj <- colSum
  n1 <- sum(ti * (ti - 1) / 2)
  n2 <- sum(uj * (uj - 1) / 2)
  tauB <- S / sqrt((n0 - n1) * (n0 - n2))

  # 'prob' and 'tDen' above are spelled out rather than reusing the base
  # names pi and T, which they would mask inside this function.
  prob   <- tab / n
  pdiff  <- (piC - piD) / n
  Pdiff  <- 2 * S / n^2

  rowsum <- rowSums(prob)
  colsum <- colSums(prob)
  rowmat <- matrix(rep(rowsum, nc), ncol = nc)
  colmat <- matrix(rep(colsum, nr), nrow = nr, byrow = TRUE)

  delta1 <- sqrt(1 - sum(rowsum^2))
  delta2 <- sqrt(1 - sum(colsum^2))

  tauphi <- (2 * pdiff + Pdiff * colmat) * delta2 * delta1 +
    (Pdiff * rowmat * delta2) / delta1

  # tauphi is on the O(1) scale (built from probabilities), not the
  # O(1/n) scale of the psi above, hence the explicit centring and the
  # trailing division by n here rather than a call to .influenceVar().
  sigma2B <- ((sum(prob * tauphi^2) - sum(prob * tauphi)^2) /
                (delta1 * delta2)^4) / n

  if(sigma2B < .Machine$double.eps * 10) sigma2B <- 0

  # ============================
  # CI helper
  # ============================
  z <- qnorm(1 - (1 - conf.level) / 2)

  mk <- function(est, var, lo = -1, hi = 1){
    se <- sqrt(var)
    c(est = est,
      lci = max(est - z * se, lo),
      uci = min(est + z * se, hi))
  }

  list(
    somers = mk(somers, varS),
    gamma  = mk(gamma,  varG),
    tauA   = mk(tauA,   varA),
    tauB   = mk(tauB,   sigma2B),
    tauC   = mk(tauC,   varC),
    # the c-statistic is a probability: its interval is bounded by [0, 1],
    # not by [-1, 1] like the rank correlations above
    cstat  = mk((somers + 1) / 2, varS / 4, lo = 0, hi = 1)
  )
}
