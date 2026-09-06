
# Assembles the five counts from what the compiled helpers return.
#
# Two things this fixes, both of which were live:
#
#   condis_pairs_xy_cpp() used to return only four names, while the R side
#   asked for five - the fifth element came out as NA under the name NA,
#   and sum() over the result was NA for every vector-mode call. The count
#   is now returned by the C++ (it was computed there all along, as the
#   correction subtracted from both marginal tie counts, and then dropped).
#
#   condis_pairs_tab_cpp() returns a List, so subsetting it with a
#   character vector gave a LIST back - table mode and vector mode returned
#   different types for the same question. vapply() settles both on a named
#   numeric vector.
#' @noRd
.completePairCounts <- function(z, n) {

  need <- c("C", "D", "Ties_X", "Ties_Y", "Ties_XY")

  if (!all(need %in% names(z)))
    stop(gettextf(
      "the compiled helper did not return %s - the package needs to be rebuilt.",
      paste(sQuote(setdiff(need, names(z)), FALSE), collapse = ", ")),
      domain = NA)

  res <- vapply(need, function(k) as.numeric(z[[k]]), numeric(1L))

  # The partition is the defining property of these five numbers. If it
  # ever fails, the counts are wrong, and saying so beats returning them -
  # that is exactly what went unnoticed while the fifth one was missing.
  if (abs(sum(res) - choose(n, 2)) > 0.5)
    stop(gettextf(
      "pair counts do not add up: %.0f counted, %.0f pairs in %.0f observations.",
      sum(res), choose(n, 2), n), domain = NA)

  res
}


#' Concordant and Discordant Pairs
#'
#' @description
#' Counts, for all \eqn{n(n-1)/2} pairs of observations, how many are
#' concordant, how many discordant, and how the remaining ties are
#' distributed. This is the quantity every rank-based association measure for
#' ordinal data is built on.
#'
#' @details
#' Two observations \eqn{(x_i, y_i)} and \eqn{(x_j, y_j)} are
#' **concordant** if they are ordered the same way in both variables -
#' one is larger in \eqn{x} *and* larger in \eqn{y} - and
#' **discordant** if the orderings disagree. If either variable ties the
#' pair, it is neither, and is counted among the ties instead.
#'
#' The five counts partition the pairs exhaustively and without overlap:
#'
#' \deqn{C + D + T_X + T_Y + T_{XY} = \frac{n(n-1)}{2}}
#'
#' `Ties_X` counts pairs tied in \eqn{x} but *not* in \eqn{y},
#' `Ties_Y` the reverse, and `Ties_XY` the pairs tied in both. The
#' counts are therefore *exclusive*; the inclusive marginal counts that
#' the classic tau-b formula uses are `Ties_X + Ties_XY` and
#' `Ties_Y + Ties_XY`. That separation is what the ordinal measures
#' need: Goodman-Kruskal's gamma
#' ignores all ties, Kendall's tau-b corrects for `Ties_X` and
#' `Ties_Y` separately, Somers' \eqn{D} for one of them only, and the
#' \eqn{c} statistic scores half a point for a tie in \eqn{y}. All of them
#' are therefore a short formula on top of this one function - see
#' [ordAssocs()].
#'
#' \subsection{Two input modes}{
#' **Vector mode** (`x` and `y` given) works on the raw
#' observations and keeps the full resolution of the data. Missing values are
#' removed pairwise.
#'
#' **Table mode** (`x` a matrix or table) works on a cross
#' tabulation. Use it when the data are already tabulated, or when the
#' variables have few distinct values: the count then depends on the table's
#' dimensions rather than on the number of observations, so a million
#' observations in a 4x5 table cost the same as a hundred.
#'
#' Both modes return the same five numbers for the same data.
#' }
#'
#' \subsection{Why this is fast}{
#' Counted naively, the definition is a double loop over all pairs and costs
#' \eqn{O(n^2)} - a hundred thousand observations are five billion
#' comparisons, which is where a straightforward implementation stops being
#' usable.
#'
#' Vector mode sorts the observations by \eqn{x} and processes equal
#' \eqn{x} values in blocks. A one-dimensional Fenwick tree (binary indexed
#' tree) over the compressed ranks of \eqn{y} counts smaller and larger
#' preceding values in \eqn{O(\log n)} time per observation. The resulting
#' complexity is \eqn{O(n \log n)} time and \eqn{O(n)} memory. The difference
#' is not a constant factor: it is what makes six-figure sample sizes a matter
#' of a moment instead of a coffee break.
#'
#' Table mode uses the cumulative-sum identity over the table and is
#' \eqn{O(r c)} in the table's dimensions, independent of \eqn{n}.
#'
#' Both are implemented in C++ and return exact integer counts - no
#' approximation, no sampling.
#' }
#'
#' @param x a numeric vector, an ordered factor, or a contingency table
#' @param y optional second numeric vector or ordered factor. If provided,
#'   vector mode is used.
#'
#'   An [ordered()] factor is converted to its level codes, which is
#'   exactly the ordinal information the counts rest on. An *unordered*
#'   factor is refused: its levels have no order, and converting it anyway
#'   would silently impose the alphabetical one and report concordances that
#'   are an artefact of the level names.
#'
#' @return a named numeric vector with elements:
#' \describe{
#'   \item{`C`}{number of concordant pairs}
#'   \item{`D`}{number of discordant pairs}
#'   \item{`Ties_X`}{pairs tied in `x` only}
#'   \item{`Ties_Y`}{pairs tied in `y` only}
#'   \item{`Ties_XY`}{pairs tied in both}
#' }
#' If fewer than two observations remain, all five elements are `NA`.
#'
#' @seealso [ordAssocs], [gkGamma], [kendallTauA], [kendallTauB],
#'   [stuartTauC], [somersDelta]
#'
#' @references
#' Goodman, L. A., Kruskal, W. H. (1954) Measures of association for
#' cross classifications. *Journal of the American Statistical
#' Association*, **49**, 732-764.
#'
#' Agresti, A. (2010) *Analysis of Ordinal Categorical Data* (2nd ed.).
#' Wiley.
#'
#' @examples
#' # vector input
#' x <- c(1, 2, 3, 1, 2)
#' y <- c(2, 1, 3, 2, 1)
#' conDisPairs(x, y)
#'
#' # the five counts partition all n(n-1)/2 pairs
#' sum(conDisPairs(x, y)) == choose(length(x), 2)
#'
#' # table input gives the same answer
#' tab <- table(x, y)
#' conDisPairs(tab)
#'
#' # the ordinal measures are short formulas on top of it
#' p <- conDisPairs(x, y)
#' unname((p["C"] - p["D"]) / (p["C"] + p["D"]))   # Goodman-Kruskal's gamma
#' gkGamma(x, y)
#'
#' \donttest{
#' # vector mode stays usable where a pairwise double loop would not:
#' # 200'000 observations are 2 * 10^10 pairs
#' set.seed(1)
#' n <- 2e5
#' system.time(conDisPairs(rnorm(n), rnorm(n)))
#' }
#'
#' @family assoc.ordinal
#' @concept association-measure
#' @concept ordinal
#' @concept concordance
#'
#' @export
conDisPairs <- function(x, y = NULL){
  
  # ============================
  # VECTOR MODE
  # ============================
  if(!is.null(y)){
    
    # ---- checks ----
    if(length(x) != length(y)){
      stop("x and y must have the same length.")
    }
    
    # An ordered factor is the natural type for ordinal data, and its
    # level codes carry precisely the ordering the pair counts use. An
    # UNORDERED factor is refused rather than coerced: as.integer() would
    # impose the alphabetical order of the levels and produce concordance
    # counts that are an artefact of the level names.
    .asOrdinal <- function(z, name) {
      
      if(is.ordered(z))
        return(as.integer(z))
      
      if(is.factor(z))
        stop(gettextf(
          "'%s' is an unordered factor; concordance needs an order. Use ordered() to supply one.",
          name), domain = NA)
      
      if(!is.numeric(z))
        stop(gettextf(
          "'%s' must be a numeric vector or an ordered factor.",
          name), domain = NA)
      
      z
    }
    
    x <- .asOrdinal(x, "x")
    y <- .asOrdinal(y, "y")
    
    # ---- remove NA pairwise ----
    ok <- !(is.na(x) | is.na(y))
    x <- x[ok]
    y <- y[ok]
    
    if(length(x) < 2){
      return(setNamesX(rep(NA_real_, 5),
                      c("C","D","Ties_X","Ties_Y","Ties_XY")))
    }
    
    # ---- call C++ ----
    z <- condis_pairs_xy_cpp(x, y)
    
    res <- .completePairCounts(z, length(x))
    
  } else {
    
    # ============================
    # TABLE MODE
    # ============================
    
    if(!(is.matrix(x) || is.table(x))){
      stop("If 'y' is NULL, 'x' must be a contingency table or matrix.")
    }
    
    # is.table() is TRUE for any number of dimensions, so a 2x2x2 table
    # used to reach the C++ code, which reads it as if it were flat
    if(length(dim(x)) != 2L)
      stop(gettextf(
        "'x' must be a two-dimensional table, not one with %d dimensions.",
        length(dim(x))), domain = NA)
    
    if(!is.numeric(x))
      stop("Table counts must be numeric.")
    
    # Checked before the comparisons below. With na.rm = TRUE the negative
    # test passed an NA table through, and sum(x) was then NA, so the
    # count test aborted with "missing value where TRUE/FALSE needed" -
    # a message about the condition rather than about the table.
    if(anyNA(x))
      stop("Table counts must not be missing.")
    
    if(any(x < 0)){
      stop("Table counts must be non-negative.")
    }
    
    if(sum(x) < 2){
      return(setNamesX(rep(NA_real_, 5),
                      c("C","D","Ties_X","Ties_Y","Ties_XY")))
    }
    
    # ---- call table version ----
    z <- condis_pairs_tab_cpp(x)
    
    res <- .completePairCounts(z, sum(x))
  }
  
  return(res)
}
