
#' Normalize Input to a Contingency or Agreement Table
#'
#' Converts diverse input formats into a numeric contingency matrix suitable
#' for agreement measures (e.g. Cohen's Kappa) or association measures
#' (e.g. Cramer's V). Accepted input formats include tables, matrices,
#' data frames, lists, and raw rating vectors.
#'
#' @details
#' The function handles the following input formats:
#' \describe{
#'   \item{\code{table}}{A pre-computed 2D contingency table. Validated via
#'     \code{\link{isConfusionTable}}. For \code{mode = "agreement"}, the
#'     table must be square with identical row and column names.}
#'   \item{\code{matrix}}{Either a pre-computed contingency matrix (square,
#'     passing \code{\link{isConfusionTable}} validation) or a two-rater
#'     matrix with exactly 2 columns. A 2-column non-square matrix is always
#'     treated as a rater matrix; each column is one rater's ratings.}
#'   \item{two vectors}{If both \code{x} and \code{y} are supplied, they are
#'     tabulated via \code{\link{table}} after coercing to factors.}
#'   \item{\code{list} or \code{data.frame} with 2 elements}{Each element is
#'     treated as one rater's ratings.}
#' }
#'
#' For \code{mode = "agreement"}, levels must be shared between both raters
#' and the resulting table is square. For \code{mode = "association"}, row and
#' column levels may differ and the table may be rectangular.
#' 
#' 
#' @param x Input object. Accepted formats: a \code{\link{table}} representing
#'   a pre-computed contingency table; a square numeric \code{matrix}
#'   representing a pre-computed contingency table; a numeric \code{matrix}
#'   with exactly 2 columns (one per rater); a \code{list} or
#'   \code{data.frame} with exactly 2 elements; or a vector if \code{y} is
#'   also supplied.
#' @param y Optional second rating vector. If supplied, \code{x} and \code{y}
#'   are tabulated together.
#' @param levels Optional category levels.\cr
#'   For \code{mode = "agreement"}: an atomic vector of common levels shared
#'   by both raters.\cr
#'   For \code{mode = "association"}: a list of length 2,
#'   \code{list(x_levels, y_levels)}.
#' @param useNA Passed to \code{\link{table}}. Controls whether \code{NA}
#'   values appear as a level. One of \code{"no"} (default), \code{"ifany"},
#'   or \code{"always"}.
#' @param mode Character string, either \code{"agreement"} (default) or
#'   \code{"association"}. Agreement mode enforces a square table with
#'   identical row and column names. Association mode allows rectangular
#'   tables with independent row and column levels.
#'
#' @return A numeric matrix representing the contingency table, with dimnames
#'   set according to \code{levels} (if supplied) or derived from the input.
#'
#' @seealso \code{\link{isConfusionTable}()}, \code{\link{raterFrame}()}, [pairApply()]
#'
#' @examples
#' A <- c("pos","neg","pos","inc")
#' B <- c("pos","pos","neg","inc")
#' normalizeToConfusion(A, B)
#'
#' tab <- table(A, B)
#' normalizeToConfusion(tab)
#'
#' set.seed(1)
#' C <- sample(c("pos","neg","inc"), length(A), TRUE)
#' df <- data.frame(R1=A, R2=B, R3=C)
#' normalizeToConfusion(df[, 1:2])      # R1 vs R2
#' normalizeToConfusion(df[, c(1,3)])   # R1 vs R3
#'
#' # list of rating vectors:
#' normalizeToConfusion(list(A, B))
#'
#' # use NAs
#' B[2] <- NA
#' normalizeToConfusion(A, B, useNA = "always")
#'
#' anxiety <- data.frame(
#'   rater1 = c(3,3,3,4,5,5,2),
#'   rater2 = c(3,6,4,6,2,4,2),
#'   rater3 = c(2,1,4,4,3,2,1)
#' )
#'
#' x <- anxiety[, 1]
#' y <- anxiety[, 2]
#'
#' # two vectors:
#' normalizeToConfusion(x, y)
#'
#' # matrix / data.frame with 2 columns (subjects x raters):
#' normalizeToConfusion(cbind(x, y))
#' normalizeToConfusion(data.frame(x, y))
#'
#' # list with 2 elements:
#' normalizeToConfusion(list(x, y))
#'
#' # pre-built table:
#' ratingscale <- sort(unique(c(x, y)))
#' normalizeToConfusion(table(factor(x, levels = ratingscale),
#'                            factor(y, levels = ratingscale)))
#'
#' d.anxiety <- data.frame(
#'   rater  = c("rater1", "rater1", "rater1", "rater1", "rater1", "rater1", "rater1",
#'              "rater2", "rater2", "rater2", "rater2", "rater2", "rater2", "rater2",
#'              "rater3", "rater3", "rater3", "rater3", "rater3", "rater3", "rater3"),
#'   rating = c(3, 3, 3, 4, 5, 5, 2,
#'              3, 6, 4, 6, 2, 4, 2,
#'              2, 1, 4, 4, 3, 2, 1),
#'   subj   = c(1, 2, 3, 4, 5, 6, 7,
#'              1, 2, 3, 4, 5, 6, 7,
#'              1, 2, 3, 4, 5, 6, 7)
#' )
#'
#' # via raterFrame (wide format, subjects x raters):
#' normalizeToConfusion(
#'   raterFrame(rating ~ subj | rater, data = d.anxiety,
#'              subset = rater %in% c("rater1", "rater2"), drop.subj = TRUE)
#' )
#'
#' @family confusion
#' @concept classification-metrics
#' @concept data-manipulation
#' @concept table-manipulation



#' @export
normalizeToConfusion <- function(
    x,
    y      = NULL,
    levels = NULL,
    useNA  = "no",
    mode   = c("agreement", "association")
) {
  
  mode <- match.arg(mode)
  
  #------------------------------------------------
  # Helper: build table from two vectors
  #------------------------------------------------
  two_vec_to_tab <- function(a, b, levels, useNA, mode) {
    if (mode == "agreement") {
      if (is.null(levels))
        levels <- sort(unique(c(a, b)))
      a <- factor(a, levels = levels)
      b <- factor(b, levels = levels)
    } else {
      if (is.null(levels)) {
        a <- factor(a)
        b <- factor(b)
      } else {
        if (!is.list(levels) || length(levels) != 2L)
          stop("For mode='association', 'levels' must be list(x_levels, y_levels).")
        a <- factor(a, levels = levels[[1L]])
        b <- factor(b, levels = levels[[2L]])
      }
    }
    as.matrix(table(a, b, useNA = useNA))
  }
  
  #------------------------------------------------
  # Helper: apply levels to an existing table
  #------------------------------------------------
  apply_levels <- function(tab, levels, mode) {
    if (is.null(levels)) return(tab)
    if (mode == "agreement") {
      if (!is.atomic(levels) || length(levels) != nrow(tab))
        stop("'levels' must be an atomic vector matching the table dimensions.")
      dimnames(tab) <- list(levels, levels)
    } else {
      if (!is.list(levels) || length(levels) != 2L)
        stop("For mode='association', 'levels' must be list(x_levels, y_levels).")
      dimnames(tab) <- list(levels[[1L]], levels[[2L]])
    }
    tab
  }
  
  #------------------------------------------------
  # 1) table input only
  #------------------------------------------------
  if (inherits(x, "table") && length(dim(x)) == 2L) {
    
    if (!isConfusionTable(x,
                          requireDimnames   = FALSE,
                          requireSameLevels = FALSE,
                          requireSquare     = (mode == "agreement")))
      stop("'x' does not look like a valid contingency table ",
           "(negative values, non-finite entries, or wrong shape).")
    
    tab <- as.matrix(x)
    
    if (mode == "agreement") {
      dn <- dimnames(tab)
      if (is.null(dn[[1L]]) || is.null(dn[[2L]])) {
        if (is.null(levels))
          warning("Table has no dimnames; consider supplying 'levels=' for stable ordering.")
      } else if (!identical(dn[[1L]], dn[[2L]])) {
        stop("For agreement measures, row and column names must match.")
      }
    }
    
    return(apply_levels(tab, levels, mode))
  }
  
  #------------------------------------------------
  # 2) matrix: confusion table OR rater matrix
  #------------------------------------------------
  if (is.matrix(x) && length(dim(x)) == 2L) {
    
    is_conf        <- isConfusionTable(x,
                                       requireDimnames   = FALSE,
                                       requireSameLevels = FALSE,
                                       requireSquare     = (mode == "agreement"))
    is_rater_matrix <- ncol(x) == 2L && nrow(x) != ncol(x)
    
    if (is_conf && !is_rater_matrix) {
      tab <- as.matrix(x)
      
      if (mode == "agreement") {
        dn <- dimnames(tab)
        if (is.null(dn[[1L]]) || is.null(dn[[2L]])) {
          if (is.null(levels))
            warning("Matrix has no dimnames; consider supplying 'levels=' for stable ordering.")
        } else if (!identical(dn[[1L]], dn[[2L]])) {
          stop("For agreement measures, row and column names must match.")
        }
      }
      
      return(apply_levels(tab, levels, mode))
    }
    
    # not a confusion table or looks like a rater matrix: fall through
  }
  
  #------------------------------------------------
  # 3) Two vectors explicitly provided
  #------------------------------------------------
  if (!is.null(y))
    return(two_vec_to_tab(x, y, levels, useNA, mode))
  
  #------------------------------------------------
  # 4) matrix with exactly 2 columns (rater matrix)
  #------------------------------------------------
  if (is.matrix(x)) {
    if (ncol(x) != 2L)
      stop("Matrix input must have exactly 2 columns.")
    return(two_vec_to_tab(x[, 1L], x[, 2L], levels, useNA, mode))
  }
  
  #------------------------------------------------
  # 5) list or data.frame with exactly 2 elements
  #------------------------------------------------
  if (is.list(x)) {
    if (length(x) != 2L)
      stop("List/data.frame input must contain exactly 2 elements.")
    return(two_vec_to_tab(x[[1L]], x[[2L]], levels, useNA, mode))
  }
  
  stop("Unsupported input type or missing second variable.")
}

