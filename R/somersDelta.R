
#' Somers' D (Somers' Delta)
#'
#' @description
#' Computes Somers' D, an asymmetric measure of association for ordinal variables.
#' The function provides interfaces for both contingency tables and paired vectors.
#'
#' @details
#' Somers' D is a directional measure of association related to Kendall's
#' \eqn{\tau_b} and Goodman–Kruskal's Gamma. It differs from Gamma in that
#' it corrects only for ties on the dependent variable.
#'
#' For two variables \eqn{X} and \eqn{Y}, Somers' D is defined as
#'
#' \deqn{D(Y|X) = \frac{C - D}{C + D + T_Y}}
#'
#' where \eqn{C} and \eqn{D} denote the number of concordant and discordant pairs,
#' and \eqn{T_Y} the number of pairs tied on the dependent variable.
#'
#' Somers' D takes values in \eqn{[-1, 1]}. Values close to \eqn{1} indicate
#' strong positive association, values close to \eqn{-1} strong negative association.
#'
#' \strong{Direction:}
#' \itemize{
#'   \item For vectors: \code{somersDelta(x, y)} estimates \eqn{D(Y|X)}.
#'   \item Reversing the order, \code{somersDelta(y, x)}, estimates \eqn{D(X|Y)}.
#'   \item For tables: \code{direction="row"} computes \eqn{D(Y|X)},
#'   \code{direction="column"} computes \eqn{D(X|Y)}.
#' }
#'
#' Somers' D is appropriate only when both variables are ordinal.
#'
#' @param x a numeric vector or a contingency table (matrix or table)
#' @param y optional numeric vector. If supplied, must have the same length as \code{x}.
#' @param conf.level confidence level for confidence intervals. If \code{NA},
#'   no confidence interval is returned.
#' @param direction direction for contingency tables:
#'   \code{"row"} (default) computes \eqn{D(Y|X)},
#'   while \code{"column"} computes \eqn{D(X|Y)}
#'
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Somers' D}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' @seealso
#' \code{\link[Hmisc]{somers2}} (restricted to binary response),
#'
#' @references
#' Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley & Sons, pp. 57–59.
#'
#' Brown, M. B., & Benedetti, J. K. (1977).
#' Sampling behavior of tests for correlation in two-way contingency tables.
#' \emph{Journal of the American Statistical Association}, 72, 309–315.
#'
#' Goodman, L. A., & Kruskal, W. H. (1954).
#' Measures of association for cross classifications.
#' \emph{Journal of the American Statistical Association}, 49, 732–764.
#'
#' Somers, R. H. (1962).
#' A new asymmetric measure of association for ordinal variables.
#' \emph{American Sociological Review}, 27, 799–811.
#'
#' Goodman, L. A., & Kruskal, W. H. (1963).
#' Measures of association for cross classifications III.
#' \emph{Journal of the American Statistical Association}, 58, 310–364.
#'
#' @examples
#'
#' # Example from SAS documentation (PROC FREQ)
#' # https://support.sas.com/documentation/
#' #
#' # Reported values:
#' # Somers' D C|R = 0.4427 (95% CI: 0.2786, 0.6068)
#' # Somers' D R|C = 0.2569 (95% CI: 0.1592, 0.3547)
#'
#' tab <- as.table(rbind(
#'   c(26,26,23,18,9),
#'   c(6,7,9,14,23)
#' ))
#'
#' # D(Y|X)  (row direction)
#' somersDelta(tab, direction="row", conf.level=0.95)
#'
#' # D(X|Y)  (column direction)
#' somersDelta(tab, direction="column", conf.level=0.95)
#'
#' # Vector interface
#' somersDelta(mtcars$wt, mtcars$mpg)
#'
#'
#' @family assoc.ordinal  
#' @concept association-measure  
#' @concept ordinal  
#' @concept asymmetric-association
#'
#'
#' @export
somersDelta <- function(x, y = NULL,
                        conf.level = NA,
                        direction = c("row","column")){
  
  direction <- match.arg(direction)
  
  if(is.null(y)){
    
    # ============================
    # TABLE MODE
    # ============================
    # .assocs() handles 'direction' itself in the table path (it picks
    # colSums vs rowSums for the denominator), so pass it through rather
    # than transposing here.
    res <- .assocs(
      x = x,
      which = "somers",
      conf.level = conf.level,
      direction = direction
    )
    
  } else {
    
    # ============================
    # XY MODE
    # ============================
    # assoc_cpp() always treats its SECOND argument as the dependent
    # variable, so 'direction' has no effect on the vector path of
    # .assocs() - swapping the vectors is the only way to flip it.
    if(direction == "column"){
      tmp <- x
      x <- y
      y <- tmp
    }
    
    res <- .assocs(
      x = x,
      y = y,
      which = "somers",
      conf.level = conf.level
    )
  }
  
  if(is.na(conf.level))
    unname(res[[1]])
  else
    setNamesX(unname(res[[1]]), c("est", "lci", "uci"))
  
}
