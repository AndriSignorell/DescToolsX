
#' Association Measures for Two Variables (Fast C++ Backend)
#'
#' @description
#' Computes a set of rank-based association measures between two variables
#' using a fast C++ implementation. Supported measures include Goodman–Kruskal
#' Gamma, Kendall's tau-a, tau-b, tau-c, Somers' D, and the C-statistic.
#'
#' @details
#' The function is designed for efficient computation on large datasets
#' (O(n log n)) and supports numeric or ordinal data. Internally, concordant
#' and discordant pairs are counted using an optimized algorithm based on
#' a 2D Fenwick tree.
#'
#' Confidence intervals are computed using asymptotic normal approximations:
#' \itemize{
#'   \item Somers' D: influence function (U-statistic based)
#'   \item Gamma: quotient-based asymptotics
#'   \item Kendall tau-b: exact variance formula (Knight, 1966)
#'   \item tau-a and tau-c: asymptotic variance
#' }
#'
#' Missing values are removed pairwise before computation.
#'
#' @param x A numeric or ordinal vector.
#' @param y A numeric or ordinal vector of the same length as \code{x}.
#' @param conf.level Confidence level for the intervals (default: 0.95).
#'
#' @return
#' A named list with elements:
#' \itemize{
#'   \item \code{gamma}  Goodman–Kruskal Gamma
#'   \item \code{tau_a}  Kendall's tau-a
#'   \item \code{tau_b}  Kendall's tau-b (tie-adjusted)
#'   \item \code{tau_c}  Stuart's tau-c
#'   \item \code{somers} Somers' D
#'   \item \code{ctat}   C-statistic (=(Somers' D + 1)/2)
#' }
#'
#' Each element is a named numeric vector with:
#' \itemize{
#'   \item \code{est} Estimate
#'   \item \code{lci} Lower confidence interval
#'   \item \code{uci} Upper confidence interval
#' }
#'
#' @references
#' Kendall, M. G. (1938). A new measure of rank correlation.
#' \emph{Biometrika}, 30(1/2), 81–93. \doi{10.1093/biomet/30.1-2.81}
#'
#' Knight, W. R. (1966). A computer method for calculating Kendall's tau
#' with ungrouped data. \emph{Journal of the American Statistical Association},
#' 61(314), 436–439. \doi{10.1080/01621459.1966.10480879}
#'
#' @examples
#' set.seed(1)
#' x <- rnorm(100)
#' y <- x + rnorm(100, sd = 0.5)
#'
#' assocsXY(x, y)
#'
#' # ordinal example
#' x <- sample(1:5, 200, replace = TRUE)
#' y <- sample(1:5, 200, replace = TRUE)
#'
#' assocsXY(x, y)
#'



#' @export
assocsXY <- function(x, y, conf.level=0.95){
  
  z <- assoc_revo_cpp(x, y, conf.level)
  
  list(
    gamma  = c(est=z["gamma"], lci=z["gamma_l"], uci=z["gamma_u"]),
    tau_a  = c(est=z["tau_a"], lci=z["tau_a_l"], uci=z["tau_a_u"]),
    tau_b  = c(est=z["tau_b"], lci=z["tau_b_l"], uci=z["tau_b_u"]),
    tau_c  = c(est=z["tau_c"], lci=z["tau_c_l"], uci=z["tau_c_u"]),
    somers = c(est=z["somers"], lci=z["somers_l"], uci=z["somers_u"]),
    ctat   = c(est=z["cstat"], lci=z["cstat_l"], uci=z["cstat_u"])
  )
  
}
