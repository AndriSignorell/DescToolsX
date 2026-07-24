
#' Pearson's Contingency Coefficient
#' 
#' Calculates Pearson's contingency coefficient for a table \code{x}. If
#' \code{x} and \code{y} are given, the corresponding table is built
#' first (more details in \link{Association}).
#' The contingency coefficient goes from 0 to 
#' \eqn{\sqrt(\frac{min(r, c) - 1}{min(r, c)})}. Sakoda (1977) proposed the 
#' corrected contingency coefficient with a range between 0 and 1. 
#' 
#' @aliases ContCoef
#' @inheritParams Association
#' @param method character string specifying the confidence interval method
#' for the contingency coefficient; currently only \code{"boot"} is implemented
#' 
#' @param correct logical; whether Sakoda's adjusted Pearson's C should be
#' returned; defaults to \code{FALSE}
#' 
#' @return if \code{conf.level = NA}, a numeric scalar containing Pearson's
#' contingency coefficient; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the contingency coefficient.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#' 
#' @details
#' For Pearson's contingency coefficient 
#' no generally accepted analytical confidence intervals exist. If interval 
#' estimation is required, resampling methods such as the bootstrap may be 
#' applied. In applied research, effect size measures with better inferential 
#' properties (e.g. Cramer's V) are usually preferred.
#' 
#' @references 
#' Sakoda, J.M. (1977) Measures of Association for Multivariate Contingency
#' Tables, \emph{Proceedings of the Social Statistics Section of the American
#' Statistical Association} (Part III), 777-780.
#' 
#' @seealso \code{\link[bedrock]{pairApply}}
#' 
#' @examples
#' 
#' tab <- apply(HairEyeColor, c(1,2), sum)
#' contCoef(tab)
#' 
#' # just x and y
#' with(bedrock::untable(tab), contCoef(Hair, Eye))
#' 
#' @family assoc.nominal  
#' @concept association-measure  
#' @concept nominal  
#' @concept chi-square-based
#'
#' @export
contCoef <- function(x, y = NULL, 
                     correct = FALSE,
                     conf.level = NA,
                     sides = c("two.sided","left","right"),
                     method = c("boot"),
                     ...) {
  
  sides  <- match.arg(sides)
  method <- match.arg(method)
  
  # ------------------------------------------
  # Extract bootstrap arguments from ...
  # ------------------------------------------
  dots <- list(...)
  
  R    <- dots$R    %||% 5000
  type <- dots$type %||% "perc"
  
  type <- match.arg(type, c("perc","bca"))
  
  if (!is.numeric(R) || length(R) != 1L || R <= 0)
    stop("'R' must be a positive integer.")
  
  R <- as.integer(R)
  
  # ------------------------------------------
  # Normalize input
  # ------------------------------------------
  tab <- normalizeToConfusion(x, y, mode = "association")
  
  # ------------------------------------------
  # Point estimate (C++)
  # ------------------------------------------
  cc <- contcoef_table_cpp(tab, correct)
  
  if (is.na(conf.level))
    return(cc)
  
  if (method != "boot")
    stop("Currently only method = 'boot' is supported.")
  
  # ------------------------------------------
  # Deterministic base seed from R RNG
  # ------------------------------------------
  base_seed <- as.integer(sample.int(.Machine$integer.max, 1))
  
  alpha <- 1 - conf.level
  
  # ------------------------------------------
  # Percentile bootstrap
  # ------------------------------------------
  if (type == "perc") {
    
    boot_vals <- contcoef_table_boot_cpp(
      tab     = tab,
      R       = R,
      seed    = base_seed,
      correct = correct
    )
    
    probs <- switch(
      sides,
      two.sided = c(alpha/2, 1 - alpha/2),
      left      = c(0, conf.level),
      right     = c(1 - conf.level, 1)
    )
    
    ci <- quantile(boot_vals, probs = probs, names = FALSE)
    
  }
  
  # ------------------------------------------
  # BCa bootstrap
  # ------------------------------------------
  if (type == "bca") {
    
    res <- contcoef_table_boot_bca_cpp(
      tab,
      R,
      base_seed,
      correct,
      conf.level
    )

    cc <- res$estimate
    ci <- c(res$conf.low, res$conf.high)
    
  }
  
  
  return( setNamesX(c(cc,ci), names=c("est","lci","uci")) )
  
  
}
