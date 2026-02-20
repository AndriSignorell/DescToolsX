
#' Yule's Coefficients of Association (Q and Y)
#'
#' Computes Yule's Q or Y for a 2x2 contingency table, optionally with
#' asymptotic confidence interval based on the log odds ratio.
#'
#' @param x A 2x2 contingency table (matrix or table). If \code{y} is supplied,
#'   \code{x} and \code{y} are cross-tabulated via \code{table()}.
#' @param y Optional second variable for cross-tabulation.
#' @param conf.level Confidence level for the interval. Default is 0.95.
#' @param sides Indicates the alternative hypothesis and type of interval.
#'   One of \code{"two.sided"}, \code{"less"}, or \code{"greater"}.
#' @param correction Logical; if \code{TRUE}, applies Haldane-Anscombe correction
#'   (adds 0.5 to all cells).
#' @param ... Further arguments passed to \code{table()}.
#'
#' @details
#' For a 2x2 table with cell counts \eqn{a, b, c, d}:
#'
#' Odds ratio:
#' \deqn{OR = \frac{ad}{bc}}
#'
#' Yule's Q:
#' \deqn{Q = \frac{OR - 1}{OR + 1} = \tanh(\log(OR))}
#'
#' Yule's Y:
#' \deqn{Y = \frac{\sqrt{OR} - 1}{\sqrt{OR} + 1}
#'      = \tanh\left(\frac{1}{2}\log(OR)\right)}
#'
#' Confidence intervals are obtained from the asymptotic normal approximation:
#' \deqn{\log(OR) \pm z \cdot \sqrt{1/a + 1/b + 1/c + 1/d}}
#' and then transformed to the selected coefficient.
#' 
#' @name yuleQY
#'
#' @return a single numeric value if no confidence intervals are requested,\cr
#' and otherwise a numeric vector with 3 elements for the estimate, the lower
#' and the upper confidence interval
#'
#' @references
#' Yule, G. U. (1912). On the methods of measuring association between two attributes.
#'
#' @examples
#' m <- matrix(c(12, 5, 3, 20), nrow = 2)
#' yuleQ(m)
#' yuleY(m, conf.level = 0.95)
#'
#' @export
#'  
#' @family topic.association-measures
#' @concept association
#' @concept contingency-tables
#' @concept categorical-data
#' 

#' @rdname yuleQY
#' @export
yuleQ <- function(x, y=NULL,
                  conf.level = 0.95,
                  sides = c("two.sided","left","right"),
                  correction = FALSE, ...){
  
  if(!is.null(y))
    x <- table(x, y, ...)
  
  sides <- match.arg(sides)
  
  stopifnot(is.matrix(x), all(dim(x) == c(2,2)))
  
  if(correction) x <- x + 0.5
  
  a <- x[1,1]; b <- x[1,2]
  c <- x[2,1]; d <- x[2,2]
  
  OR <- (a*d)/(b*c)
  Q  <- (OR - 1)/(OR + 1)
  
  se <- sqrt(1/a + 1/b + 1/c + 1/d)
  logOR <- log(OR)
  
  alpha <- 1 - conf.level
  
  if(sides == "two.sided"){
    z <- qnorm(1 - alpha/2)
    lower_log <- logOR - z*se
    upper_log <- logOR + z*se
  }
  
  if(sides == "left"){
    z <- qnorm(1 - alpha)
    lower_log <- logOR - z*se
    upper_log <- Inf
  }
  
  if(sides == "right"){
    z <- qnorm(1 - alpha)
    lower_log <- -Inf
    upper_log <- logOR + z*se
  }
  
  lower_OR <- exp(lower_log)
  upper_OR <- exp(upper_log)
  
  lower_Q <- if(is.finite(lower_OR)) (lower_OR - 1)/(lower_OR + 1) else -1
  upper_Q <- if(is.finite(upper_OR)) (upper_OR - 1)/(upper_OR + 1) else 1
  
  c(est = Q,
    lci = lower_Q,
    uci = upper_Q)
}



#' @rdname yuleQY
#' @export
yuleY <- function(x, y=NULL, 
                     conf.level = 0.95,
                     sides = c("two.sided","left","right"),
                     correction = FALSE, ...){
  
  if(!is.null(y))
    x <- table(x, y, ...)
  
  sides <- match.arg(sides)
  
  stopifnot(is.matrix(x), all(dim(x) == c(2,2)))
  
  if(correction) x <- x + 0.5
  
  a <- x[1,1]; b <- x[1,2]
  c <- x[2,1]; d <- x[2,2]
  
  OR <- (a*d)/(b*c)
  logOR <- log(OR)
  
  # Schaetzer fuer Y (stabiler via tanh)
  Y <- tanh(logOR / 2)
  
  se <- sqrt(1/a + 1/b + 1/c + 1/d)
  alpha <- 1 - conf.level
  
  if(sides == "two.sided"){
    z <- qnorm(1 - alpha/2)
    lower_log <- logOR - z*se
    upper_log <- logOR + z*se
  }
  
  if(sides == "left"){
    z <- qnorm(1 - alpha)
    lower_log <- logOR - z*se
    upper_log <- Inf
  }
  
  if(sides == "right"){
    z <- qnorm(1 - alpha)
    lower_log <- -Inf
    upper_log <- logOR + z*se
  }
  
  lower_Y <- if(is.finite(lower_log)) tanh(lower_log/2) else -1
  upper_Y <- if(is.finite(upper_log)) tanh(upper_log/2) else 1
  
  c(est = Y,
    lci = lower_Y,
    uci = upper_Y)
}
