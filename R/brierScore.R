
#' Brier Score for Binary Outcomes
#'
#' Computes the Brier score for probabilistic predictions of a binary outcome.
#' The function accepts either a binary response vector with predicted
#' probabilities, or a fitted classification model object.
#'
#' @param x A binary response vector coded as \code{0} and \code{1},
#'   or a fitted model object (e.g., \code{glm}) providing predicted
#'   probabilities.
#'
#' @param pred A numeric vector of predicted probabilities for the event
#'   coded as \code{1}. If \code{NULL}, predictions are extracted from
#'   the supplied model object.
#'
#' @param scaled Logical. If \code{FALSE} (default), the raw Brier score
#'   is returned. If \code{TRUE}, a scaled version is computed:
#'   \deqn{1 - \frac{BS}{BS_{max}}}
#'   where \eqn{BS_{max}} is the Brier score of a non-informative model.
#'
#' @param conf.level Confidence level of the interval. If set to \code{NA}
#'   (default), no confidence interval is computed.
#'
#' @param sides A character string specifying the side of the confidence
#'   interval, one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}. Partial matching is allowed.
#'
#' @param method Character string specifying the type of confidence interval.
#'   One of:
#'   \itemize{
#'     \item \code{"none"} - no interval (default if \code{conf.level = NA})
#'     \item \code{"normal"} - analytic normal approximation
#'     \item \code{"percentile"} - bootstrap percentile interval
#'     \item \code{"student"} - studentized bootstrap interval
#'     \item \code{"bca"} - bias-corrected and accelerated bootstrap interval
#'   }
#'
#' @param R Number of bootstrap resamples used for bootstrap-based
#'   confidence intervals. Ignored for analytic intervals.
#'
#' @param ... Further arguments (currently not used).
#'
#' @details
#' The Brier score is defined as
#' \deqn{
#' BS = \frac{1}{n} \sum_{i=1}^{n}
#' \left( y_i (1 - \hat{p}_i)^2 +
#' (1 - y_i) \hat{p}_i^2 \right),
#' }
#' where \eqn{y_i} is the observed binary outcome and \eqn{\hat{p}_i}
#' the predicted probability of the event.
#'
#' Smaller values indicate better predictive accuracy.
#'
#' If a model object is supplied, predicted probabilities are extracted
#' using \code{predict()}. For \code{glm} objects, \code{type = "response"}
#' is used.
#'
#' Confidence intervals can be computed either analytically (normal
#' approximation) or via bootstrap methods. Bootstrap intervals are
#' generally applicable and do not require closed-form variance
#' expressions. The \code{"bca"} method provides improved coverage in
#' the presence of bias or skewness.
#'
#' @return
#' If \code{conf.level = NA}, a numeric scalar containing the Brier score.
#'
#' Otherwise, a named numeric vector with components:
#' \itemize{
#'   \item \code{brier} - the point estimate
#'   \item \code{lci} - lower confidence limit
#'   \item \code{uci} - upper confidence limit
#' }
#'
#' @references
#' Brier, G. W. (1950).
#' Verification of forecasts expressed in terms of probability.
#' \emph{Monthly Weather Review}, 78(1), 1-3.
#'
#' Efron, B., & Tibshirani, R. J. (1993).
#' \emph{An Introduction to the Bootstrap}.
#' Chapman & Hall.
#'
#' @examples
#' y <- rbinom(200, 1, 0.4)
#' p <- runif(200)
#'
#' # Raw Brier score
#' brierScore(y, p)
#'
#' # Scaled Brier score
#' brierScore(y, p, scaled = TRUE)
#'
#' # Bootstrap confidence interval
#' brierScore(y, p,
#'            conf.level = 0.95,
#'            method = "percentile",
#'            R = 999)
#'
#' @seealso
#' \code{\link[stats]{predict}},
#' \code{\link[boot]{boot}}
#'


#' @export
brierScore <- function(x, pred = NULL,
                       scaled = FALSE,
                       conf.level = NA,
                       sides = c("two.sided", "left", "right"),
                       method = c("percentile", "normal",
                                  "student", "bca"),
                       R = 1999,
                       ...) {
  
  sides  <- match.arg(sides)
  method <- match.arg(method)
  
  # -----------------------------
  # Core Brier
  # -----------------------------
  .Brier <- function(resp, pred, scaled = FALSE) {
    
    if (!all(resp %in% c(0,1)))
      stop("brierScore requires binary response.")
    
    bs <- mean(resp * (1 - pred)^2 +
                 (1 - resp) * pred^2)
    
    if (scaled) {
      mean_y <- mean(resp)
      Bmax <- mean_y * (1 - mean_y)^2 +
        (1 - mean_y) * mean_y^2
      bs <- 1 - bs / Bmax
    }
    
    bs
  }
  
  # -----------------------------
  # Extract response/prediction
  # -----------------------------
  .numResponse <- function(obj) {
    obj$terms <- eval(obj$call$formula)
    res <- model.response(model.frame(obj))
    if (is.factor(res))
      res <- as.numeric(res) - 1
    res
  }
  
  if (!is.null(pred)) {
    resp <- x
  } else {
    
    if (inherits(x, "glm")) {
      pred <- predict(x, type = "response")
      resp <- .numResponse(x)
    } else {
      pred <- predict(x, type = "prob")[,2]
      resp <- .numResponse(x)
    }
  }
  
  # -----------------------------
  # Point estimate
  # -----------------------------
  bs_hat <- .Brier(resp, pred, scaled)
  
  if (is.na(conf.level))
    return(bs_hat)
  
  if (conf.level <= 0 || conf.level >= 1)
    stop("conf.level must be in (0,1)")
  
  alpha <- 1 - conf.level
  n     <- length(resp)
  
  # -----------------------------
  # Bootstrap replicates
  # -----------------------------
  if (method %in% c("percentile", "student", "bca")) {
    
    boot_vals <- brier_boot_cpp(resp, pred, R, scaled)
    
    # ----- Percentile -----
    if (method == "percentile") {
      
      probs <- switch(sides,
                      "two.sided" = c(alpha/2, 1-alpha/2),
                      "left"      = c(alpha, 1),
                      "right"     = c(0, 1-alpha))
      
      ci <- quantile(boot_vals, probs = probs, names = FALSE)
    }
    
    # ----- Studentized -----
    if (method == "student") {
      
      se_boot <- sd(boot_vals)
      t_vals  <- (boot_vals - bs_hat) / se_boot
      
      q <- quantile(t_vals,
                    probs = c(1-alpha/2, alpha/2),
                    names = FALSE)
      
      ci <- bs_hat - q * se_boot
    }
    
    # ----- BCa -----
    if (method == "bca") {
      
      z0 <- qnorm(mean(boot_vals < bs_hat))
      
      # Jackknife acceleration
      jack <- numeric(n)
      for (i in seq_len(n))
        jack[i] <- .Brier(resp[-i], pred[-i], scaled)
      
      jack_mean <- mean(jack)
      num <- sum((jack_mean - jack)^3)
      den <- 6 * (sum((jack_mean - jack)^2))^(3/2)
      a <- num / den
      
      z_alpha <- qnorm(c(alpha/2, 1-alpha/2))
      
      adj <- pnorm(z0 + (z0 + z_alpha) /
                     (1 - a * (z0 + z_alpha)))
      
      ci <- quantile(boot_vals, probs = adj, names = FALSE)
    }
  }
  
  # -----------------------------
  # Normal approximation
  # -----------------------------
  if (method == "normal") {
    
    var_hat <- var(resp * (1 - pred)^2 +
                     (1 - resp) * pred^2) / n
    
    se <- sqrt(var_hat)
    
    z  <- qnorm(1 - alpha/2)
    
    ci <- bs_hat + c(-1,1) * z * se
  }
  
  names(ci) <- c("lci", "uci")
  
  c(brier = bs_hat, ci)
  
}
