

#' Brier Score
#'
#' Computes the Brier score for binary probabilistic predictions, optionally
#' with a confidence interval via a normal approximation or bootstrap.
#'
#' @details
#' The Brier score is defined as
#' \deqn{BS = \frac{1}{n}\sum_{i=1}^n
#'   \bigl[y_i(1-\hat p_i)^2 + (1-y_i)\hat p_i^2\bigr]}
#' where \eqn{y_i \in \{0,1\}} and \eqn{\hat p_i} is the predicted
#' probability.  Lower is better; a perfect model scores 0.
#'
#' The scaled Brier score (\code{scaled = TRUE}) expresses skill relative
#' to the climatological baseline \eqn{BS_{\max}}, yielding 1 for a
#' perfect model and 0 for the no-skill reference.
#'
#' **Normal interval** (\code{method = "normal"})
#'
#' A delta-method normal approximation based on the variance of the
#' per-observation Brier losses.  Fast and deterministic; reliable for
#' moderate to large samples.
#'
#' **Bootstrap interval** (\code{method = "boot"})
#'
#' Case-resampling bootstrap via \code{brier_boot_cpp()}.  The bootstrap
#' type is controlled by the \code{type} argument (passed through
#' \code{...}):
#' \describe{
#'   \item{\code{"bca"}}{bias-corrected and accelerated (default).
#'     Most accurate; requires \eqn{R \geq 200}.}
#'   \item{\code{"perc"}}{percentile interval}
#'   \item{\code{"norm"}}{normal approximation using the bootstrap standard
#'     error}
#' }
#' Further bootstrap arguments passed through \code{...} via
#' \code{.extractBootArgs()}:
#' \describe{
#'   \item{\code{R}}{number of bootstrap replicates (default \code{999})}
#'   \item{\code{parallel}}{parallelisation: \code{"no"},
#'     \code{"multicore"}, or \code{"snow"} (default \code{"no"})}
#'   \item{\code{ncpus}}{number of CPUs (default
#'     \code{getOption("boot.ncpus", 1L)})}
#' }
#'
#' @param x       either a numeric vector of observed binary outcomes
#'   (\eqn{0}/\eqn{1}) when \code{pred} is supplied, or a fitted model
#'   object (\code{glm} or similar) from which both response and
#'   predictions are extracted
#' @param pred    a numeric vector of predicted probabilities in
#'   \eqn{[0,1]}. Required when \code{x} is a numeric vector; ignored
#'   when \code{x} is a model object.
#' @param scaled  logical. Should the scaled Brier score be returned?
#'   Default \code{FALSE}.
#' @param conf.level confidence level of the interval. A single numeric
#'   value in \eqn{(0, 1)}, or \code{NA} (default) to return only the
#'   point estimate.
#' @param sides   a character string specifying the side of the interval:
#'   \code{"two.sided"} (default), \code{"left"}, or \code{"right"}.
#'   Partial matching is supported. \code{"left"} sets \code{uci = Inf};
#'   \code{"right"} sets \code{lci = -Inf}. Ignored when
#'   \code{conf.level = NA}.
#' @param method  confidence interval method: \code{"normal"} (delta-method
#'   approximation, default) or \code{"boot"} (bootstrap via
#'   \code{brier_boot_cpp()})
#' @param ...     further arguments passed to the bootstrap engine when
#'   \code{method = "boot"}: \code{R}, \code{type}, \code{parallel},
#'   \code{ncpus}. See Details.
#'
#' @return if \code{conf.level = NA}, a numeric scalar containing the Brier
#' score; otherwise a named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of the Brier score.}
#'   \item{\code{lci}}{lower confidence interval bound.}
#'   \item{\code{uci}}{upper confidence interval bound.}
#' }
#'
#' @seealso \code{\link[stats]{predict}}
#'
#' @examples
#' set.seed(1)
#' resp <- rbinom(200, 1, 0.4)
#' pred <- plogis(rnorm(200, ifelse(resp == 1, 0.5, -0.5)))
#'
#' brierScore(resp, pred)
#' brierScore(resp, pred, conf.level = 0.95)
#' brierScore(resp, pred, conf.level = 0.95, method = "boot", type = "bca")
#' brierScore(resp, pred, conf.level = 0.95, scaled = TRUE)
#'


#' @family model.metrics  
#' @concept model-evaluation  
#' @concept calibration
#'
#'
#' @export
brierScore <- function(x,
                       pred       = NULL,
                       scaled     = FALSE,
                       conf.level = NA,
                       sides      = c("two.sided", "left", "right"),
                       method     = c("normal", "boot"),
                       ...) {
  
  
  # --- extract resp / pred ---------------------------------------------
  if (!is.null(pred)) {
    resp <- x
  } else {
    if (inherits(x, "glm")) {
      pred <- predict(x, type = "response")
      resp <- .numResponse(x)
    } else {
      pred <- predict(x, type = "prob")[, 2L]
      resp <- .numResponse(x)
    }
  }
  
  # --- validate resp / pred --------------------------------------------
  if (!all(resp %in% c(0L, 1L)))
    stop("'x' (response) must be binary (0/1).")
  if (any(pred < 0 | pred > 1))
    stop("'pred' must contain probabilities in [0, 1].")
  if (length(resp) != length(pred))
    stop("'x' and 'pred' must have the same length.")
  
  # --- point estimate --------------------------------------------------
  bs_hat <- .brierLoss(resp, pred, scaled)
  
  if (is.na(conf.level))
    return(bs_hat)
  
  # --- CI setup --------------------------------------------------------
  if (!is.numeric(conf.level) || length(conf.level) != 1L ||
      conf.level <= 0 || conf.level >= 1)
    stop("Argument 'conf.level' must be a single numeric value in (0, 1).")
  
  sides  <- match.arg(sides)
  method <- match.arg(method)
  
  conf_adj <- if (sides != "two.sided") 1 - 2 * (1 - conf.level) else conf.level
  alpha    <- 1 - conf_adj
  n        <- length(resp)
  
  ci <- switch(method,
               
               normal = {
                 loss    <- resp * (1 - pred)^2 + (1 - resp) * pred^2
                 se      <- sqrt(var(loss) / n)
                 z       <- qnorm(1 - alpha / 2)
                 c(bs_hat - z * se, bs_hat + z * se)
               },
               
               boot = {
                 dots      <- list(...)
                 boot_args <- .extractBootArgs(dots)
                 
                 boot_vals <- brier_boot_cpp(resp, pred, boot_args$R, scaled)
                 
                 switch(boot_args$type,
                        
                        perc = {
                          probs <- switch(sides,
                                          two.sided = c(alpha / 2, 1 - alpha / 2),
                                          left      = c(alpha,     1             ),
                                          right     = c(0,         1 - alpha     ))
                          quantile(boot_vals, probs = probs, names = FALSE)
                        },
                        
                        norm = {
                          se_boot <- sd(boot_vals)
                          z       <- qnorm(1 - alpha / 2)
                          c(bs_hat - z * se_boot, bs_hat + z * se_boot)
                        },
                        
                        bca = {
                          z0 <- qnorm(mean(boot_vals < bs_hat))
                          
                          jack      <- vapply(seq_len(n),
                                              function(i) .brierLoss(resp[-i], pred[-i], scaled),
                                              numeric(1L))
                          jack_mean <- mean(jack)
                          num       <- sum((jack_mean - jack)^3)
                          den       <- 6 * sum((jack_mean - jack)^2)^(3 / 2)
                          a         <- num / den
                          
                          z_alpha <- qnorm(c(alpha / 2, 1 - alpha / 2))
                          adj     <- pnorm(z0 + (z0 + z_alpha) / (1 - a * (z0 + z_alpha)))
                          quantile(boot_vals, probs = adj, names = FALSE)
                        }
                 )
               }
  )
  
  # --- one-sided truncation --------------------------------------------
  # (perc boot with sides already handled above; apply to all others)
  if (!(method == "boot" && boot_args$type == "perc")) {
    if (sides == "left")  ci[2L] <- Inf
    if (sides == "right") ci[1L] <- -Inf
  }
  
  c(est = bs_hat, lci = ci[1L], uci = ci[2L])
}


# == internal helper functions ===============================================

# --- internal: Brier loss per observation and score ------------------

.brierLoss <- function(resp, pred, scaled = FALSE) {
  loss <- resp * (1 - pred)^2 + (1 - resp) * pred^2
  bs   <- mean(loss)
  if (scaled) {
    mean_y <- mean(resp)
    bs_max <- mean_y * (1 - mean_y)^2 + (1 - mean_y) * mean_y^2
    bs     <- 1 - bs / bs_max
  }
  bs
}



# --- internal: extract response from model object --------------------

.numResponse <- function(obj) {
  obj$terms <- eval(obj$call$formula)
  res <- model.response(model.frame(obj))
  if (is.factor(res)) res <- as.numeric(res) - 1L
  res
}

