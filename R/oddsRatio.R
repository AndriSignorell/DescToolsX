
#' Compute Odds Ratios
#'
#' Computes odds ratios for 2x2 contingency tables or binomial
#' generalized linear models.
#'
#' For contingency tables, the function returns the odds ratio together with
#' optional confidence intervals. For binomial generalized linear models,
#' exponentiated regression coefficients together with confidence intervals
#' are returned.
#'
#' @param x an object for which odds ratios should be computed
#' @param ... further arguments passed to methods
#'
#' @return the returned value depends on the input. For contingency tables,
#' \code{conf.level = NA} yields a numeric scalar; otherwise the result is a
#' named numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{odds ratio estimate}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#'
#' For binomial generalized linear models, an object of class
#' \code{"OddsRatio"} is returned.
#'
#' @details
#' For 2x2 contingency tables, the odds ratio is defined as:
#'
#' \deqn{
#' OR = \frac{n_{11} n_{22}}{n_{12} n_{21}}
#' }
#'
#' The following confidence interval methods are available:
#'
#' \itemize{
#'   \item \code{"wald"} Asymptotic Wald interval.
#'   \item \code{"exact"} Fisher exact interval.
#'   \item \code{"midp"} Median unbiased mid-p interval.
#' }
#'
#' For generalized linear models, exponentiated regression coefficients are
#' reported together with Wald or profile likelihood confidence intervals.
#'
#' @references
#' Agresti, A. (2013). \emph{Categorical Data Analysis} (3rd ed.).
#' Wiley.
#'
#' Fisher, R. A. (1935). The logic of inductive inference.
#' \emph{Journal of the Royal Statistical Society},
#' \emph{98}(1), 39--82.
#'
#' Gart, J. J. (1966). Alternative analyses of contingency tables.
#' \emph{Journal of the Royal Statistical Society Series B},
#' \emph{28}(1), 164--179.
#'
#' @note
#' No short alias is exported by default to avoid conflicts with
#' \pkg{rlang} and base R naming conventions.  Call
#' \code{\link{attachAliases}()} once per session (or script) to make
#' \code{or()} available as a convenient shorthand.
#' 
#' @seealso [attachAliases]
#'
#' @examples
#' # 2x2 contingency table
#' tab <- matrix(
#'   c(10, 20,
#'     5, 30),
#'   nrow = 2
#' )
#'
#' oddsRatio(tab)
#'
#' oddsRatio(
#'   tab,
#'   conf.level = 0.95
#' )
#'
#'
#' # logistic regression
#' fit <- glm(
#'   vs ~ am,
#'   data = mtcars,
#'   family = binomial
#' )
#'
#' oddsRatio(fit)
#'
#'
#' @family effect.size  
#' @concept effect-size  
#' @concept binary-outcome
#'
#'
#' @export
oddsRatio <- function(x, ...) {
  UseMethod("oddsRatio")
}



#' @param y optional second variable. If supplied,
#'   \code{table(x, y, ...)} is computed.
#' @param conf.level confidence level for interval estimation.
#'   If \code{NA}, only the point estimate is returned.
#' @param sides type of confidence interval. One of
#'   \code{"two.sided"}, \code{"left"}, or \code{"right"}.
#' @param method character string specifying the estimation method.
#'   One of \code{"wald"}, \code{"exact"}, or \code{"midp"}.
#' @param interval numeric vector of length two specifying the search interval
#'   used by the mid-p method
#'
#' @rdname oddsRatio
#' @method oddsRatio default
#' @export
oddsRatio.default <- function(
    x,
    y = NULL,
    conf.level = NA,
    sides = c("two.sided", "left", "right"),
    method = c("wald", "exact", "midp"),
    interval = c(0, 1000),
    ...
) {
  
  if (!is.null(y))
    x <- table(x, y, ...)
  
  if (!is.numeric(x))
    stop("Argument 'x' must be numeric.")
  
  if (anyNA(x))
    stop("Argument 'x' must not contain missing values.")
  
  if (length(dim(x)) != 2L)
    stop("Argument 'x' must be a matrix.")
  
  if (!all(dim(x) == c(2L, 2L)))
    stop("Argument 'x' must be a 2x2 matrix.")
  
  if (any(x < 0))
    stop("Argument 'x' must contain non-negative counts.")
  
  if (any(x %% 1 != 0))
    stop("Argument 'x' must contain integer counts.")
  
  if (any(rowSums(x) == 0))
    stop("Rows of 'x' must contain positive totals.")
  
  method <- match.arg(method)
  sides  <- match.arg(sides)
  
  res <- switch(
    method,
    
    "wald" = .oddsRatioWald(
      x = x,
      conf.level = conf.level,
      sides = sides
    ),
    
    "exact" = .oddsRatioExact(
      x = x,
      conf.level = conf.level,
      sides = sides
    ),
    
    "midp" = .oddsRatioMidP(
      x = x,
      conf.level = conf.level,
      sides = sides,
      interval = interval
    )
  )
  
  res
  
}



#' @rdname oddsRatio
#' @param method character string specifying the interval method.
#'   One of \code{"wald"} or \code{"profile"}.
#'
#' @method oddsRatio glm
#' @export
oddsRatio.glm <- function(
    x,
    conf.level = 0.95,
    method = c("wald", "profile"),
    sides = c("two.sided", "left", "right"),
    ...
) {
  
  if (!inherits(x, "glm"))
    stop("Object must inherit from class 'glm'.")
  
  if (family(x)$family != "binomial")
    stop("Model must use binomial family.")
  
  method <- match.arg(method)
  sides  <- match.arg(sides)
  
  coefTable <- summary(x)$coefficients
  
  beta <- coefTable[, "Estimate"]
  se   <- coefTable[, "Std. Error"]
  pval <- coefTable[, "Pr(>|z|)"]
  
  est <- exp(beta)
  
  alpha <- 1 - conf.level
  
  if (method == "wald") {
    
    if (sides == "two.sided") {
      
      z <- qnorm(1 - alpha / 2)
      
      lci <- exp(beta - z * se)
      uci <- exp(beta + z * se)
      
    } else if (sides == "left") {
      
      z <- qnorm(1 - alpha)
      
      lci <- exp(beta - z * se)
      uci <- Inf
      
    } else {
      
      z <- qnorm(1 - alpha)
      
      lci <- 0
      uci <- exp(beta + z * se)
      
    }
    
  } else {
    
    if (sides != "two.sided") {
      warning(
        "Profile likelihood intervals are always two-sided."
      )
    }
    
    ci <- exp(
      confint(
        x,
        level = conf.level
      )
    )
    
    ci <- ci[rownames(coefTable), , drop = FALSE]
    
    lci <- ci[, 1]
    uci <- ci[, 2]
    
  }
  
  coefficients <- data.frame(
    term = rownames(coefTable),
    est = est,
    logEst = beta,
    stdError = se,
    pValue = pval,
    lci = lci,
    uci = uci,
    row.names = NULL
  )
  
  res <- list(
    coefficients = coefficients,
    source = "glm",
    method = method,
    conf.level = conf.level,
    sides = sides,
    nObs = nobs(x),
    call = x$call
  )
  
  class(res) <- "OddsRatio"
  
  res
  
}


#' @rdname oddsRatio
#' @param digits number of digits used for printing
#' @export
print.OddsRatio <- function(x, digits = 3, ...) {
  
  cat("\nCall:\n")
  print(x$call)
  
  cat(
    "\nOdds Ratios (",
    x$conf.level * 100,
    "% ",
    x$sides,
    " CI, method = ",
    x$method,
    "):\n\n",
    sep = ""
  )
  
  tab <- x$coefficients
  
  tabPrint <- data.frame(
    est = round(tab$est, digits),
    lci = round(tab$lci, digits),
    uci = round(tab$uci, digits),
    pValue = signif(tab$pValue, digits)
  )
  
  rownames(tabPrint) <- tab$term
  
  print(tabPrint)
  
  cat("\n")
  
  invisible(x)
  
}



# == internal helper functions ==============================================

.oddsRatioWald <- function(
    x,
    conf.level,
    sides
) {
  
  if (any(x == 0))
    x <- x + 0.5
  
  logEst <- (
    log(x[1, 1]) +
      log(x[2, 2]) -
      log(x[1, 2]) -
      log(x[2, 1])
  )
  
  est <- exp(logEst)
  
  if (is.na(conf.level))
    return(est)
  
  se <- sqrt(sum(1 / x))
  
  alpha <- 1 - conf.level
  
  if (sides == "two.sided") {
    
    z <- qnorm(1 - alpha / 2)
    
    lci <- exp(logEst - z * se)
    uci <- exp(logEst + z * se)
    
  } else if (sides == "left") {
    
    z <- qnorm(1 - alpha)
    
    lci <- exp(logEst - z * se)
    uci <- Inf
    
  } else {
    
    z <- qnorm(1 - alpha)
    
    lci <- 0
    uci <- exp(logEst + z * se)
    
  }
  
  c(
    est = est,
    lci = lci,
    uci = uci
  )
  
}



.oddsRatioExact <- function(
    x,
    conf.level,
    sides
) {
  
  alternative <- switch(
    sides,
    "two.sided" = "two.sided",
    "left" = "less",
    "right" = "greater"
  )
  
  fit <- fisher.test(
    x,
    conf.int = !is.na(conf.level),
    conf.level = conf.level,
    alternative = alternative
  )
  
  est <- unname(fit$estimate)
  
  if (is.na(conf.level))
    return(est)
  
  c(
    est = est,
    lci = fit$conf.int[1],
    uci = fit$conf.int[2]
  )
  
}



.oddsRatioMidP <- function(
    x,
    conf.level,
    sides,
    interval
) {
  
  a1 <- x[1, 1]
  a0 <- x[1, 2]
  b1 <- x[2, 1]
  b0 <- x[2, 2]
  
  .mue <- function(or) {
    
    mm <- matrix(
      c(a1, a0, b1, b0),
      nrow = 2,
      byrow = TRUE
    )
    
    fisher.test(
      mm,
      or = or,
      alternative = "less"
    )$p.value -
      fisher.test(
        mm,
        or = or,
        alternative = "greater"
      )$p.value
    
  }
  
  .midp <- function(or) {
    
    mm <- matrix(
      c(a1, a0, b1, b0),
      nrow = 2,
      byrow = TRUE
    )
    
    pLower <- fisher.test(
      mm,
      or = or,
      alternative = "less"
    )$p.value
    
    pUpper <- fisher.test(
      mm,
      or = or,
      alternative = "greater"
    )$p.value
    
    0.5 * (pLower - pUpper + 1)
    
  }
  
  est <- uniroot(
    .mue,
    interval = interval
  )$root
  
  if (is.na(conf.level))
    return(est)
  
  alpha <- 1 - conf.level
  
  lci <- uniroot(
    function(or) {
      1 - .midp(or) - alpha / 2
    },
    interval = interval
  )$root
  
  uci <- 1 / uniroot(
    function(or) {
      .midp(1 / or) - alpha / 2
    },
    interval = interval
  )$root
  
  c(
    est = est,
    lci = lci,
    uci = uci
  )
  
}
