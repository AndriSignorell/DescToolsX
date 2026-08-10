
#' Spearman Rank Correlation 
#' 
#' Calculate Spearman correlation coefficient and its confidence interval. In
#' addition to the base R function \code{\link{cor}(x, method="spearman")}, 
#' frequency tables are
#' also accepted as arguments (i.e. actually weights are used).
#' 
#' The function calculates Spearman's rho statistic by means of \code{cor(...,
#' method="spearman")} when two variables \code{x} and \code{y} are supplied.
#' If a frequency table is provided an implementation based on SAS
#' documentation is used. Both routes use midranks for ties and agree exactly;
#' see the examples.\cr The confidence intervals are calculated via
#' z-Transformation.\cr
#' 
#' The number of observations entering the z-transformation is
#' \code{length(x)} in the vector interface and \code{sum(x)}, the table total,
#' in the table interface. At least four observations are needed for an
#' interval.
#' 
#' \code{sides} names the side of the interval that carries the finite bound,
#' so \code{"left"} returns \code{[lci, 1]} and \code{"right"} returns
#' \code{[-1, uci]}. Since rho is bounded, the open side is reported at the
#' range boundary rather than as infinite.
#' 
#' @inheritParams Association
#' 
#' @param conf.level confidence level of the interval. If set to \code{NA}
#'   (the default), only the point estimate is returned.
#' @param sides character string specifying the sidedness of the confidence
#'   interval (one of \code{"two.sided"} (default), \code{"left"} or
#'   \code{"right"}). See \code{\link{ConfidenceIntervals}}.
#'
#' @param na.rm logical; whether to remove incomplete pairs. Applies to the
#' vector interface; a frequency table must not contain missing counts.
#' @return if \code{conf.level = NA}, a numeric scalar. Otherwise a named
#' numeric vector with elements:
#' \describe{
#'   \item{\code{est}}{point estimate of Spearman's rank correlation}
#'   \item{\code{lci}}{lower confidence interval bound}
#'   \item{\code{uci}}{upper confidence interval bound}
#' }
#' 
#' @references Conover W. J. (1999) \emph{Practical Nonparametric Statistics
#' (3rd edition)}. Wiley 
#' 
#' @examples
#' 
#' # Example from SAS documentation (PROC FREQ)
#' pain <- as.table(matrix(c(26,  6, 26, 7, 23, 
#'                            9, 18, 14, 9, 23), 
#'                            ncol=5, 
#'         dimnames=list(adverse=c("no", "yes"), dose=1:5)))
#' 
#' spearmanCor(pain)
#' 
#' spearmanCor(pain, conf.level=0.95)
#'   
#' # must be the same as
#' with(lapply(
#'        bedrock::untable(pain, 
#'                         colnames = c("adverse","dose")), 
#'        ordered), 
#'      spearmanCor(adverse, dose, conf.level=0.95))
#' 
#' @seealso \code{\link{Association}} 
#' 
#' @family assoc.continuous  
#' @concept correlation  
#' @concept rank-correlation
#'
#'
#' @export 
spearmanCor <- function(x, y = NULL,
                        conf.level = NA,
                        sides = c("two.sided","left","right"),
                        na.rm = FALSE) {
  
  sides <- match.arg(sides)
  
  conf.level <- checkConfLevel(conf.level)
  
  if(is.null(y)) {
    # implemented following
    # https://support.sas.com/documentation/onlinedoc/stat/151/freq.pdf
    # S. 3103
    
    # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
    # pp 1738
    
    # Old References:
    # https://stat.ethz.ch/pipermail/r-help/2006-October/114319.html
    # fisher z transformation for calc spearmanCor ci :
    # Conover WJ, Practical Nonparametric Statistics (3rd edition). Wiley 1999.
    
    # Without y the whole branch indexes x by two margins, so anything that is
    # not a two-dimensional table fails several lines further down with a
    # message about dim(X) rather than about the argument the caller passed.
    # The check must come BEFORE as.matrix(): that turns a plain vector into an
    # n x 1 matrix, which then passes any test on length(dim(x)) - the vector
    # ran through the whole SAS branch and came back as a silent NA.
    if (length(dim(x)) != 2L)
      stop("'x' must be a two-dimensional numeric frequency table when 'y' is not given")
    
    x <- as.matrix(x)
    
    if (!is.numeric(x))
      stop("'x' must be a two-dimensional numeric frequency table when 'y' is not given")
    
    if (anyNA(x))
      stop("'x' must not contain missing counts; 'na.rm' applies to the vector interface only")
    
    if (any(x < 0))
      stop("'x' must not contain negative counts")
    
    n <- sum(x)
    ni. <- rowSums(x)
    n.j <- colSums(x)
    
    # SAS defines the rank score of row i as the count in the preceding rows
    # plus (n_i. + 1)/2 and centres it at (n + 1)/2. The two halves cancel, so
    # the midrank can be built without them.
    R1i <- cumsum(ni.) - ni. / 2
    C1i <- cumsum(n.j) - n.j / 2
    
    Ri <- R1i - n / 2
    Ci <- C1i - n / 2
    
    v <- sum(x * outer(Ri, Ci))
    
    # F and G would mask the base constants FALSE and TRUE for the rest of the
    # body; named after the SAS notation but spelled out here.
    fRow <- n^3 - sum(ni.^3)
    gCol <- n^3 - sum(n.j^3)
    
    w <- 1/12 * sqrt(fRow * gCol)
    
    rho <- if (w == 0) NA_real_ else v / w
    
  } else {
    
    if (is.ordered(x)) x <- as.numeric(x)
    if (is.ordered(y)) y <- as.numeric(y)
    
    if (!is.numeric(x) || !is.numeric(y))
      stop("'x' and 'y' must be numeric or ordered factors.",
           call. = FALSE)
    
    if (length(x) != length(y))
      stop("'x' and 'y' must have the same length.", call. = FALSE)
    
    # http://www-01.ibm.com/support/docview.wss?uid=swg21478368
    
    if (na.rm) {
      ok <- complete.cases(x, y)
      x  <- x[ok]
      y  <- y[ok]
    }
    
    n <- length(x)
    rho <- cor(x, y, method="spearman")
    
  }
  
  rho <- unname(rho)
  
  if (is.na(conf.level))
    return(rho)
  
  # Below 0.5 the one-sided alpha exceeds 1, qnorm() turns negative and
  # the two bounds come out in reverse order - the clamp below is
  # elementwise and would not notice. Refused as everywhere else.
  if (sides != "two.sided" && conf.level <= 0.5)
    stop(gettextf(
      "a one-sided interval needs 'conf.level' above 0.5, not %g",
      conf.level), domain = NA)
  
  # The z-transformation is undefined at |rho| = 1 (atanh is infinite) and
  # needs n > 3. Both used to be answered with a made-up interval: (rho,
  # rho) at |rho| = 1, which rules out every value below 1 and is a claim
  # no finite sample supports, and (-1, 1) for n <= 3, which looks like a
  # computed result and is merely the whole range. cramerV reports NA in
  # exactly these two situations - see .fisherHalfWidth() there - and the
  # estimate is still returned either way.
  if (is.na(rho)) {
    
    ci <- c(NA_real_, NA_real_)
    
  } else if (abs(rho) >= 1) {
    
    warning("the z-transformation cannot bound a perfect correlation; ",
            "no interval computed", call. = FALSE)
    ci <- c(NA_real_, NA_real_)
    
  } else if (is.na(n) || n <= 3) {
    
    warning("the z-transformation needs more than 3 observations; ",
            "no interval computed", call. = FALSE)
    ci <- c(NA_real_, NA_real_)
    
  } else {
    
    alpha <- if (sides == "two.sided") 1 - conf.level else 2 * (1 - conf.level)
    
    zr <- atanh(rho)
    se <- 1 / sqrt(n - 3)
    
    ci <- tanh(zr + c(-1, 1) * qnorm(1 - alpha / 2) * se)
  }
  
  # rho is bounded, so the open side is reported at the range boundary -
  # applySides() also does the clamping the two lines above used to do
  c(est = rho, applySides(ci, sides, lo = -1, hi = 1))
  
}
