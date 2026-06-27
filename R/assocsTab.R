
#' Association Measures 
#' 
#' Collects a number of association measures for nominal and ordinal data. 
#' 
#' This function wraps the association measures phi, contingency coefficient,
#' Cramer's V, Goodman Kruskal's Gamma, Kendall's Tau-b, Stuart's Tau-c,
#' Somers' Delta, Pearson and Spearman correlation, Guttman's Lambda, Theil's
#' Uncertainty Coefficient and the mutual information.
#' 
#' @aliases Assocs print.Assocs
#' @param x a 2 dimensional contingency table or a matrix. 
#' @param conf.level confidence level of the interval. If set to \code{NA} no
#' confidence interval will be calculated. Default is 0.95.
#' 
#' @param verbose Integer (1–3) controlling verbosity.
#'   Defaults to \code{getOption("DescTools.verbose", 2)} if \code{NULL}.
#'   Higher values produce more detailed output.
#'    
#' @return numeric matrix, dimension \verb{[1:17, 1:3]}\cr the first column contains
#' the estimate, the second the lower confidence interval, the third the upper
#' one.
#' @seealso \code{\link{Association}}
#' @keywords multivariate
#' @examples
#' 
#' options(scipen=8)
#' 
#' # Example taken from: SAS/STAT(R) 9.2 User's Guide, Second Edition, The FREQ Procedure
#' # http://support.sas.com/documentation/cdl/en/statugfreq/63124/PDF/default/statugfreq.pdf
#' # Hair-Eye-Color pp. 1816
#' 
#' tob <- as.table(matrix(c(
#'   69, 28, 68, 51,  6,
#'   69, 38, 55, 37,  0,
#'   90, 47, 94, 94, 16
#' ), nrow=3, byrow=TRUE,
#'    dimnames=list(eye=c("blue","green","brown"),
#'                  hair=c("fair","red","medium","dark","black")) ))
#' desc(tob)
#' assocsTab(tob)
#' 
#' # Example taken from: http://www.math.wpi.edu/saspdf/stat/chap28.pdf
#' # pp. 1349
#' 
#' pain <- as.table(matrix(c(
#'    26,  6,
#'    26,  7,
#'    23,  9,
#'    18, 14,
#'     9, 23
#'    ), ncol=2, byrow=TRUE))
#' 
#' desc(pain)
#' assocsTab(pain)
#' 




#' @family assoc.nominal
#' @concept association-measures
#' @concept descriptive-statistics
#' @concept table-manipulation
#'
#'
#' @export
assocsTab <- function(x, conf.level = 0.95, verbose = 2){
  
  verbose <- .checkVerbose(verbose)
  
  # all association measures combined for table description
  # get generic hard assocs for tables
  ords <- assocsXY(x, conf.level = conf.level)

  if(is.na(conf.level)){
    res <- rbind("Contingency Coeff." = c(contCoef(x), NA, NA))
    res <- rbind(res, "Cramer V" = c(cramerV(x), NA, NA))
    res <- rbind(res, "Kendall Tau-b" = ords$tau_b)
    
  } else {
    res <- rbind("Contingency Coeff." = contCoef(x, conf.level=conf.level))
    res <- rbind(res, "Cramer V" = cramerV(x, conf.level=conf.level))
    res <- rbind(res, "Kendall Tau-b" = ords$tau_b)
  }
  
  if(verbose == 3) {

    res <- rbind(res
                 , "Goodman Kruskal Gamma" = ords$gamma
                 , "Stuart Tau-c" = ords$tau_c
                 , "Somers D R|C" = ords$somers
                 , "Pearson Correlation" = pearsonCor(x, conf.level = conf.level)
                 , "Spearman Correlation" = spearmanCor(x, conf.level=conf.level)
#                 , "Lambda C|R" = lambda(x, direction="column", conf.level=conf.level)
                 , "Lambda R|C" = lambda(x, direction="row", conf.level=conf.level)
                 , "Lambda sym" = lambda(x, direction="sym", conf.level=conf.level)
#                 , "Uncertainty Coeff. C|R" = uncertCoef(x, direction="column", conf.level=conf.level)
                 , "Uncertainty Coeff. R|C" = uncertCoef(x, direction="row", conf.level=conf.level)
                 , "Uncertainty Coeff. sym" = uncertCoef(x, direction="sym", conf.level=conf.level)
                 , "Mutual Information" = c(mutInf(x),NA,NA)
    ) }
  
  if(verbose == 3)
    dimnames(res)[[2]][1] <- "est"
  else
    dimnames(res)[[2]] <- c("est", "lci", "uci")
  
  class(res) <- c("AssocsTab", "matrix")
  return(res)
  
}



#' @export
print.AssocsTab <- function(x, digits=4, ...){
  
  out <- fm(unclass(x), digits=digits)
  
  if(nrow(x) == 3){
    
  } else {
    # only for mutinf, which has been removed ... 
    # out[c(1,16), 2:3] <- "      -"
  }
  dimnames(out) <- dimnames(x)
  
  print(data.frame(out), quote=FALSE)
}



#' Ordinal association measures for two variables or a contingency table
#'
#' Computes concordance-based association measures (Goodman-Kruskal
#' \eqn{\gamma}, Kendall's \eqn{\tau_a}, \eqn{\tau_b}, \eqn{\tau_c},
#' Somers' \eqn{D}, and the c-statistic) for a pair of ordinal vectors or
#' a pre-computed contingency table.  Optionally returns confidence
#' intervals.
#'
#' @param x Either a numeric vector (when \code{y} is supplied) or a
#'   matrix / table representing a contingency table (when \code{y} is
#'   \code{NULL}).
#' @param y Optional numeric vector of the same length as \code{x}.  If
#'   supplied, the measures are computed from the raw paired observations
#'   via a fast C++ routine.  If \code{NULL}, \code{x} is treated as a
#'   contingency table.
#' @param which Character string selecting which measure(s) to return.
#'   One of \code{"all"} (default), \code{"gamma"}, \code{"tau-a"},
#'   \code{"tau-b"}, \code{"tau-c"}, \code{"somers"}, or \code{"cstat"}.
#' @param conf.level Numeric scalar in \eqn{(0, 1)}.  If supplied,
#'   confidence intervals are appended to each measure.  Default
#'   \code{NA} suppresses intervals.
#' @param direction Character string controlling the direction for
#'   Somers' \eqn{D} when \code{y = NULL} (table mode): \code{"row"}
#'   (default) treats row totals as the dependent variable;
#'   \code{"column"} treats column totals as dependent.
#'
#' @return A named list with one element per requested measure.  Each
#'   element is a named numeric scalar (point estimate only) or a named
#'   numeric vector of length 3 (estimate, lower CI, upper CI) when
#'   \code{conf.level} is supplied.
#'
#' @details
#' Two computational paths are used:
#'
#' \describe{
#'   \item{XY mode (\code{y} supplied)}{A compiled C++ routine
#'     (\code{assoc_cpp}) processes the raw paired observations directly.
#'     This is substantially faster than the table path for large
#'     vectors.}
#'   \item{Table mode (\code{y = NULL})}{Concordant and discordant pairs
#'     are counted via \code{\link{conDisPairs}}.  Confidence intervals
#'     are obtained from \code{.tableAssocCI}.}
#' }
#'
#' The c-statistic (area under the ROC curve for a binary outcome) equals
#' \eqn{(\text{Somers' } D + 1) / 2}.
#'
#' @examples
#' # From raw vectors
#' x <- c(1, 2, 3, 2, 1, 3)
#' y <- c(2, 3, 3, 1, 1, 2)
#' assocsXY(x, y)
#' assocsXY(x, y, which = "cstat")
#' assocsXY(x, y, conf.level = 0.95)
#'
#' # From a contingency table
#' tab <- table(cut(swiss$Fertility, 3), cut(swiss$Education, 3))
#' assocsXY(tab)
#' assocsXY(tab, which = "gamma", conf.level = 0.95)
#'
#' @seealso \code{\link{conDisPairs}}

#' @export
assocsXY <- function(x, y = NULL,
                       which = c("all","gamma","tau-a","tau-b","tau-c","somers","cstat"),
                       conf.level = NA,
                       direction = c("row","column")) {
  
  direction <- match.arg(direction)
  which <- gsub("-", "_", match.arg(which), fixed = TRUE)
  
  # ============================
  # XY MODE (use C++)
  # ============================
  if(!is.null(y)){
    
    cl <- if(is.na(conf.level)) NA_real_ else conf.level
    z <- assoc_cpp(x, y, cl)
    
    res_all <- list(
      gamma  = if(is.na(conf.level)) z["gamma"] else z[c("gamma","gamma_l","gamma_u")],
      tau_a  = if(is.na(conf.level)) z["tau_a"] else z[c("tau_a","tau_a_l","tau_a_u")],
      tau_b  = if(is.na(conf.level)) z["tau_b"] else z[c("tau_b","tau_b_l","tau_b_u")],
      tau_c  = if(is.na(conf.level)) z["tau_c"] else z[c("tau_c","tau_c_l","tau_c_u")],
      somers = if(is.na(conf.level)) z["somers"] else z[c("somers","somers_l","somers_u")],
      cstat  = if(is.na(conf.level)) z["cstat"] else z[c("cstat","cstat_l","cstat_u")]
    )
    if(is.na(conf.level))
      res_all <- lapply(res_all, unname)
    else
      res_all <- lapply(res_all, setNamesX, names=c("est", "lci", "uci"))
    
  } else {
    
    # ============================
    # TABLE MODE
    # ============================
    tab <- as.table(x)
    
    # base counts
    cd <- conDisPairs(tab)
    
    C  <- unname(cd[["C"]])
    D  <- unname(cd[["D"]])
    Tx <- unname(cd[["Ties_X"]])
    Ty <- unname(cd[["Ties_Y"]])
    
    n  <- sum(tab)
    n0 <- n*(n-1)/2
    S  <- C - D
    
    # measures
    gamma <- S / (C + D)
    tau_a <- S / n0
    tau_b <- S / sqrt((n0 - Tx)*(n0 - Ty))
    
    m <- min(dim(tab))
    tau_c <- 2*S*m/(n^2*(m-1))
    
    ni <- if(direction=="row") colSums(tab) else rowSums(tab)
    denom <- n0 - sum(ni*(ni-1)/2)
    
    somers <- S / denom
    cstat  <- (somers + 1)/2
    
    # no CI case
    if(is.na(conf.level)){
      res_all <- list(
        gamma  = gamma,
        tau_a  = tau_a,
        tau_b  = tau_b,
        tau_c  = tau_c,
        somers = somers,
        cstat  = cstat
      )
      
    } else {
      
      # exact table CI
      ci_all <- .tableAssocCI(tab, conf.level, direction)
      
      res_all <- list(
        gamma  = ci_all$gamma,
        tau_a  = ci_all$tau_a,
        tau_b  = ci_all$tau_b,
        tau_c  = ci_all$tau_c,
        somers = ci_all$somers,
        cstat  = ci_all$cstat
      )
      
      res_all <- lapply(res_all, setNamesX, names=c("est", "lci", "uci"))
      
    }
  }
  
  # ============================
  # SELECT WHICH
  # ============================
  if(which != "all"){
    res_all <- res_all[which]
  }
  
  return(res_all)
}


# == internal helper functions ================================================


.tableAssocCI <- function(tab, conf.level=0.95, direction="row"){
  
  cd <- condis_pairs_tab_cpp(tab)
  
  pi.c <- cd$pi.c
  pi.d <- cd$pi.d
  
  n <- sum(tab)
  S <- cd$C - cd$D
  
  n0 <- n*(n-1)/2
  
  # ties
  rowSum <- rowSums(tab)
  colSum <- colSums(tab)
  
  Tx <- cd$Ties_X
  Ty <- cd$Ties_Y
  
  # ============================
  # SOMERS
  # ============================
  ni <- if(direction=="row") colSum else rowSum
  T  <- n0 - sum(ni*(ni-1)/2)
  
  psi_s <- (T*(pi.c - pi.d) - S*(n - ni)) / T^2
  var_s <- sum(tab * psi_s^2)
  
  somers <- S / T
  
  
  # ============================
  # GAMMA
  # ============================
  denom <- cd$C + cd$D
  
  psi_g <- ((pi.c - pi.d)*denom - S*(pi.c + pi.d)) / denom^2
  var_g <- sum(tab * psi_g^2)
  
  gamma <- S / denom
  
  # ============================
  # TAU-A
  # ============================
  psi_a <- (pi.c - pi.d) / n0
  var_a <- sum(tab * psi_a^2)
  
  tau_a <- S / n0
  
  # ============================
  # TAU-C
  # ============================
  m <- min(dim(tab))
  k <- 2*m/(n^2*(m-1))
  
  psi_c <- k * (pi.c - pi.d)
  var_c <- sum(tab * psi_c^2)
  
  tau_c <- k * S
  
  # ============================
  # TAU-B 
  # ============================
  ti <- rowSum
  uj <- colSum
  
  n1 <- sum(ti * (ti-1) / 2)
  n2 <- sum(uj * (uj-1) / 2)
  
  tau_b <- S / sqrt((n0-n1)*(n0-n2))
  
  # probabilities
  pi <- tab / n
  
  pdiff <- (pi.c - pi.d) / n
  Pdiff <- 2 * S / n^2
  
  rowsum <- rowSums(pi)
  colsum <- colSums(pi)
  
  rowmat <- matrix(rep(rowsum, ncol(tab)), ncol = ncol(tab))
  colmat <- matrix(rep(colsum, nrow(tab)), nrow = nrow(tab), byrow = TRUE)
  
  delta1 <- sqrt(1 - sum(rowsum^2))
  delta2 <- sqrt(1 - sum(colsum^2))
  
  tauphi <- (2 * pdiff + Pdiff * colmat) * delta2 * delta1 +
    (Pdiff * rowmat * delta2) / delta1
  
  sigma2_b <- ((sum(pi * tauphi^2) - sum(pi * tauphi)^2) /
                 (delta1 * delta2)^4) / n
  
  if(sigma2_b < .Machine$double.eps * 10) sigma2_b <- 0
  
  # ============================
  # CI helper
  # ============================
  z <- qnorm(1-(1-conf.level)/2)
  
  mk <- function(est, var){
    se <- sqrt(var)
    c(est=est,
      lci=max(est - z*se, -1),
      uci=min(est + z*se, 1))
  }
  
  list(
    somers = mk(somers,  var_s),
    gamma  = mk(gamma,  var_g),
    tau_a  = mk(tau_a,  var_a),
    tau_b  = mk(tau_b,  sigma2_b),
    tau_c  = mk(tau_c,  var_c),
    cstat  = mk((somers+1)/2, var_s/4)
  )
}





