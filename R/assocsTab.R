
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
  ords <- .assocsGen(x, conf.level = conf.level)

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
                 # , "Mutual Information" = c(MutInf(x),NA,NA)
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



