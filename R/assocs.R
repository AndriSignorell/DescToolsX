
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
#' @param out either \code{"def"} or \code{"ext"} defining the set of results.
#' If \code{"def"} is selected, only the value of the statistic is returned,
#' supplemented with the confidence interval if necessary. If the argument is
#' set to \code{"ext"}, an extended result set with various intermediate
#' results is returned.
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
#' Desc(tob)
#' assocs(tob)
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
#' Desc(pain)
#' assocs(pain)
#' 




#' @export
assocs <- function(x, conf.level = 0.95, out = c("def", "ext")){
  
  # all association measures combined
  
  out <- match.arg(out)
  # if(is.null(verbose)) verbose <- "3"
  # if(verbose != "3") conf.level <- NA

  if(is.na(conf.level)){
    res <- rbind("Contingency Coeff." = c(contCoef(x), NA, NA))
    res <- rbind(res, "Cramer V" = c(cramerV(x), NA, NA))
    res <- rbind(res, "Kendall Tau-b" = c(kendallTauB(x), NA, NA))
    
  } else {
    res <- rbind("Contingency Coeff." = contCoef(x, conf.level=conf.level))
    res <- rbind(res, "Cramer V" = cramerV(x, conf.level=conf.level))
    res <- rbind(res, "Kendall Tau-b" = c(kendallTauB(x, conf.level=conf.level)))
  }
  
  if(out == "ext") {
    
    res <- rbind(res
                 , "Goodman Kruskal Gamma" = gkGamma(x, conf.level=conf.level)
                 , "Stuart Tau-c" = stuartTauC(x, conf.level=conf.level)
                 , "Somers D C|R" = somersDelta(x, direction="column", conf.level=conf.level)
                 , "Somers D R|C" = somersDelta(x, direction="r", conf.level=conf.level)
                 , "Pearson Correlation" = pearsonCor(x, conf.level = conf.level)
                 , "Spearman Correlation" = spearmanCor(x, conf.level=conf.level)
                 , "Lambda C|R" = lambda(x, direction="column", conf.level=conf.level)
                 , "Lambda R|C" = lambda(x, direction="row", conf.level=conf.level)
                 , "Lambda sym" = lambda(x, direction="sym", conf.level=conf.level)
                 , "Uncertainty Coeff. C|R" = uncertCoef(x, direction="column", conf.level=conf.level)
                 , "Uncertainty Coeff. R|C" = uncertCoef(x, direction="row", conf.level=conf.level)
                 , "Uncertainty Coeff. sym" = uncertCoef(x, direction="sym", conf.level=conf.level)
                 # , "Mutual Information" = c(MutInf(x),NA,NA)
    ) }
  
  if(out == "ext")
    dimnames(res)[[2]][1] <- "est"
  else
    dimnames(res)[[2]] <- c("est", "lci", "uci")
  
  class(res) <- c("Assocs", "matrix")
  return(res)
  
}



#' @export
print.Assocs <- function(x, digits=4, ...){
  
  out <- fm(unclass(x), digits=digits)
  
  if(nrow(x) == 3){
    
  } else {
    # only for mutinf, which has been removed ... 
    # out[c(1,16), 2:3] <- "      -"
  }
  dimnames(out) <- dimnames(x)
  
  print(data.frame(out), quote=FALSE)
}


