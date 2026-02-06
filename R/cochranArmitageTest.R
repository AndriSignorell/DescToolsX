
#' Cochran-Armitage Test for Trend 
#' 
#' Perform a Cochran Armitage test for trend in binomial proportions across the
#' levels of a single variable. This test is appropriate only when one variable
#' has two levels and the other variable is ordinal. The two-level variable
#' represents the response, and the other represents an explanatory variable
#' with ordered levels. The null hypothesis is the hypothesis of no trend,
#' which means that the binomial proportion is the same for all levels of the
#' explanatory variable.
#' 
#' 
#' @param x a frequency table or a matrix. 
#' @param alternative a character string specifying the alternative hypothesis,
#' must be one of \code{"two.sided"} (default), \code{"one.sided"}. You can
#' specify just the initial letter. 
#' @return A list of class \code{htest}, containing the following components:
#' \item{statistic}{ the z-statistic of the test.} \item{parameter}{ the
#' dimension of the table.} \item{p.value}{ the p-value for the test.}
#' \item{alternative}{a character string describing the alternative
#' hypothesis.} \item{method}{the character string \dQuote{Cochran-Armitage
#' test for trend}.} \item{data.name}{a character string giving the names of
#' the data.}
#' 
#' @author Andri Signorell <andri@@signorell.net> strongly based on code from
#' Eric Lecoutre <lecoutre@@stat.ucl.ac.be>\cr
#' \url{https://stat.ethz.ch/pipermail/r-help/2005-July/076371.html}
#' @seealso \code{\link{prop.trend.test}}
#' 
#' \url{https://documentation.sas.com/doc/en/pgmsascdc/9.4_3.5/procstat/procstat_freq_details76.htm}
#' @references Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley
#' & Sons
#' 
#' @keywords htest
#' @examples
#' 
#' # http://www.lexjansen.com/pharmasug/2007/sp/sp05.pdf, pp. 4
#' dose <- matrix(c(10,9,10,7, 0,1,0,3), byrow=TRUE, nrow=2, 
#'                dimnames=list(resp=0:1, dose=0:3))
#' desc(dose)
#' 
#' cochranArmitageTest(dose)
#' cochranArmitageTest(dose, alternative="one.sided")
#' 
#' 
#' # not exactly the same as in package coin:
#' # independence_test(tumor ~ dose, data = lungtumor, teststat = "quad")
#' lungtumor <- data.frame(dose = rep(c(0, 1, 2), c(40, 50, 48)),
#'                         tumor = c(rep(c(0, 1), c(38, 2)),
#'                                   rep(c(0, 1), c(43, 7)),
#'                                   rep(c(0, 1), c(33, 15))))
#' tab <- table(lungtumor$dose, lungtumor$tumor)
#' cochranArmitageTest(tab)
#' 
#' # but similar to
#' prop.trend.test(tab[,1], apply(tab,1, sum))


#' @export
cochranArmitageTest <- function(x, alternative = c("two.sided","one.sided")) {
  
  # based on:
  # https://stat.ethz.ch/pipermail/r-help/2005-July/076371.html
  DNAME <- deparse(substitute(x))
  
  if (!(any(dim(x)==2)))
    stop("Cochran-Armitage test for trend must be used with rx2-table", call.=FALSE)
  
  if (dim(x)[2]!=2) x <- t(x)
  
  nidot <- apply(x, 1, sum)
  n <- sum(nidot)
  
  Ri <- .scores(x, 1, "table")
  
  Rbar <- sum(nidot*Ri)/n
  
  s2 <- sum(nidot*(Ri-Rbar)^2)
  pdot1 <- sum(x[,1])/n
  z <- sum(x[,1]*(Ri-Rbar))/sqrt(pdot1*(1-pdot1)*s2)
  STATISTIC <- z
  
  alternative <- match.arg(alternative)
  
  PVAL <- switch(alternative,
                 two.sided = 2*pnorm(abs(z), lower.tail=FALSE),
                 one.sided = 1- pnorm(abs(z))
  )
  
  PARAMETER <- dim(x)[1]
  names(STATISTIC) <- "Z"
  names(PARAMETER) <- "dim"
  
  METHOD <- "Cochran-Armitage test for trend"
  structure(list(statistic = STATISTIC, parameter = PARAMETER, alternative = alternative,
                 p.value = PVAL, method = METHOD, data.name = DNAME
  ), class = "htest")
  
}



# == internal helper functions =====================================

.scores <- function(x, MARGIN=1, method="table") { 
  
  # original by Eric Lecoutre
  # https://stat.ethz.ch/pipermail/r-help/2005-July/076371.html
  
  if (method == "table"){
    
    if (is.null(dimnames(x)) || 
         any(is.na(suppressWarnings(as.numeric(dimnames(x)[[MARGIN]]))))) {
      res <- 1:dim(x)[MARGIN]
    } else {
      res <- (as.numeric(dimnames(x)[[MARGIN]]))
    }
    
  } else	{
    ### method is a rank one
    Ndim <- dim(x)[MARGIN]
    OTHERMARGIN <- 3 - MARGIN
    
    ranks <- c(0, (cumsum(apply(x, MARGIN, sum))))[1:Ndim] + 
      (apply(x, MARGIN, sum)+1) /2 
    
    if (method == "ranks") res <- ranks
    if (method == "ridit") res <- ranks/(sum(x))
    if (method == "modridit") res <- ranks/(sum(x)+1)
  }
  
  return(res)
  
}

