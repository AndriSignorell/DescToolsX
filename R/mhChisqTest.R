
#' Mantel-Haenszel Chi-Square Test 
#' 
#' The Mantel-Haenszel chi-square statistic tests the alternative hypothesis
#' that there is a linear association between the row variable and the column
#' variable. Both variables must lie on an ordinal scale. 
#' 
#' The statistic is computed as \eqn{ Q_{MH} = (n-1) \cdot r^2}, where
#' \eqn{r^2} is the Pearson correlation between the row variable and the column
#' variable. The Mantel-Haenszel chi-square statistic use the scores specified
#' by srow and scol. Under the null hypothesis of no association, \eqn{Q_{MH}}
#' has an asymptotic chi-square distribution with one degree of freedom.
#' 
#' 
#' @param x a frequency table or a matrix. 
#' @param srow scores for the row variable, defaults to 1:nrow. 
#' @param scol scores for the colummn variable, defaults to 1:ncol. 
#' @return A list with class \code{"htest"} containing the following
#' components: \item{statistic}{the value the Mantel-Haenszel chi-squared test
#' statistic.} \item{parameter}{the degrees of freedom of the approximate
#' chi-squared distribution of the test statistic.} \item{p.value}{the p-value
#' for the test.} \item{method}{a character string indicating the type of test
#' performed.} \item{data.name}{a character string giving the name(s) of the
#' data.}
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{chisq.test}}, for calculating correlation of a table:
#' \code{\link[boot]{corr}} 
#' 
#' @references Agresti, A. (2002) \emph{Categorical Data Analysis}. John Wiley
#' & Sons, pp 86 ff. %% ~put references to the literature/web site here ~
#' @keywords htest
#' @examples
#' 
#' ## A r x c table  Agresti (2002, p. 57) Job Satisfaction
#' Job <- matrix(c(1,2,1,0, 3,3,6,1, 10,10,14,9, 6,7,12,11), 4, 4,
#'               dimnames = list(income = c("< 15k", "15-25k", "25-40k", "> 40k"),
#'                               satisfaction = c("VeryD", "LittleD", "ModerateS", "VeryS"))
#'        )
#' 
#' mhChisqTest(Job, srow=c(7.5,20,32.5,60))
#'


#'@export 
mhChisqTest <- function(x, srow=1:nrow(x), scol=1:ncol(x)){
  
  # calculates Mantel-Haenszel Chisquare test
  
  # check for rxc 2-dim matrix
  p <- (d <- dim(x))[1L]
  if(!is.numeric(x) || length(d) != 2L)
    stop("'x' is not a rxc numeric matrix")
  
  DNAME <- deparse(substitute(x))
  
# check if ok  ****************
  #  STATISTIC <- (sum(x) - 1) * corX(d=combPairs(srow, scol), as.vector(x))^2
  STATISTIC <- (sum(x) - 1) * pearsonCor(x)^2
  PARAMETER <- 1
  names(STATISTIC) <- "X-squared"
  names(PARAMETER) <- "df"
  PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  METHOD <- "Mantel-Haenszel Chi-Square"
  
  structure(list(statistic = STATISTIC, parameter = PARAMETER,
                 p.value = PVAL, method = METHOD, data.name = DNAME), 
            class = "htest")
}

