
#' Frequency Table for a Single Variable
#' 
#' Calculates absolute and relative frequencies of a vector \code{x}.
#' Continuous (numeric) variables will be cut using the same logic as used by
#' the function \code{\link{hist}}. Categorical variables will be aggregated by
#' \code{\link{table}}. The result will contain single and cumulative
#' frequencies for both, absolute values and percentages.
#' 
#' By default only the valid cases are considered for the frequencies, say
#' \code{NA} values are excluded. (This is in accordance with the default
#' behavior of the R function \code{table}, which seemed a reasonable
#' reference.) If the \code{NA}s should be included you can set the
#' \code{useNA} argument to either \code{"ifany"} or \code{"always"}.
#' 
#' For numeric variables, if \code{breaks} is specified as a single number, the
#' range of the data is divided into breaks pieces of equal length, and then
#' the outer limits are moved away by 0.1\% of the range to ensure that the
#' extreme values both fall within the break intervals. (If \code{x} is a
#' constant vector, equal-length intervals are created that cover the single
#' value.) See \code{\link{cut}}.
#' 
#' @name FreqTable
#' @aliases Freq print.Freq
#' @param x the variable to be described, can be any atomic type.
#' @param breaks either a numeric vector of two or more cut points or a single
#' number (greater than or equal to 2) giving the number of intervals into
#' which x is to be cut. Default taken from the function \code{hist()}.  This
#' is ignored if x is not of numeric type.
#' @param include.lowest logical, indicating if an \verb{x[i]} equal to the lowest (or
#' highest, for \code{right = FALSE}) \code{"breaks"} value should be included.
#' Ignored if x is not of numeric type.
#' @param ord how should the result be ordered? Default is \code{"level"},
#' other choices are 'by frequency' (\code{"descending"} or \code{"ascending"})
#' or 'by name of the levels' (\code{"name"}). The argument can be abbreviated.
#' This is ignored if x is numeric.
#' @param useNA one out of \code{"no"}, \code{"ifany"}, \code{"always"}.
#' Defines whether to include extra \code{NA} levels in the table.  Defaults to
#' \code{"no"} which is the \code{\link{table}()} default too.
#' @param digits integer, determining the number of digits used to format the
#' relative frequencies.
#' @param \dots further arguments are passed to the function
#' \code{\link{cut}()}. Use \code{dig.lab} to control the format of numeric
#' group names. Use the argument \code{right} to define if the intervals should
#' be closed on the right (and open on the left) or vice versa. \cr In
#' \code{print.Freq} the dots are not used.
#' @return an object of type \code{"Freq"}, which is basically a data.frame
#' with 5 columns (earning a specific print routine), containing the following
#' components: \item{level}{ factor. The levels of the grouping variable.}
#' \item{freq}{ integer. The absolute frequencies. } \item{perc}{ numeric. The
#' relative frequencies (percent).} \item{cumfreq}{integer. The cumulative sum
#' of the absolute frequencies.} \item{cumperc}{numeric. The cumulative sum of
#' the relative frequencies.}
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{cut}}, \code{\link{hist}}, \code{\link{cumsum}},
#' \code{\link{table}}, \code{\link{prop.table}}, \code{\link{PercTable}},
#' \code{\link{freq2D}}
#' @keywords univar
#' @examples
#' 
#' data(d.pizza)
#' 
#' # result is a data.frame
#' d.freq <- Freq(d.pizza$price)
#' d.freq
#' 
#' # it is printed by default with 3 digits for the percent values,
#' # but the number of digits can be defined in the print function
#' print(d.freq, digits=5)
#' 
#' # sorted by frequency
#' Freq(d.pizza$driver, ord="desc")
#' 
#' # sorted by name using all the observations, say including NAs
#' Freq(d.pizza$driver, ord="name", useNA="ifany")
#' 
#' # percentages and cumulative frequencies for a vector of count data
#' Freq(as.table(c(2,4,12,8)))
#' 



#' @export
#' @rdname FreqTable
Freq <- function(x, breaks = hist(x, plot = FALSE)$breaks, include.lowest = TRUE,
                 ord = c("level", "desc", "asc", "name"),
                 useNA = c("no", "ifany", "always"), ...){
  
  # check if x is a vector (do not use is.vector()!!!)
  if(!(is.atomic(x) || is.list(x))) stop("'x' must be a vector")
  
  if(inherits(x, "table")){
    tab <- x
    
  } else {
    
    if(is.numeric(x) || isDate(x)){
      x <- cut(x, breaks = breaks, include.lowest = include.lowest,
               ordered_result = TRUE, ...)
    }
    
    tab <- table(x, useNA = useNA)
  }
  
  names(tab)[is.na(names(tab))] <- "<NA>"
  
  # how should the table be sorted, by name, level or frq? (NULL means "desc")
  switch(match.arg(ord, c("level", "desc", "asc", "name")),
         level  = {  }
         , name   = { tab <- tab[rownames(tab)] }
         , asc    = { tab <- sort(tab) }
         , desc   = { tab <- -sort(-tab) }
  )
  
  ptab <- prop.table(tab)
  
  z <- data.frame(level = names(tab),
                  freq = as.vector(tab[]), perc = as.vector(ptab[]),
                  cumfreq = cumsum(tab[]), cumperc = cumsum(ptab[]))
  
  # the first class beyond 50% cumulative percentages is the median class
  attr(z, which = "medianclass") <- z[which(z[, 5]>=0.5)[1], 1]
  
  rownames(z) <- NULL # enumerate from 1:nrow(z)
  class(z) <- c("Freq", "data.frame")
  return(z)
  
}



#' @rdname FreqTable
#' @method print Freq
#' @export
print.Freq <- function(x, digits=NULL, ...) {
  
  # print as data.frame if something was changed
  if(ncol(x) != 5) {
    print.data.frame(x, digits=digits, ...)
  } else {
    
    afmt <- style("abs.sty")
    pfmt <- style("per.sty")
    if(!is.null(digits))
      pfmt$digits <- digits
    
    # object x comes as list lacking an as.data.frame option...
    d.frm <- setNamesX(data.frame(x[,1],
                                 fm(x$freq, fmt=afmt),
                                 fm(x$perc, fmt=pfmt),
                                 fm(x$cumfreq, fmt=afmt),
                                 fm(x$cumperc, fmt=pfmt)),
                      colnames=names(x))
    
    print(d.frm,
          print.gap = inDots(..., arg="print.gap", default=2))
  }
}

