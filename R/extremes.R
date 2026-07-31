
#' Kth Smallest/Largest Values 
#' 
#' Find the kth smallest, resp. largest values from a vector \code{x} and
#' return the values and their frequencies. 
#' 
#' This does not seem to be a difficult problem at first sight. We could simply
#' tabulate and sort the vector and finally take the first or last k values.
#' However sorting and tabulating the whole vector when we're just interested
#' in the few smallest values is a considerable waste of resources. This
#' approach becomes already impracticable for medium vector lengths
#' (~\ifelse{html}{10\out{<sup>5</sup>}}{\eqn{10^5}}). There are several points and
#' solutions of this problem discussed out there. The present implementation is
#' based on highly efficient C++ code and proved to be very fast.
#' 
#' \code{highLow()} combines the two upper functions and reports the k extreme values on
#' both sides together with their frequencies in parentheses. It is used for
#' describing univariate variables and is interesting for checking the ends of
#' the vector, where in real data often wrong values accumulate. In
#' essence this is nothing more than a combine printing routine for \code{large()}
#' and \code{small()}.
#' 
#' @name extremes
#' @aliases large small highLow
#' 
#' @param x a numeric vector
#' @param k a positive integer defining how many extreme values are returned.
#' Default is \code{k = 5}. If \code{k > length(x)}, all values will be
#' returned. 
#' @param unique logical, defining if unique values should be considered or
#' not. If this is set to \code{TRUE}, a list with the \code{k} extreme values
#' and their frequencies is returned. Default is \code{FALSE} (as unique is a
#' rather expensive function). 
#' @param na.last for controlling the treatment of \code{NA}s.  If \code{TRUE},
#' missing values in the data are put last; if \code{FALSE}, they are put
#' first; if \code{NA}, they are removed.
#' @param nlow number of smallest values included in the formatted output;
#' defaults to 5
#' @param nhigh number of largest values included in the formatted output;
#' defaults to \code{nlow}
#' 
#' @return for \code{large()} and \code{small()}, a vector of extreme values
#' when \code{unique = FALSE}, otherwise a list with components:
#' \describe{
#'   \item{\code{value}}{extreme values}
#'   \item{\code{frequency}}{corresponding frequencies}
#' }
#' \code{highLow()} returns a character scalar containing formatted lowest and
#' highest values with frequencies.
#' 
#' @note Based on C++ code by Nathan Russell and Romain Francois, 
#' adapted to conform to package standards.  
#' 
#' @seealso \code{\link{max}}, \code{\link{min}}, \code{\link{sort}},
#' \code{\link{rank}}
#' 
#' @references
#' \href{https://stackoverflow.com/questions/36993935}{StackOverflow: Largest n unique values}
#' 
#' \href{https://gallery.rcpp.org/articles/top-elements-from-vectors-using-priority-queue/}{Rcpp Gallery article}
#' 
#' @examples
#' 
#' x <- sample(1:10, 1000, rep=TRUE)
#' large(x, 3)
#' large(x, k=3, unique=TRUE)
#' 
#' # works fine up to x ~ 1e6
#' x <- runif(1000000)
#' small(x, 3, unique=TRUE)
#' small(x, 3, unique=FALSE)
#' 
#' # Both ends
#' cat(highLow(bedrock::Pizza$temperature, na.last=NA))
#' 
#'  
#' @family quantile
#' @concept order-statistic
#' @concept distribution-summary
#' @rdname extremes
#' @export
large <- function (x, k = 5L, unique = FALSE, na.last = NA) {
  
  n <- length(x)
  x <- x[!is.na(x)]
  na_n <- n - length(x)
  
  #  na.last
  #  for controlling the treatment of NAs. If TRUE, missing values in the data are put last;
  #  if FALSE, they are put first;
  #  if NA, they are removed.
  
  if (unique==TRUE) {
    
    res <- top_n_cpp(x, k)
    
    if(na_n > 0){
      if(!is.na(na.last)){
        if(na.last==FALSE) {
          res$value <- tail(c(NA, res$value), k)
          res$frequency <- tail(c(na_n, res$frequency), k)
        }
        if(na.last==TRUE){
          res$value <- tail(c(res$value, NA), k)
          res$frequency <- tail(c(res$frequency, na_n), k)
        }
      }
    }
    
    if(is.factor(x))
      res$value <- levels(x)[res$value]
    else
      class(res$value) <- class(x)
    
  } else {
    
    # cap k by the number of NON-MISSING values: x has just been stripped
    # of its NAs, so min(k, n) with the original length let k exceed
    # length(x) and top_i_cpp() then read past the end of the vector
    k <- min(k, length(x))
    
    res <- x[top_i_cpp(x, k)]
    
    if(!is.na(na.last)){
      if(na.last==FALSE)
        res <- tail(c(rep(NA, na_n), res), k)
      if(na.last==TRUE)
        res <- tail(c(res, rep(NA, na_n)), k)
    }
    
  }
  
  return(res)
  
}



#' @rdname extremes
#' @export
small <- function (x, k = 5L, unique = FALSE, na.last = NA) {
  
  n <- length(x)
  x <- x[!is.na(x)]
  na_n <- n - length(x)
  
  #  na.last
  #  for controlling the treatment of NAs. If TRUE, missing values in the data are put last;
  #  if FALSE, they are put first;
  #  if NA, they are removed.
  
  if (unique==TRUE) {
    
    res <- bottom_n_cpp(x, k)
    
    if(na_n > 0L){
      if(!is.na(na.last)){
        if(na.last==FALSE) {
          k <- min(length(res$value) + 1L, k)
          res$value <- c(NA, res$value)[seq_len(k)]
          res$frequency <- c(na_n, res$frequency)[seq_len(k)]
        }
        if(na.last==TRUE){
          k <- min(length(res$value) + 1L, k)
          res$value <- c(res$value, NA)[seq_len(k)]
          res$frequency <- c(res$frequency, na_n)[seq_len(k)]
        }
      }
    }
    if(is.factor(x))
      res$value <- levels(x)[res$value]
    else
      class(res$value) <- class(x)
    
  } else {
    
    # see large(): cap by the stripped length, not the original one
    k <- min(k, length(x))
    
    res <- rev(x[bottom_i_cpp(x, k)])
    
    if(!is.na(na.last) && k > 0L){
      if(na.last==FALSE)
        res <- c(rep(NA, na_n), res)[seq_len(k)]
      if(na.last==TRUE)
        res <- c(res, rep(NA, na_n))[seq_len(k)]
    }
    
  }
  
  return(res)
  
}




#' @rdname extremes
#' @export
highLow <- function (x, nlow = 5L, nhigh = nlow, na.last = NA) {
  
  # updated 1.2.2014 / Andri
  # using table() was unbearable slow and inefficient for big vectors!!
  
  # sort(partial) is the way to go..
  # http://r.789695.n4.nabble.com/Fast-way-of-finding-top-n-values-of-a-long-vector-td892565.html
  
  # updated 1.5.2016 / Andri
  # ... approach above seemed the way to go so far, but now significantly 
  # outperformed by nathan russell's C++ solution
  
  
  if ((nlow + nhigh) != 0L) {
    frqs <- small(x, k=nlow, unique=TRUE, na.last=na.last)
    frql <- large(x, k=nhigh, unique=TRUE, na.last=na.last)
    frq <- c(frqs$frequency, frql$frequency)
    
    vals <- c(frqs$value, frql$value)
    if (is.numeric(x)) {
      vals <- prettyNum(vals, big.mark = "'")
    }
    else {
      vals <- vals
    }
    frqtxt <- paste(" (", frq, ")", sep = "")
    frqtxt[frq < 2L] <- ""
    
    txt <- strTrim(paste(vals, frqtxt, sep = ""))
    lowtxt <- paste(head(txt, min(length(frqs$frequency), nlow)), collapse = ", ")
    hightxt <- paste(tail(txt, min(length(frql$frequency), nhigh)), collapse = ", ")
  }
  else {
    lowtxt <- ""
    hightxt <- ""
  }
  return(paste("lowest : ", lowtxt, "\n",
               "highest: ", hightxt, "\n", sep = ""))
}
