
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
#' HighLow combines the two upper functions and reports the k extreme values on
#' both sides together with their frequencies in parentheses. It is used for
#' describing univariate variables and is interesting for checking the ends of
#' the vector, where in real data often wrong values accumulate. This is in
#' essence a printing routine for the highest and the lowest values of x.
#' 
#' @name large_small
#' @aliases large small highLow
#' @param x a \code{numeric} vector 
#' @param k an integer >0 defining how many extreme values should be returned.
#' Default is \code{k = 5}. If \code{k > length(x)}, all values will be
#' returned. 
#' @param unique logical, defining if unique values should be considered or
#' not. If this is set to \code{TRUE}, a list with the \code{k} extreme values
#' and their frequencies is returned. Default is \code{FALSE} (as unique is a
#' rather expensive function). 
#' @param na.last for controlling the treatment of \code{NA}s.  If \code{TRUE},
#' missing values in the data are put last; if \code{FALSE}, they are put
#' first; if \code{NA}, they are removed.
#' @param nlow a single integer. The number of the smallest elements of a
#' vector to be printed. Defaults to 5.
#' @param nhigh a single integer. The number of the greatest elements of a
#' vector to be printed. Defaults to the number of \code{nlow}.
#' 
#' @return if \code{unique} is set to \code{FALSE}: a vector with the k most
#' extreme values, \cr else: a list, containing the k most extreme values and
#' their frequencies.
#' 
#' @author Andri Signorell <andri@@signorell.net>\cr C++ parts by Nathan
#' Russell and Romain Francois
#' @seealso \code{\link{max}}, \code{\link{min}}, \code{\link{sort}},
#' \code{\link{rank}}
#' @references
#' \url{https://stackoverflow.com/questions/36993935/find-the-largest-n-unique-values-and-their-frequencies-in-r-and-rcpp/}
#' 
#' \url{https://gallery.rcpp.org/articles/top-elements-from-vectors-using-priority-queue/}
#' @keywords arith
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
#' cat(highLow(d.pizza$temperature, na.last=NA))
#' 


#' @rdname large_small
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
    
    res <- top_n(x, k)
    
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
    
    # do not allow k be bigger than n
    k <- min(k, n)
    
    res <- x[top_i(x, k)]
    
    if(!is.na(na.last)){
      if(na.last==FALSE)
        res <- tail(c(rep(NA, na_n), res), k)
      if(na.last==TRUE)
        res <- tail(c(res, rep(NA, na_n)), k)
    }
    
  }
  
  return(res)
  
}



#' @rdname large_small
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
    
    res <- bottom_n(x, k)
    
    if(na_n > 0L){
      if(!is.na(na.last)){
        if(na.last==FALSE) {
          k <- min(length(res$value) + 1L, k)
          res$value <- c(NA, res$value)[1L:k]
          res$frequency <- c(na_n, res$frequency)[1L:k]
        }
        if(na.last==TRUE){
          k <- min(length(res$value) + 1L, k)
          res$value <- c(res$value, NA)[1L:k]
          res$frequency <- c(res$frequency, na_n)[1L:k]
        }
      }
    }
    if(is.factor(x))
      res$value <- levels(x)[res$value]
    else
      class(res$value) <- class(x)
    
  } else {
    
    # do not allow k be bigger than n
    k <- min(k, n)
    
    res <- rev(x[bottom_i(x, k)])
    
    if(!is.na(na.last)){
      if(na.last==FALSE)
        res <- c(rep(NA, na_n), res)[1L:k]
      if(na.last==TRUE)
        res <- c(res, rep(NA, na_n))[1L:k]
    }
    
  }
  
  return(res)
  
}




#' @rdname large_small
#' @export
highLow <- function (x, nlow = 5L, nhigh = nlow, na.last = NA) {
  
  # updated 1.2.2014 / Andri
  # using table() was unbearable slow and inefficient for big vectors!!
  # sort(partial) is the way to go..
  # http://r.789695.n4.nabble.com/Fast-way-of-finding-top-n-values-of-a-long-vector-td892565.html
  
  # updated 1.5.2016 / Andri
  # ... seemed the way to go so far, but now outperformed by nathan russell's C++ solution
  
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

