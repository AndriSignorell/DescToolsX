
#' Replacing NAs by Specific Values or Vice Versa
#' 
#' \code{zeroIfNA()} replaces \code{NA}s in a numeric vector \code{x} with 0.
#' This function has the same logic as the \code{ZEROIFNULL()} function in SQL.
#' For values other than zero the function \code{NAVal()} can be used, which
#' replaces the \code{NA} values by a given value.  \code{NAIfZero()} replaces
#' zeros with \code{NA}. \code{NAIfBlank()} does the same, but for character
#' vectors.  #' 
#' 
#' @name na_handling
#' @aliases zeroIfNA naIfZero naIfBlank naVal naIf nz impute
#' @param x the vector x, whose \code{NA}s should be overwritten with 0s.
#' @param val the value \code{NA}s should be replaced by.  Default is an empty
#' string (\code{""}).
#' @param what a vector of elements to be replaced with \code{NA} in \code{x}.
#' @param FUN the name of a function to be used as imputation. Can as well be a
#' self defined function or a constant value.  Default is \code{\link{median}}.
#' @return the edited vector x
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{replace}}
#' @keywords manip
#' @examples
#' 
#' z <- c(8, NA, 9, NA, 3)
#' 
#' zeroIfNA(z)
#' # [1] 8 0 9 0 3
#' 
#' # set 8 and 9 to NA
#' naIf(zeroIfNA(z), what=c(8, 9))
#' 
#' 
#' # Impute(z)
#' # [1] 8 8 9 8 3
#' 
#' 
#' z <- c("a", NA, "", NA, "k")
#' 
#' naVal(z)
#' # [1] "a" "" "" "" "k"
#' 
#' 


#' @rdname na_handling
#' @export
zeroIfNA <- function(x) {
  #  same as zeroifnull in SQL
  replace(x, is.na(x), 0L)
}

#' @rdname na_handling
#' @export
naIfZero <- function(x)
  replace(x, isZero(x), NA)



#' @rdname na_handling
#' @export
naIf <- function (x, what) {
  x[!is.na(match(x, what))] <- NA
  return(x)
} 


#' @rdname na_handling
#' @export
naVal <- function(x, val=""){
  # same as NVL() in SQL
  replace(x, is.na(x), val)
}


#' @rdname na_handling
#' @export
naIfBlank <- function(x)
  replace(x, x=="", NA)


#' @rdname na_handling
#' @export
nz <- function(x){
  # return non-zero elements of x
  x[ !isZero(x) ]
}


#' @rdname na_handling
#' @export
impute <- function(x, FUN = function(x) median(x, na.rm=TRUE)) {
  
  if(is.function(FUN)) {
    #  if FUN is a function, then save it under new name and
    # overwrite function name in FUN, which has to be character
    fct <- FUN
    FUN <- "fct"
    FUN <- gettextf("%s(x)", FUN)
  }
  # Calculates the mean absolute deviation from the sample mean.
  return(eval(parse(text = gettextf("replace(x, is.na(x), %s)", FUN))))
  
}



