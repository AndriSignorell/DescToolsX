
#' Compactly Display the Structure of any R Object 
#' 
#' Basically a wrapper for \code{\link{str}()}, extended with an enumeration
#' for the variables of a data.frame. 
#' 
#' 
#' @param x any \code{R} object about which you want to have some information.
#' @param \dots dots are passed to \code{\link{str}}. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{str}} 
#' @keywords utilities
#' @examples
#' 
#' strX(d.pizza)


 
#' @export
strX <- function(x, ...){
  
  if(identical(class(x), "data.frame") || identical(class(x), "list") ) {
    
    args <- list(...)
    if(is.null(args["strict.width"])) args["strict.width"] <- "cut"
    
    out <- .CaptOut(do.call(str, c(list(object=x), args)))
    idx <- format(1:length(grep(pattern="^ \\$", out)))
    i <- 1
    j <- 1
    while(i <= length(out)) {
      if( length(grep(pattern="^ \\$", out[i])) > 0 ) {
        out[i] <- gsub(pattern="^ \\$", replacement= paste(" ", idx[j], " \\$", sep=""), out[i])
        j <- j + 1
      }
      i <- i + 1
    }
    res <- out
  } else {
    res <- str(x, ...)
  }
  cat(res, sep="\n")
  invisible(res)
}

