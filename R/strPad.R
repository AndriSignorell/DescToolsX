
#' Pad a String With Justification
#' 
#' \code{strPad} will fill a string x with defined characters to fit a given
#' length.
#' 
#' If a string x has more characters than width, it will be chopped on the
#' length of width.
#' 
#' @param x a vector of strings to be padded.
#' @param width resulting width of padded string. If x is a vector and width is
#' left to NULL, it will be set to the length of the largest string in x.
#' @param pad string to pad with. Will be repeated as often as necessary.
#' Default is " ".
#' @param adj adjustement of the old string, one of \code{"left"},
#' \code{"right"}, \code{"center"}. If set to \code{"left"} the old string will
#' be adjusted on the left and the new characters will be filled in on the
#' right side.
#' @return the string
#' @author Christian W. Hoffmann <c-w.hoffmann@@sunrise.ch>\cr some extensions
#' Andri Signorell <andri@@signorell.net>
#' @keywords character
#' @examples
#' 
#' strPad("My string", 25, "XoX", "center")
#'  # [1] "XoXXoXXoMy stringXXoXXoXX"
#' 


#' @export
strPad <- function(x, width = NULL, pad = " ", adj = "left") {
  
  .pad <- function(x, width, pad=" ", adj="left"){
    
    if(is.na(x)) return(NA)
    
    mto <- match.arg(adj, c("left", "right", "center"))
    free <- max(0, width - nchar(x))
    fill <- substring(paste(rep(pad, ceiling(free / nchar(pad))), collapse = ""), 1, free)
    #### cat("  free=",free,",  fill=",fill,",  mto=",mto,"\n")
    # old, but chop is not a good idea:  if(free <= 0) substr(x, 1, len)
    if(free <= 0) x
    else if  (mto == "left") paste(x, fill, sep = "")
    else if  (mto == "right") paste(fill, x, sep = "")
    else  paste(substring(fill, 1, free %/% 2), x, 
                substring(fill, 1 + free %/% 2, free), sep = "")
  }
  
  # adj <- sapply(adj, match.arg, choices=c("left", "right", "center"))
  
  if(is.null(width)) width <- max(nchar(x), na.rm=TRUE)
  
  lgp <- recycle(x=x, width=width, pad=pad, adj=adj)
  sapply( 1:attr(lgp, "maxdim"), function(i) .pad(lgp$x[i], lgp$width[i], 
                                                  lgp$pad[i], lgp$adj[i]) )
  
}
