

# Recycle <- function(...){
#   
#   lst <- list(...)
#   
#   # optimization suggestion by moodymudskipper 20.11.2019  
#   maxdim <- max(lengths(lst)) # instead of max(unlist(lapply(lst, length)))
#   # recycle all params to maxdim
#   # res <- lapply(lst, rep_len, length.out=maxdim)
#   
#   # rep_len would not work for Dates
#   res <- lapply(lst, rep, length.out=maxdim)
#   
#   attr(res, "maxdim") <- maxdim
#   
#   return(res)
#   
# }
# 




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
    else  paste(substring(fill, 1, free %/% 2), x, substring(fill, 1 + free %/% 2, free), sep = "")
  }
  
  # adj <- sapply(adj, match.arg, choices=c("left", "right", "center"))
  
  if(is.null(width)) width <- max(nchar(x), na.rm=TRUE)
  
  lgp <- recycle(x=x, width=width, pad=pad, adj=adj)
  sapply( 1:attr(lgp, "maxdim"), function(i) .pad(lgp$x[i], lgp$width[i], lgp$pad[i], lgp$adj[i]) )
  
}




