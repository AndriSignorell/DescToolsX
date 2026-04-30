
.desc_qq <- function(x, y) {
  desc(table(x, y))
}


#' @rdname desc.table
#' @exportS3Method
print.Desc.qq <- function(x, digits = NULL, ...) {
  
  .printHeader(x$meta)
  
  cat(x$pair$strOut)

  print.Desc.table(x$res, print_header=FALSE, ...)

}



#' @exportS3Method
#' @rdname desc.table
plot.Desc.qq <- function(x, which = 1,  ...) {
  plot.Desc.table(x, which, ...)
}

