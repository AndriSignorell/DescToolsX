

.desc_qq <- function(x, y) {
  desc(table(x, y))
}



#' @rdname Desc
#' @exportS3Method
print.Desc.qq <- function(x, digits = NULL, ...) {
  
  .printHeader(x$meta)
  
  cat(x$pair$strOut)

  print.Desc.table(x$res, print_header=FALSE, ...)

}

