
# Checks whether the shared C++ headers in lumen and DescToolsX
# are byte-for-byte identical. 
#
# Run from one of the two package root directories, or with
# customized paths.

syncCheck <- function(lumen      = "c:/temp/lumen/",
                      desctoolsx = "c:/temp/DescToolsX/",
                      files      = c("src/boot_framework.h", 
                                     "src/bca_helpers.h", 
                                     "r/extractBootArgs.R")) {

  paths <- file.path(rep(c(lumen, desctoolsx), each = length(files)), files)
  miss  <- paths[!file.exists(paths)]
  if (length(miss))
    stop("nicht gefunden: ", paste(miss, collapse = ", "))

  md5 <- unname(tools::md5sum(paths))
  dim(md5) <- c(length(files), 2L)

  out <- data.frame(file  = files,
                    lumen = md5[, 1],
                    dtx   = md5[, 2],
                    equal = md5[, 1] == md5[, 2],
                    stringsAsFactors = FALSE)

  if (!all(out$equal))
    warning("Diese Dateien laufen auseinander: ",
            paste(out$file[!out$equal], collapse = ", "))

  out
}

#  syncCheck()

