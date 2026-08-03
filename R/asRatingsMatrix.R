
# Shared input helper for the interrater-agreement functions
# (percAgreement, randolphKappa, kappaM, ...).
#
# Own file on purpose: a helper used by several files must not be defined
# twice at top level -- R keeps whichever definition is collated last, without
# a warning from R CMD check (cf. the .nctCI() collision in coefVar.R /
# glassDelta.R).

# as.matrix() on a data frame with mixed or non-character columns runs every
# column through format(), which right-pads to a common width *within* the
# column. Two raters coding 1..10 and 1..9 then produce " 1" and "1" for the
# same rating and count as a disagreement. Convert column-wise instead.
.asRatingsMatrix <- function(x) {
  
  if (is.data.frame(x) || is.list(x)) {
    
    if (!length(x))
      stop("ratings must contain at least two raters")
    
    lens <- lengths(x)
    
    if (length(unique(lens)) != 1L)
      stop("all rating vectors must have equal length")
    
    numericInput <- all(vapply(x, is.numeric, logical(1L)))
    
    if (numericInput) {
      x <- do.call(cbind, x)
    } else {
      x <- do.call(
        cbind,
        lapply(x, function(col)
          if (is.character(col)) col else as.character(col))
      )
    }
  }
  
  if (is.null(dim(x)) || length(dim(x)) != 2L)
    stop("ratings must be given as a matrix or data frame with subjects in rows and raters in columns")
  
  if (ncol(x) < 2L)
    stop("ratings must contain at least two raters")
  
  x
}