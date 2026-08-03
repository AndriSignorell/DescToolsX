

# internal utilities for the DescToolsX ecosystem
#    ( to be used in every package, where needed, 
#      copy and update when edited! )


# Check and resolve verbose level
#
# @description
# Resolves verbosity level using the following priority:
# \itemize{
#   \item function argument
#   \item global option \code{DescTools.verbose}
#   \item default (2)
# }
#
# @param verbose Optional integer (1–3).
#
# @return Integer in {1,2,3}.


.checkVerbose <- function(verbose = NULL){
  
  # resolve: arg > option > default
  verbose <- if(!is.null(verbose)) {
    verbose
  } else {
    getOption("DescToolsX.verbose", 2L)
  }
  
  # validation
  if(length(verbose) != 1 || !is.numeric(verbose) || !(verbose %in% 1:3)){
    stop("verbose must be a single integer: 1 (minimal), 2 (standard), or 3 (detailed).")
  }
  
  as.integer(verbose)
}




 
## ============================================================
## CONSOLIDATION CHECKLIST (run across package)
## ============================================================

# Replace patterns like:
# inDots(..., arg="type", default="bca")
# inDots(..., arg="R", default=999)
# inDots(..., arg="parallel", default="no")
# inDots(..., arg="ncpus", default=...)

# With:
# boot_args <- .extractBootArgs(list(...))

# Search targets:
# grep -R "inDots" .
# grep -R "type =" .
# grep -R "R =" .
# grep -R "parallel =" .
# grep -R "ncpus =" .

# Goal:
# unify ALL bootstrap argument handling via .extractBootArgs()

## ============================================================
## END
## ============================================================
