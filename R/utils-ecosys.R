

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


#' @keywords internal
.checkVerbose <- function(verbose = NULL){
  
  # resolve: arg > option > default
  verbose <- if(!is.null(verbose)) {
    verbose
  } else {
    getOption("DescTools.verbose", 2L)
  }
  
  # validation
  if(length(verbose) != 1 || !is.numeric(verbose) || !(verbose %in% 1:3)){
    stop("verbose must be a single integer: 1 (minimal), 2 (standard), or 3 (detailed).")
  }
  
  as.integer(verbose)
}




## ============================================================
## Argument handling utilities (centralized, reusable)
## ============================================================

.extractBootArgs <- function(dots) {
  
  bedrock::extractArgs(
    dots,
    defaults = list(
      type     = "bca",
      R        = 999,
      parallel = "no",
      ncpus    = getOption("boot.ncpus", 1L)
    ),
    validate = function(x) {
      
      if (!is.numeric(x$R) || length(x$R) != 1 || x$R <= 0)
        stop("R must be a positive integer")
      
      if (!is.numeric(x$ncpus) || x$ncpus < 1)
        stop("ncpus must be >= 1")
      
      if (!x$parallel %in% c("no", "multicore", "snow"))
        stop("parallel must be 'no', 'multicore', or 'snow'")
    }
  )
}


 
# ## ============================================================
# ## Example usage in gini()
# ## ============================================================
# 
# # inside gini():
# dots <- list(...)
# boot_args <- .extractBootArgs(dots)
# 
# boot.fun <- boot::boot(
#   data = x,
#   statistic = function(z, i, u, unbiased)
#     i.gini(z[i], u[i], unbiased),
#   R = boot_args$R,
#   u = weights,
#   unbiased = unbiased,
#   parallel = boot_args$parallel,
#   ncpus = boot_args$ncpus
# )
# 
# ci <- boot::boot.ci(
#   boot.fun,
#   conf = conf.level,
#   type = boot_args$type
# )
# 

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
