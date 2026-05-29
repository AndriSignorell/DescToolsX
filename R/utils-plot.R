



.withGraphicsState <- function(expr, stamp = .getOption("stamp", NULL), 
                               reset_layout = FALSE) {
  
  # nur "sichere" par-Parameter speichern
  keep <- c(
    "mar","mai","cex","cex.axis","cex.lab","cex.main","cex.sub",
    "las","tck","mgp","xaxs","yaxs","xaxt","yaxt",
    "col","col.axis","col.lab","col.main","col.sub",
    "lwd","lty","pch","bg","fg","xpd", "plt"
  )
  
  op <- par(keep)
  
  withr::defer(par(op))
  withr::local_options(warn = 1)
  
  ok <- FALSE
  
  on.exit({
    if (ok && !is.null(stamp))
      tryCatch(stamp(), error = function(e) NULL)
    if (ok && reset_layout)
      tryCatch(layout(matrix(1)), error = function(e) NULL)
  }, add = TRUE)
  
  eval.parent(substitute(expr))
  
  ok <- TRUE
  invisible(NULL)
}





# .resolvePar <- function(name, value = NULL, default = NULL) {
#   
#   if (!is.null(value)) {
#     return(value)
#   }
#   
#   opt <- .getOption(paste0("descToolsX.plot.", name))
#   if (!is.null(opt)) {
#     return(opt)
#   }
#   
#   default
# }
# 
# 
# 
# .isLastPanel <- function(tol = 1e-7) {
#   # returns TRUE if the current panel is the last on mfrow, layout screens
#   
#   if (par("page"))
#     return(FALSE)
#   
#   mfg <- par("mfg")
#   
#   if(length(mfg) == 4)
#     return(mfg[1] == mfg[3] && mfg[2] == mfg[4])
#   
#   # needed for split.screen or manuel set fig parameters
#   fig <- par("fig")
#   abs(fig[2] - 1) < tol && abs(fig[3] - 0) < tol
# }
# 


.applyParFromDots <- function(...) {
  
  # if some defaults for dots required use:
  # do.call(.applyParFromDots, 
  #         mergeArgs(defaults=list(
  #           yaxs="r"), 
  #           list(...)
  #         ))
  
  
  patch_fourpar <- function(new_val, old_val, pname) {
    
    if (!is.null(names(new_val))) {
      
      idx <- match(names(new_val),
                   c("bottom","left","top","right"))
      
      if (any(is.na(idx)))
        stop(sprintf("%s names must be bottom, left, top, right", pname))
      
      old_val[idx] <- new_val
      return(old_val)
    }
    
    new_val <- rep_len(new_val, 4)
    idx_na <- is.na(new_val)
    new_val[idx_na] <- old_val[idx_na]
    
    new_val
  }
  
  
  dots <- list(...)
  if (!length(dots)) return(invisible())
  
  dots <- dots[!is.na(names(dots))]
  dots <- dots[names(dots) %in% names(par(no.readonly = TRUE))]
  
  if (!length(dots)) return(invisible())
  
  p <- par(no.readonly = TRUE)
  
  
  if ("mar" %in% names(dots)) {
    dots$mar <- patch_fourpar(dots$mar, p$mar, "mar")
  }
  
  if ("oma" %in% names(dots)) {
    dots$oma <- patch_fourpar(dots$oma, p$oma, "oma")
  }
  
  do.call(par, dots)
  
  invisible()
}


