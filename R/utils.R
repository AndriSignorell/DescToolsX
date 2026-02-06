

.recycle_to_ncol <- function(x, n, name = "") {
  
  if (length(x) == n)
    return(x)
  
  if (length(x) == 1L)
    return(rep(x, n))
  
  stop(
    sprintf(
      "Argument '%s' has length %d, but must be 1 or %d",
      name, length(x), n
    ),
    call. = FALSE
  )
}
