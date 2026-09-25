# Opens a null graphics device for the calling test and closes it on exit,
# so plot methods can be exercised without writing Rplots.pdf.
local_null_device <- function(.env = parent.frame()) {
  grDevices::pdf(NULL)
  dev <- grDevices::dev.cur()
  withr::defer(grDevices::dev.off(dev), envir = .env)
}

# Number of plot.new() calls while evaluating 'expr'. Counts frames, not
# plots: compare against a reference count, don't expect a literal.
count_plot_new <- function(expr) {
  n <- 0L
  old <- getHook("before.plot.new")
  setHook("before.plot.new", function() n <<- n + 1L, "replace")
  on.exit(setHook("before.plot.new", old, "replace"))
  force(expr)
  n
}
