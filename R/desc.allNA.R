
# ── .descAllNA ───────────────────────────────────────────────────────────────
# Internal constructor for all-NA / zero-length vectors.
# Called at the top of every desc.* method after n is known to be 0.
#
# Downstream check: isTRUE(inherits(x, "Desc.AllNA"))
#
.descAllNA <- function(x, xname, main, plotit, verbose) {
  total_n <- length(x)
  res <- list(
    meta   = .descMeta(x, xname, main, plotit, verbose),
    length = total_n,
    n      = 0L,
    NAs    = total_n,
    main   = main
  )
  class(res) <- c("Desc.AllNA", "Desc")
  res
}


# ── print.Desc.AllNA ─────────────────────────────────────────────────────────
#' @rdname desc
#' @export
print.Desc.AllNA <- function(x, ...) {
  
  .printHeader(x$meta)
  
  nperc  <- fm(0,              fmt = "%", digits = 1)
  naperc <- fm(1,              fmt = "%", digits = 1)
  len    <- fm(x$length, fmt = style("abs.sty"))
  n      <- fm(0L,       fmt = style("abs.sty"))
  nas    <- fm(x$NAs,    fmt = style("abs.sty"))
  
  lst <- list(
    # l1 = c(length = len, n = n, NAs = nas, unique = "NA"),
    l1 = c(length = len, n = n, NAs = nas),
    l2 = c("", nperc, naperc)
  )
  
  width <- max(c(
    unlist(lapply(lst, nchar)),
    unlist(lapply(lapply(lst, names), nchar))
  ), na.rm = TRUE)
  
  m <- rbind(lst$l1, lst$l2, "")
  out <- capture.output(.print.charmatrix(m))
  cat(out, sep = "\n")
  
  if (x$meta$plotit)
    plot(x)
  
  invisible(x)
}


# ── plot.Desc.AllNA ──────────────────────────────────────────────────────────
#' @rdname desc
#' @export
plot.Desc.AllNA <- function(x, ...) {
  canvas()
  text(0, 0,
       labels = "Nothing to plot!\n\n\u00af\\_(\u30c4)_/\u00af",
       cex    = 4,
       col    = fade(pal(n = NA)[5]),
       font   = 2,
       xpd    = NA)
  invisible(x)
}
