
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
  out <- capture.output(printCharMatrix(m, showRownames = FALSE))
  
  cat(out, sep = "\n")
  
  .plotIfRequested(x)
}


# ── plot.Desc.AllNA ──────────────────────────────────────────────────────────
#' @rdname desc
#' @export
plot.Desc.AllNA <- function(x, ...) {
  canvas()

  # The shrug's U+30C4 (katakana "tsu") is fine on screen devices and
  # cairo_pdf(), but pdf() and postscript() encode text in a single-byte
  # font encoding, where it does not exist: text() stopped with
  # "conversion failure ... in 'mbcsToSbcs'". Those devices get a shrug
  # made of Latin-1 characters instead: a diaeresis over a slash, which
  # is what the tsu looks like (two strokes and a curve). Written as
  # \u escapes, so the source file stays ASCII for R CMD check.
  singleByteDevice <- names(grDevices::dev.cur()) %in%
    c("pdf", "postscript", "xfig", "pictex")

  face <- if (singleByteDevice) "\u00a8/" else "\u30c4"

  text(0, 0,
       labels = paste0("Nothing to plot!\n\n\u00af\\_(", face, ")_/\u00af"),
       cex    = 4,
       col    = fade(pal(n = NA)[5]),
       font   = 2,
       xpd    = NA)
  invisible(x)
}
