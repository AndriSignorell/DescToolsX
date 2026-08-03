
# Guard against the class of errors that cost this release several check
# rounds: an argument name fm() does not know - nsmall, ldigits, justify,
# na.form, big.mark - lands in fm()'s '...' and aborts, but only in
# whatever branch happens to reach it. Two of the five occurrences sat in
# branches no example ever entered.
#
# WHAT IS SCANNED, AND WHY NOT THE SOURCES. Under R CMD check the tests run
# in <pkg>.Rcheck/tests, where there is no R/ and no man/ directory. A
# scanner reading source files would find nothing there and pass vacuously -
# worse than no test at all. This one walks the function bodies of the
# LOADED namespace and the Rd database of the INSTALLED package; both exist
# in every context the tests run in.

pkgName <- "DescToolsX"          # adjust per package


# argument names that are legal when calling fm() or a wrapper forwarding to it
fmKnown <- function() {
  nms <- c(names(formals(pharos::fm)),
           tryCatch(names(formals(pharos::fmCI)),
                    error = function(e) character(0)))
  setdiff(unique(nms), "...")
}


# pkg = NULL scans only 'extra', which is what the self-test below needs
scanCalls <- function(funs, known, pkg = pkgName, examples = TRUE,
                      extra = list()) {

  hits <- character(0)

  callName <- function(fn) {
    if (is.symbol(fn)) as.character(fn)
    else if (is.call(fn) &&
             as.character(fn[[1L]])[1L] %in% c("::", ":::")) as.character(fn[[3L]])
    else ""
  }

  # An empty argument slot - the gap in x[, 1] - is R's missing-arg object.
  # It must never be bound to a variable and handed on: the promise would
  # then be a plain symbol, and forcing it raises "argument is missing".
  # is.symbol() and as.character() are primitives and take the value
  # directly, so this test is safe.
  isEmptyArg <- function(z) is.symbol(z) && !nzchar(as.character(z))

  walk <- function(e, where) {
    if (!is.call(e)) return(invisible(NULL))
    if (callName(e[[1L]]) %in% funs) {
      nms <- names(e)
      nms <- if (is.null(nms)) character(0) else nms[nzchar(nms)]
      bad <- setdiff(nms, known)
      if (length(bad))
        hits <<- c(hits, sprintf("%s: %s", where, paste(bad, collapse = ", ")))
    }
    for (k in seq_along(e)) {
      if (isEmptyArg(e[[k]])) next
      walk(e[[k]], where)
    }
  }

  # 1) every function in the namespace, exported or not
  if (!is.null(pkg)) {
    ns <- asNamespace(pkg)
    for (nm in ls(ns, all.names = TRUE)) {
      obj <- tryCatch(get(nm, envir = ns, inherits = FALSE),
                      error = function(e) NULL)
      if (is.function(obj) && !is.null(body(obj)))
        walk(body(obj), nm)
    }
  }

  # 2) anything handed in directly
  for (nm in names(extra))
    if (is.function(extra[[nm]]) && !is.null(body(extra[[nm]])))
      walk(body(extra[[nm]]), nm)

  # 3) the examples: comments in the sources, therefore invisible to anything
  #    parsing R/ - but executed by R CMD check
  if (examples && !is.null(pkg)) {
    db <- tryCatch(tools::Rd_db(package = pkg), error = function(e) NULL)
    for (topic in names(db)) {
      tmp <- tempfile(fileext = ".R")
      ok <- tryCatch({ tools::Rd2ex(db[[topic]], out = tmp); TRUE },
                     error = function(e) FALSE)
      if (!ok || !file.exists(tmp)) next
      exprs <- tryCatch(parse(tmp), error = function(e) NULL)
      for (i in seq_along(exprs))
        walk(exprs[[i]], paste0(topic, " (examples)"))
      unlink(tmp)
    }
  }

  hits
}


test_that("the scan finds a planted wrong argument name", {

  # Without this the two tests below are worthless: a walker that visits
  # nothing reports nothing and passes. m[, 1] is deliberate - the empty
  # index is the construct that made an earlier version of the walker abort.
  skip_if_not_installed("pharos")

  probe <- function(m) fm(m[, 1], nsmall = 2)   # nsmall belongs to format()

  hits <- scanCalls(funs = c("fm", "fmCI"), known = fmKnown(),
                    pkg = NULL, extra = list(probe = probe))

  expect_length(hits, 1L)
  expect_match(hits, "nsmall")
})


test_that("no fm() call names an argument fm() does not know", {

  skip_on_cran()
  skip_if_not_installed("pharos")

  hits <- scanCalls(funs = c("fm", "fmCI"), known = fmKnown())

  expect_equal(hits, character(0),
               info = paste0("unknown fm() arguments:\n",
                             paste(hits, collapse = "\n")))
})


test_that("no style() call names an argument style() does not know", {

  skip_on_cran()
  skip_if_not_installed("pharos")

  hits <- scanCalls(funs = "style",
                    known = setdiff(names(formals(pharos::style)), "..."))

  expect_equal(hits, character(0),
               info = paste0("unknown style() arguments:\n",
                             paste(hits, collapse = "\n")))
})



# Every registered style is handed to fm() component by component, so a
# component fm() does not know aborts wherever that style is used - in
# print.Freq(), print.PercTable(), the whole desc family. The failure then
# names a formatting function, not the style that caused it.
#
# styles() collects from options() AND from .GlobalEnv, and this validates
# whatever it finds, from either source. Both matter and for different
# reasons:
#
#   * options() is the intended registration mechanism, and R CMD check runs
#     examples and tests without the user profile - a style registered in
#     ~/.Rprofile is therefore invisible to the check and shows up only in an
#     interactive run.
#   * .GlobalEnv is read as well, so a workspace object named abs.sty
#     silently overrides the package's own format.
#
# Runs first (file name sorts first) so the cause is named instead of
# surfacing later as an unexplained format error.

test_that("every registered style only carries names fm() knows", {
  
  skip_on_cran()
  skip_if_not_installed("pharos")
  
  known <- c(setdiff(names(formals(pharos::fm)), "..."),
             "label", "name")          # style metadata, see .styleMetaNames
  
  sty <- pharos::styles()
  
  bad <- vapply(sty, function(s)
    paste(setdiff(names(unclass(s)), known), collapse = ", "),
    character(1L))
  
  src <- vapply(sty, function(s) {
    z <- attr(s, "source")
    if (is.null(z)) NA_character_ else as.character(z)[1L]
  }, character(1L))
  
  offenders <- if (any(nzchar(bad)))
    paste(sprintf("  %s (%s): %s", names(bad)[nzchar(bad)],
                  src[nzchar(bad)], bad[nzchar(bad)]),
          collapse = "\n") else ""
  
  expect_equal(
    unname(bad[nzchar(bad)]), character(0),
    info = paste0("styles carrying components fm() does not know:\n",
                  offenders,
                  "\nCheck the registration site - options() set in the ",
                  "package, in ~/.Rprofile, or a Style object in .GlobalEnv."))
})
