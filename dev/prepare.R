# ***************************************************************************
# dev/prepare.R - development commands, identical for all packages of the suite
#
# Run line by line with the package project open (working directory = package
# root). Nothing in here refers to one particular package: name and paths are
# derived below. Package-specific material (data preparation, experiments)
# belongs in a file of its own next to this one.
#
# The folder dev/ is excluded from the build by .Rbuildignore (^dev$).
# ***************************************************************************

stop("dev/prepare.R is a collection of commands: run them line by line, do not source the file.")


# == 0  where we are ========================================================

pkgName   <- unname(read.dcf("DESCRIPTION")[1L, "Package"])
suiteRoot <- dirname(getwd())                       # e.g. C:/temp
suite     <- c("bedrock", "pharos", "lumen", "DescToolsX",
               "alloy", "pons", "swissValet")       # in order of dependency
hasSrc    <- dir.exists("src")

pkgName; suiteRoot; hasSrc


# == 1  the daily cycle =====================================================

if (hasSrc) Rcpp::compileAttributes()
if (hasSrc) pkgbuild::clean_dll()                   # after changes in src/ that do not rebuild

devtools::document()
devtools::load_all()

devtools::test()
devtools::test(filter = "auditNames")               # one test file: filter is a regex on its name
testthat::test_local(reporter = "progress")
devtools::run_examples()

rstudioapi::restartSession()                        # Ctrl+Shift+F10; needed before install on Windows
devtools::install(dependencies = FALSE, upgrade = FALSE)


# == 2  checks ==============================================================

devtools::check()
devtools::check(args = "--as-cran")
devtools::check(run_dont_test = TRUE)

# other platforms
devtools::check_win_devel()
devtools::check_mac_release()
rhub::rhub_check()                                  # the hard CRAN check

# quality
covr::package_coverage()
goodpractice::gp()

# non-ASCII characters in the sources
invisible(lapply(list.files("R", pattern = "[.][Rr]$", full.names = TRUE),
                 tools::showNonASCIIfile))

# what the NAMESPACE imports from / exports to
grep("bedrock", readLines("NAMESPACE"), value = TRUE)
getNamespaceExports(pkgName) |> sort()


# == 3  documentation =======================================================

# the PDF manual of this package (written next to the package folder)
devtools::build_manual()

# one help page as PDF, to check the LaTeX of a single topic
rd2pdf <- function(topic, outDir = suiteRoot) {
  rd <- file.path("man", paste0(topic, ".Rd"))
  if (!file.exists(rd))
    stop("no such help file: ", rd)
  out <- file.path(outDir, paste0(topic, ".pdf"))
  callr::rcmd("Rd2pdf", c("--no-preview", "--force",
                          paste0("--output=", out), rd), show = TRUE)
  invisible(out)
}
rd2pdf("between-operators")

# the targets of all \link{} in R/ or man/: run before and after a conversion
# of the documentation to see which targets were lost
linkTargets <- function(path) {
  files <- list.files(path, pattern = "[.]([Rr]|Rd)$", full.names = TRUE)
  txt   <- unlist(lapply(files, readLines, warn = FALSE))
  hits  <- unlist(regmatches(txt, gregexpr("\\\\link(\\[[^]]*\\])?\\{[^}]*\\}", txt)))

  opt  <- sub("^\\\\link(\\[([^]]*)\\])?\\{.*$", "\\2", hits)          # in [ ]
  body <- sub("^\\\\link(\\[[^]]*\\])?\\{([^}]*)\\}$", "\\2", hits)    # in { }

  ifelse(opt == "",            body,                    # \link{target}
  ifelse(startsWith(opt, "="), sub("^=", "", opt),      # \link[=target]{text}
  ifelse(grepl(":", opt),      opt,                     # \link[pkg:target]{text}
                               paste0(opt, ":", body))))    # \link[pkg]{target}
}
sort(table(linkTargets("man")))

before <- linkTargets("man")
roxygen2md::roxygen2md("full")                      # Rd markup -> markdown, once per package
devtools::document()
after  <- linkTargets("man")
setdiff(before, after)                              # targets lost
setdiff(after, before)                              # targets new

# vignettes
devtools::build_vignettes()
for (f in list.files("vignettes", pattern = "[.]pdf$", full.names = TRUE))
  tools::compactPDF(f, gs_quality = "ebook")


# == 4  pkgdown =============================================================

pkgdown::build_site()

# parts of it
pkgdown::build_home()
pkgdown::build_reference_index()                    # checks _pkgdown.yml against the help topics
pkgdown::build_favicons(overwrite = TRUE)

usethis::edit_file("_pkgdown.yml")
usethis::edit_file(".github/workflows/pkgdown.yaml")
# themes: https://bootswatch.com/
usethis::use_github_action("check-standard")


# == 5  the whole suite =====================================================

# install all packages, in order of dependency. A package installed in an
# older state than the one it depends on fails on loading ("object ... is not
# exported by 'namespace:...'"), so after a rename always install all of them.
for (p in suite)
  devtools::install(file.path(suiteRoot, p), dependencies = FALSE, upgrade = "never")

for (p in suite) devtools::document(file.path(suiteRoot, p))
for (p in suite) devtools::test(file.path(suiteRoot, p))
for (p in suite) devtools::build_manual(pkg = file.path(suiteRoot, p))
for (p in suite) pkgdown::build_site(file.path(suiteRoot, p))

# all functions of the suite
funs <- stats::setNames(lapply(suite, bedrock::funList), suite)
lengths(funs)
grep("CI$", unlist(funs), value = TRUE)


# == 6  @family and @concept tags from the taxonomy table ===================

# Rewrites the @family / @concept lines of every documented function in a
# file from a table with the columns fun, family, concept1 ... concept4.
# Functions that are not in the table, or have no family there, stay as
# they are.
updateRoxyTags <- function(file, tax) {

  lines  <- readLines(file)
  funIdx <- grep("^[a-zA-Z0-9_.]+\\s*<-\\s*function", lines)

  # backwards: an insertion only moves the lines below it, so the indices
  # still to be worked on stay valid
  for (i in rev(funIdx)) {

    row <- tax[tax$fun == sub("\\s*<-.*$", "", lines[i]), , drop = FALSE]
    if (nrow(row) == 0L) next
    row <- row[1L, ]
    if (is.na(row$family) || !nzchar(trimws(row$family))) next

    concepts <- trimws(as.character(unlist(
      row[intersect(paste0("concept", 1:4), names(tax))], use.names = FALSE)))
    concepts <- concepts[!is.na(concepts) & nzchar(concepts)]

    newTags <- c(paste0("#' @family ", trimws(row$family)),
                 if (length(concepts)) paste0("#' @concept ", concepts))

    # the roxygen block above the function, blank lines included
    start <- i - 1L
    while (start > 0L && grepl("^#'|^\\s*$", lines[start]))
      start <- start - 1L
    start <- start + 1L
    end   <- i - 1L
    if (end < start) next                           # not documented

    block <- lines[start:end]

    # drop the old tags together with the empty roxygen lines after them
    keep <- rep(TRUE, length(block))
    j <- 1L
    while (j <= length(block)) {
      if (grepl("^#'\\s*@(family|concept)\\b", block[j])) {
        keep[j] <- FALSE
        k <- j + 1L
        while (k <= length(block) && grepl("^#'\\s*$", block[k])) {
          keep[k] <- FALSE
          k <- k + 1L
        }
        j <- k
      } else {
        j <- j + 1L
      }
    }
    block <- block[keep]

    # the new tags go before @export, otherwise to the end of the block
    exportIdx <- grep("^#'\\s*@export\\b", block)
    block <- if (length(exportIdx)) append(block, newTags, after = exportIdx[1L] - 1L)
             else c(block, newTags)

    lines <- c(lines[seq_len(start - 1L)], block,
               lines[seq.int(end + 1L, length.out = length(lines) - end)])
  }

  writeLines(lines, file)
  invisible(file)
}

taxFile <- file.path(suiteRoot, "DescToolsX_doku", "bedrockFamiliesConcepts.xlsx")
tax     <- bedrock::toBaseR(readxl::read_excel(taxFile))

for (f in list.files("R", pattern = "[.][Rr]$", full.names = TRUE)) {
  cat("Updating:", f, "\n")
  updateRoxyTags(f, tax)
}
devtools::document()


# == 7  data sets ===========================================================

usethis::use_data(MyData, overwrite = TRUE)                     # exported: data/MyData.rda
usethis::use_data(Prefix, Units, internal = TRUE, overwrite = TRUE)   # internal: R/sysdata.rda

# rename an exported data set: new object name, new file, old file removed
renameData <- function(oldName, newName, dataDir = "data") {
  oldFile <- file.path(dataDir, paste0(oldName, ".rda"))
  e <- new.env()
  load(oldFile, envir = e)
  assign(newName, get(oldName, envir = e), envir = e)
  save(list = newName, envir = e, compress = "xz",
       file = file.path(dataDir, paste0(newName, ".rda")))
  file.remove(oldFile)
  cat(sprintf("  %s -> %s\n", oldName, newName))
}
renameData("d.pizza", "Pizza")


# == 8  once per machine or per package =====================================

# is the name free on CRAN, GitHub, Bioconductor?
available::available("figura", browse = FALSE)

usethis::use_build_ignore("dev")
usethis::use_pkgdown_github_pages()

# GitHub: create a token in the browser, then paste it at the hidden prompt
usethis::create_github_token(description = "Andri Windows RStudio")
gitcreds::gitcreds_set()
gh::gh_whoami()                                     # shows the user if the token works
gitcreds::gitcreds_delete("https://github.com")     # removes the stored token again
usethis::edit_r_environ()
Sys.which("git")

# LaTeX for the PDF manual
install.packages("tinytex")
tinytex::install_tinytex()
tinytex::tlmgr_install("makeindex")
tinytex::tlmgr_path("add")
Sys.which("pdflatex")                               # empty: add the TinyTeX bin folder for this session
Sys.setenv(PATH = paste(file.path(tinytex::tinytex_root(), "bin", "windows"),
                        Sys.getenv("PATH"), sep = .Platform$path.sep))

# pons: the COM bridge is not on CRAN
remotes::install_github("omegahat/RDCOMClient")

# RStudio key bindings live here
normalizePath(file.path(Sys.getenv("APPDATA"), "RStudio", "keybindings"))
