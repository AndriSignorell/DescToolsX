# cranPrep.R - Checkliste fuer CRAN-Einreichungen der Suite-Pakete
#
# Ablauf:
#   source("cranPrep.R")
#   cranPrep()              # 1. lokale Pruefungen, Remote-Checks anstossen
#   ... Mails von win-builder / macbuilder abwarten ...
#   cranComments(reason = "...")   # 2. cran-comments.md schreiben
#   devtools::submit_cran()        # 3. einreichen
#
# Voraussetzung: Arbeitsverzeichnis = Paket-Root.


# ---- Hilfen -----------------------------------------------------------------

.step <- function(msg) cat("\n== ", msg, " ", strrep("=", max(0, 60 - nchar(msg))), "\n", sep = "")

.ok   <- function(msg) cat("  [OK]   ", msg, "\n", sep = "")
.warn <- function(msg) cat("  [!!]   ", msg, "\n", sep = "")

.pkgInfo <- function() {
  d <- read.dcf("DESCRIPTION", fields = c("Package", "Version"))
  list(pkg = d[1, "Package"], version = d[1, "Version"])
}

.cranVersion <- function(pkg) {
  db <- tryCatch(available.packages(repos = "https://cloud.r-project.org"),
                 error = function(e) NULL)
  if (is.null(db) || !pkg %in% rownames(db)) NA_character_
  else db[pkg, "Version"]
}


# ---- 1. Vorbereitung ----------------------------------------------------------

cranPrep <- function(remote = TRUE) {

  info <- .pkgInfo()
  cat("Paket:", info$pkg, info$version, "\n")

  # -- git sauber? (uncommittete Aenderungen landen sonst ungeprueft im Tarball)
  .step("git")
  st <- tryCatch(system2("git", c("status", "--porcelain"), stdout = TRUE),
                 error = function(e) NA)
  if (length(st) == 0L) .ok("Arbeitsverzeichnis sauber")
  else .warn(paste("uncommittete Aenderungen:", length(st), "Dateien"))

  # -- Version hoeher als CRAN?
  .step("Version")
  cv <- .cranVersion(info$pkg)
  if (is.na(cv)) .ok("noch nicht auf CRAN (Erstsubmission)")
  else if (package_version(info$version) > package_version(cv))
    .ok(sprintf("%s > CRAN %s", info$version, cv))
  else .warn(sprintf("Version %s ist nicht hoeher als CRAN %s", info$version, cv))

  # -- NEWS nachgefuehrt?
  .step("NEWS")
  if (!file.exists("NEWS.md")) .warn("keine NEWS.md")
  else {
    top <- grep("^# ", readLines("NEWS.md", warn = FALSE), value = TRUE)[1L]
    if (!is.na(top) && grepl(info$version, top, fixed = TRUE))
      .ok(paste("oberster Eintrag:", top))
    else .warn(paste("oberster Eintrag passt nicht zur Version:", top))
  }

  # -- Reverse Dependencies auf CRAN
  .step("Reverse Dependencies")
  rd <- if (is.na(cv)) character(0) else
    tools::package_dependencies(info$pkg, reverse = TRUE,
                                which = c("Depends", "Imports", "LinkingTo", "Suggests"),
                                db = available.packages(repos = "https://cloud.r-project.org"))[[1L]]
  if (length(rd) == 0L) .ok("keine")
  else .warn(paste0(length(rd), " Revdeps - revdepcheck::revdep_check() laufen lassen: ",
                    paste(rd, collapse = ", ")))

  # -- URLs (haeufigster Grund fuer NOTEs bei der Einreichung)
  .step("URLs")
  if (requireNamespace("urlchecker", quietly = TRUE)) {
    u <- urlchecker::url_check()
    if (nrow(u) == 0L) .ok("alle URLs erreichbar") else print(u)
  } else .warn("urlchecker nicht installiert - uebersprungen")

  # -- Rechtschreibung (nur Hinweis)
  .step("Spelling")
  if (requireNamespace("spelling", quietly = TRUE)) {
    s <- spelling::spell_check_package()
    if (nrow(s) == 0L) .ok("keine Funde") else print(s)
  } else .warn("spelling nicht installiert - uebersprungen")

  # -- lokaler Check wie auf CRAN
  .step("R CMD check --as-cran (lokal)")
  res <- devtools::check(remote = TRUE, manual = TRUE, error_on = "never")
  n <- c(errors = length(res$errors), warnings = length(res$warnings),
         notes = length(res$notes))
  print(n)
  if (sum(n[1:2]) == 0L) .ok("keine Errors/Warnings") else .warn("Check nicht sauber")

  # -- Remote-Checks: Resultate kommen per Mail bzw. als Link
  if (remote) {
    .step("Remote-Checks anstossen")
    devtools::check_win_devel()
    devtools::check_mac_release()
    .ok("win-builder (devel) und macbuilder (arm64) gestartet - Resultate abwarten")
  }

  invisible(list(info = info, cranVersion = cv, revdeps = rd, check = n))
}


# ---- 2. cran-comments.md ------------------------------------------------------

cranComments <- function(reason = NULL,
                         envs = c(sprintf("local %s, R %s",
                                          Sys.info()[["sysname"]], getRversion()),
                                  "win-builder (r-devel)",
                                  "macOS builder (r-release, arm64)"),
                         notes = "0 errors | 0 warnings | 0 notes") {

  info <- .pkgInfo()
  cv   <- .cranVersion(info$pkg)

  intro <- if (is.na(cv)) "This is a new submission."
           else sprintf("This is an update from %s to %s.", cv, info$version)

  txt <- c(intro, "",
           if (!is.null(reason)) c(reason, ""),
           if (!is.na(cv)) c("Further changes are listed in NEWS.md.", ""),
           "## Test environments",
           paste("*", envs), "",
           "## R CMD check results",
           notes, "",
           "## Reverse dependencies",
           "None.")

  writeLines(txt, "cran-comments.md")

  # nicht ins Paket bauen
  if (!file.exists(".Rbuildignore") ||
      !any(grepl("cran-comments", readLines(".Rbuildignore"), fixed = TRUE)))
    usethis::use_build_ignore("cran-comments.md")

  cat(txt, sep = "\n")
  invisible(txt)
}
