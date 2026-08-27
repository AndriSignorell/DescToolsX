

Rcpp::compileAttributes()
pkgbuild::clean_dll()
devtools::check()
devtools::install()
devtools::build_manual(pkg = "C:/temp/DescToolsX")
devtools::build_manual(pkg = "C:/temp/lumen")
devtools::build_manual(pkg = "C:/temp/pharos")
devtools::build_manual(pkg = "C:/temp/bedrock")
devtools::build_manual(pkg = "C:/temp/alloy")
devtools::build_manual(pkg = "C:/temp/pons")

devtools::build_manual(pkg = "C:/temp/swissValet")

devtools::document()
devtools::load_all()
devtools::check()
devtools::test()
devtools::run_examples()

bedrock::`%][%`()

devtools::

covr::package_coverage()
goodpractice::gp()


1:10 %[]% c(3,4)

usethis::use_build_ignore("dev")  

grep("bedrock", readLines("NAMESPACE"), value = TRUE)
c("%[]%", "%[)%") %in% getNamespaceExports("bedrock")   # sollte TRUE TRUE geben


pkgdown::build_site()
pkgdown::build_reference_index()
pkgdown::build_favicons(overwrite=TRUE)

pkgdown::build_home()
pkgdown::build_site()
usethis::use_pkgdown_github_pages()






# hard CRAN check
rhub::rhub_check()


spdf <- function(x){
  callr::rcmd("Rd2pdf",
              c("--no-preview", "--force", 
                gettextf("--output=%s.pdf", x),
                gettextf("%s/man/%s.Rd", getwd(), x),
              show = TRUE))
  }

spdf(x="contCoef")

getwd()


# for themes
# https://bootswatch.com/
  

# rename system data

rename_rda <- function(old_file, old_name, new_name, data_dir = "C:/temp/bedrock/data/") {
  e <- new.env()
  load(file.path(data_dir, old_file), envir = e)
  assign(new_name, get(old_name, envir = e), envir = e)
  rm(list = old_name, envir = e)
  
  tmp <- tempfile(fileext = ".rda")
  save(list = new_name, envir = e, file = tmp, compress = "xz")
  
  file.remove(file.path(data_dir, old_file))
  file.copy(tmp, file.path(data_dir, paste0(new_name, ".rda")))
  file.remove(tmp)
  
  cat(sprintf("  %s -> %s\n", old_name, new_name))
}


rename_rda("d.pizza.rda",  "d.pizza",  "Pizza")
rename_rda("cards.rda",    "cards",    "Cards")
rename_rda("roulette.rda", "roulette", "Roulette")
rename_rda("tarot.rda",    "tarot",    "Tarot")



sortX(funList("bedrock", ex=T))

grep(sortX(funList("bedrock", ex=T)), v=T)


Pizza

files <- list.files("c:/temp/bedrock/R", full.names = TRUE, pattern = "\\.R$")
files <- list.files("c:/temp/pharos/R", full.names = TRUE, pattern = "\\.R$")
files <- list.files("c:/temp/lumen/R", full.names = TRUE, pattern = "\\.R$")
files <- list.files("c:/temp/alloy/R", full.names = TRUE, pattern = "\\.R$")

tax <- bedrock::toBaseR(readxl::read_excel("c:/temp/DescToolsX_doku/bedrockFamiliesConcepts.xlsx"))

for (f in files) {
  cat("Updating:", f, "\n")
  update_roxy_safe(f, tax)
}





update_roxy_safe <- function(file, tax) {
  
  lines <- readLines(file)
  fun_idx <- grep("^[a-zA-Z0-9_.]+\\s*<-\\s*function", lines)
  
  # rueckwaerts iterieren: Insertions verschieben nur Zeilen unterhalb,
  # die noch abzuarbeitenden Indizes weiter oben bleiben gueltig
  for (i in rev(fun_idx)) {
    
    fun_name <- sub("\\s*<-.*$", "", lines[i])
    
    row <- tax[tax$fun == fun_name, , drop = FALSE]
    if (nrow(row) == 0L) next
    row <- row[1L, ]
    
    # ------------------------------------------------------------------
    # NEUE TAGS AUS family + concept1..4 BAUEN
    # ------------------------------------------------------------------
    
    if (is.na(row$family) || !nzchar(trimws(row$family))) next
    
    conceptCols <- intersect(paste0("concept", 1:4), names(tax))
    concepts <- trimws(as.character(unlist(row[conceptCols], use.names = FALSE)))
    concepts <- concepts[!is.na(concepts) & nzchar(concepts)]
    
    new_tags <- c(
      paste0("#' @family ", trimws(row$family)),
      if (length(concepts)) paste0("#' @concept ", concepts)
    )
    
    # ------------------------------------------------------------------
    # GANZEN ROXYGEN-BLOCK FINDEN (inkl. Leerzeilen)
    # ------------------------------------------------------------------
    
    start <- i - 1
    while (start > 0 && grepl("^#'|^\\s*$", lines[start]))
      start <- start - 1
    start <- start + 1
    end <- i - 1
    
    if (end < start) next   # keine roxygen-Doku vorhanden -> nichts zu tun
    
    block <- lines[start:end]
    
    # ------------------------------------------------------------------
    # ALTE @family/@concept-ZEILEN ENTFERNEN
    # ------------------------------------------------------------------
    
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
    
    # ------------------------------------------------------------------
    # EINBAU VOR @export (sonst ans Blockende)
    # ------------------------------------------------------------------
    
    export_idx <- grep("^#'\\s*@export\\b", block)
    
    if (length(export_idx))
      block <- append(block, new_tags, after = export_idx[1L] - 1L)
    else
      block <- c(block, new_tags)
    
    # ------------------------------------------------------------------
    # BLOCK ERSETZEN (splice)
    # ------------------------------------------------------------------
    
    lines <- c(
      if (start > 1L) lines[seq_len(start - 1L)] else character(0),
      block,
      if (end < length(lines)) lines[(end + 1L):length(lines)] else character(0)
    )
  }
  
  writeLines(lines, file)
  invisible(file)
}



Sys.which("git")
