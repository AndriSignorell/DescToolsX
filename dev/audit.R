

auditCI <- function(pkg = "DescToolsX", depth = 3L) {
  
  ns   <- asNamespace(pkg)
  pool <- ls(ns, all.names = TRUE)            # auch die internen Helfer
  
  bodyOf <- function(nm) {
    f <- get(nm, envir = ns)
    if (!is.function(f) || is.primitive(f)) return("")
    paste(deparse(body(f)), collapse = " ")
  }
  
  # transitive Hülle über die Funktionen desselben Namensraums
  reach <- function(nm) {
    seen <- nm; front <- nm
    for (i in seq_len(depth)) {
      new <- unlist(lapply(front, function(k) {
        f <- get(k, envir = ns)
        if (!is.function(f) || is.primitive(f)) return(character(0))
        intersect(unique(all.names(body(f))), pool)
      }))
      new   <- setdiff(unique(new), seen)
      if (!length(new)) break
      seen  <- c(seen, new); front <- new
    }
    seen
  }
  
  dat  <- c("x", "y", "n", "pred", "data", "formula", "object")
  inf  <- c("conf.level", "sides", "method")
  hand <- c("na.rm", "subset", "use", "out", "verbose", "...")
  skip <- c("randolphKappa")                  # lehnt conf.level bewusst ab
  
  rows <- lapply(setdiff(sort(getNamespaceExports(ns)), skip), function(nm) {
    
    f <- get(nm, envir = ns)
    if (!is.function(f) || is.primitive(f)) return(NULL)
    a <- names(formals(f))
    if (!"conf.level" %in% a) return(NULL)
    
    src <- paste(vapply(reach(nm), bodyOf, ""), collapse = " ")
    ie  <- which(!a %in% c(dat, inf, hand))
    ii  <- which(a %in% inf)
    
    data.frame(fun = nm,
               sides     = "sides" %in% a,
               checksCL  = grepl("checkConfLevel", src, fixed = TRUE),
               usesSides = grepl("applySides",     src, fixed = TRUE),
               badOrder  = (length(ie) > 0 && length(ii) > 0 &&
                              min(ii) < max(ie)) ||
                 is.unsorted(match(a[a %in% inf], inf)),
               args = paste(a, collapse = ", "),
               stringsAsFactors = FALSE)
  })
  
  out <- do.call(rbind, rows)
  out[!out$sides | !out$checksCL | !out$usesSides | out$badOrder, ]
}

auditCI()

x <- rnorm(100)

skew(x, conf.level = 0.95)

meanCI()
pons::xlKill()

rbind(auditCI(), auditCI("lumen"))



auditCI("pharos")
auditCI("bedrock")
auditCI("alloy")


auditMatchArg <- function(pkg = "DescToolsX") {
  ns <- asNamespace(pkg)
  out <- vapply(sort(getNamespaceExports(ns)), function(nm) {
    f <- get(nm, envir = ns)
    if (!is.function(f) || is.primitive(f)) return("")
    a <- formals(f)
    enums <- names(a)[vapply(a, function(d)
      is.call(d) && identical(d[[1L]], quote(c)) || is.character(d), FALSE)]
    if (!length(enums)) return("")
    src <- paste(deparse(body(f)), collapse = " ")
    miss <- enums[!vapply(enums, function(e)
      grepl(paste0("match.arg(", e), src, fixed = TRUE), TRUE)]
    if (length(miss)) paste(miss, collapse = ", ") else ""
  }, character(1))
  out[nzchar(out)]
}


auditMatchArg()

