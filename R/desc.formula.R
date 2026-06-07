

#' @inheritParams Formulas
#' @rdname desc
#' @method desc formula
#' @export
desc.formula <- function(formula, data, subset, na.action = na.pass,
                         main = NULL, verbose = NULL, plotit = NULL, ...) {
  
  subset_expr <- if (!missing(subset)) substitute(subset) else NULL
  call        <- match.call()
  
  # ── Zielgrösse und RHS-Terme bestimmen ─────────────────────────────────────
  y_name  <- deparse(formula[[2L]])
  x_names <- attr(terms(formula), "term.labels")
  
  # ── Pro RHS-Term eine eigene resolveFormula ─────────────────────────────────
  res <- lapply(x_names, function(nm) {
    
    f1 <- as.formula(paste(y_name, "~", nm), env = environment(formula))
    
    rf <- do.call(resolveFormula, list(
      formula   = f1,
      data      = data,
      subset    = subset_expr,
      na.action = na.action,
      allowed   = c("one-sample", "n-sample-independent", "numeric-numeric")
    ))
    
    # ── Variablen aus rf ──────────────────────────────────────────────────────
    y  <- rf$x                                                # response
    xi <- if (rf$type == "one-sample") NULL else rf$group    # Gruppierung/Prädiktor
    
    # ── one-sample: direkt zu desc() ─────────────────────────────────────────
    if (rf$type == "one-sample")
      return(desc(y,
                  main    = main %||% gettextf("%s[%s]", y_name, deparse(subset_expr)),
                  plotit  = plotit,
                  verbose = verbose, ...))
    
    # ── Typ bestimmen ─────────────────────────────────────────────────────────
    ty   <- .typeOf(y)
    type <- if (is.null(xi)) ty else paste0(ty, .typeOf(xi))
    
    FUN <- switch(type,
                  "nn" = .descNN,
                  "nq" = .descNQ,
                  "qn" = .descQN,
                  "qq" = .descQQ,
                  stop(gettextf("Unknown type combination: %s", type))
    )
    
    structure(
      list(
        meta = .descMetaXY(nm, y_name, main, plotit, verbose,
                           gettextf("Desc.%s", type), call = call),
        pair = .calcPairSummary(xi, y),
        res  = FUN(y, xi, ...),
        data = list(x = xi, y = y)
      ),
      class = c(gettextf("Desc.%s", type), "Desc")
    )
  })
  
  names(res) <- x_names
  class(res) <- c("Desc", "list")
  res
}




# == internal helper functions ===============================================



.descMetaXY <- function(xname, yname, main, plotit, verbose, class, call){
  
  list(
    xname      = xname,
    yname      = yname,
    label      = NA,
    main       = main %||% gettextf("%s ~ %s (%s)", yname, xname, deparse(call$data)),
    class      = class,
    label      = NULL, 
    #    classlabel = paste(class(x), collapse = ","),
    plotit     = plotit %||% .getOption("plotit", FALSE),
    call       = call,
    timestamp  = Sys.time(),
    verbose    = verbose %||% getOption("Desc.verbose", 2)
  )
  
}



.calcPairSummary <- function(x, y) {
  
  # Länge (Paare)
  n_total <- length(x)
  
  # Complete cases
  ok <- complete.cases(x, y)
  n_valid <- sum(ok)
  n_missing <- n_total - n_valid
  
  pct_valid <- 100 * n_valid / n_total
  pct_missing <- 100 * n_missing / n_total
  
  # Gruppen bestimmen (falls kategorial)
  is_cat <- function(v) is.factor(v) || is.character(v) || is.logical(v)
  
  n_groups <- NA_integer_
  
  if (is_cat(x) && !is_cat(y)) {
    n_groups <- length(unique(x[ok]))
  } else if (!is_cat(x) && is_cat(y)) {
    n_groups <- length(unique(y[ok]))
  } else if (is_cat(x) && is_cat(y)) {
    # beide kategorial → Kombinationen
    n_groups <- nrow(unique(cbind(x[ok], y[ok])))
  }
  
  
  out <- gettextf("Summary:\npairs: %s, valid: %s (%s), missings: %s (%s)%s\n\n",
    fm(n_total,   fmt = "abs.sty"),
    fm(n_valid,   fmt = "abs.sty"),
    fm(pct_valid / 100,   fmt = "per.sty"),
    fm(n_missing, fmt = "abs.sty"),
    fm(pct_missing / 100, fmt = "per.sty"),
    if(xor(is_cat(x), is_cat(y))) gettextf(", groups: %s", fm(n_groups, fmt = "abs.sty")) else ""
  )

  # Output
  res <- list(
    pairs_n   = n_total,
    valid_n   = n_valid,
    missing_n = n_missing,
    valid_p = pct_valid,
    missing_p = pct_missing,
    groups    = n_groups,
    missing_groups = sum(is.na(x)),
    missing_groups_p = sum(is.na(x)) / n_total,
    strOut    = out
  )
  
  return(res)
  
}


# only used in this context...
.typeOf <- function(x) {
  if (is.numeric(x)) return("n")
  if (is.factor(x) || is.character(x) || is.logical(x)) return("q")
  stop("Unsupported type")
}








