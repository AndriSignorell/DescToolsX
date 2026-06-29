

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
    y <- rf$x                                                 # response
    
    # 'group' (categorical) and 'predictor' (continuous) are distinct
    # fields under the resolveFormula() contract - reading rf$group for
    # a numeric-numeric design returns NULL and silently degrades the
    # pair to a one-sample case, failing downstream with
    # "Unknown type combination: n".
    xi <- switch(rf$type,
                 "one-sample"      = NULL,
                 "numeric-numeric" = rf$predictor,
                 rf$group)                                     # n-sample-independent
    
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
  
  nTotal   <- length(x)
  ok       <- complete.cases(x, y)
  nValid   <- sum(ok)
  nMissing <- nTotal - nValid
  
  pctValid   <- 100 * nValid   / nTotal
  pctMissing <- 100 * nMissing / nTotal
  
  isCat <- function(v) is.factor(v) || is.character(v) || is.logical(v)
  
  nGroups <- NA_integer_
  
  if (isCat(x) && !isCat(y)) {
    nGroups <- length(unique(x[ok]))
  } else if (!isCat(x) && isCat(y)) {
    nGroups <- length(unique(y[ok]))
  } else if (isCat(x) && isCat(y)) {
    nGroups <- nrow(unique(cbind(x[ok], y[ok])))
  }
  
  strOut <- gettextf("Summary:\npairs: %s, valid: %s (%s), missings: %s (%s)%s\n\n",
                     fm(nTotal,            fmt = "abs.sty"),
                     fm(nValid,            fmt = "abs.sty"),
                     fm(pctValid / 100,    fmt = "per.sty"),
                     fm(nMissing,          fmt = "abs.sty"),
                     fm(pctMissing / 100,  fmt = "per.sty"),
                     if(xor(isCat(x), isCat(y))) gettextf(", groups: %s", fm(nGroups, fmt = "abs.sty")) else ""
  )
  
  list(
    nTotal          = nTotal,
    nValid          = nValid,
    nMissing        = nMissing,
    pctValid        = pctValid,
    pctMissing      = pctMissing,
    nGroups         = nGroups,
    nMissingGroups  = sum(is.na(x)),
    pctMissingGroups = sum(is.na(x)) / nTotal,
    strOut          = strOut
  )
}



# only used in this context...
.typeOf <- function(x) {
  if (is.numeric(x)) return("n")
  if (is.factor(x) || is.character(x) || is.logical(x)) return("q")
  stop("Unsupported type")
}








