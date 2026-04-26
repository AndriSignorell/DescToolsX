

#' @rdname Desc
#' @method Desc formula
#' @export
Desc.formula <- function(formula, data, subset, na.action=na.pass, 
                         main = NULL, verbose = NULL, plotit = NULL, ...) {
  
  # Call rekonstruieren
  m <- match.call(expand.dots = FALSE)
  m$na.action <- na.action
  
  # Nur relevante Argumente behalten
  m <- m[c(1, match(c("formula", "data", "subset", "na.action"), names(m), 0))]
  
  # model.frame call setzen
  m[[1]] <- quote(stats::model.frame)
  
  ## >>> IMPORTANT: Treat subset correctly due to collision with subset()
  if (!missing(subset)) {
    m$subset <- substitute(subset)
  } else {
    m$subset <- NULL
  }
  
  # Evaluieren im Parent Frame
  mf <- eval(m, parent.frame())
  
  # Terms extrahieren
  terms_obj <- attr(mf, "terms")
  
  # Namen bestimmen
  y_name <- all.vars(formula[[2]])
  x_names <- attr(terms_obj, "term.labels")
  
  # y
  y <- mf[[y_name]]
  
  # x (Liste)
  x_list <- lapply(x_names, function(n) mf[[n]])
  names(x_list) <- x_names
  
  # Output
  z <- list(
    y = y,
    x = x_list,
    y_name = y_name,
    x_names = x_names,
    formula = formula
  )

  # --------------------------------------------------------------
  # got the data so far, now do the calculations....

  ty <- .type_of(y)
  
  call <- match.call()
  
  res <- lapply(names(x_list), function(nm) {
    
    xi <- x_list[[nm]]
    type <- paste(ty, .type_of(xi), sep = "")
    
    FUN <- switch(type,
           # choose evaluating function in dependence of the y- and x types
           # these function take y and x variables in this sequence (!)
             "nn" = .desc_nn,
             "qn"  = .desc_qn,
             "nq"  = .desc_nq,
             "qq"   = .desc_qq,
           stop("Unknown type combination")
    )

    structure(list(
      meta = .descMetaXY(nm, y_name, main, plotit, verbose, 
                         gettextf("Desc.%s", type), call=call ),
      pair = .calcPairSummary(xi, y),
      res  = FUN(y, xi, ... ),
      data = list(x=xi, y=y)
      
    ), class=c(gettextf("Desc.%s", type), "Desc"))
    
  })
  
  names(res) <- names(x_list)
  class(res) <- c("Desc", "list")
  
  return(res)

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
.type_of <- function(x) {
  if (is.numeric(x)) return("n")
  if (is.factor(x) || is.character(x) || is.logical(x)) return("q")
  stop("Unsupported type")
}








