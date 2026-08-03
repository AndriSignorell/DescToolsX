
#' Percentage Table
#' 
#' Creates a 2-way contingency table along with percentages, marginal, and
#' conditional distributions. All the frequencies are nested into one single
#' table. 
#' 
#' PercTable prints a 2-dimensional table. The absolute and relative
#' frequencies are nested into one flat table by means of \code{ftable}.
#' \code{row.vars}, resp. \code{col.vars} can be used to define the structure
#' of the table. \code{row.vars} can either be the names of the dimensions
#' (included percentages are named \code{"idx"}) or numbers (1:3, where 1 is
#' the first dimension of the table, 2 the second and 3 the percentages). \cr
#' Use \code{sortX()} if you want to have your table sorted by rows.\cr\cr The
#' style in which numbers are formatted is selected by \code{\link[pharos]{style}()} from
#' the DescToolsX options. Absolute frequencies will use \code{style("abs.sty")} and
#' \code{style("per.sty")} will do it for the percentages. The options can be changed
#' with \code{style(abs, digits=5)} which is basically a \code{"style"}-object
#' containing any format information used in \code{\link[pharos]{fm}}.
#' 
#' \code{margins} adds the marginal distributions. In the frequency table
#' these are the usual row/column sums; in the percentage tables the margin
#' holds the \emph{marginal distribution}, i.e. the row resp. column sums of
#' the frequency table divided by the grand total. A margin is only shown
#' where it carries information: the sum column of the row percentages is
#' \eqn{100\%} by construction and the sum row of the row percentages is not a
#' distribution at all, so whichever of the two is uninformative is printed as
#' \code{"."}.
#' 
#' @name percTable
#' @aliases percTable percTable.default percTable.table percTable.formula percTable.matrix print.PercTable
#' 
#' @param x a table, a matrix, or a vector to be tabulated
#' @param y an optional second vector to be tabulated against \code{x}
#' @param row.vars a vector of row variables (see Details)
#' @param col.vars a vector of column variables (see Details). If this is left
#' to \code{NULL} the table structure will be preserved. 
#' @param justify either \code{"left"} or \code{"right"} for defining the
#' alignment of the table cells
#' @param freq logical. Should absolute frequencies be included? Defaults to
#' \code{TRUE}.
#' @param prop character vector specifying the proportions to display, using
#' \code{"rows"}, \code{"cols"}, \code{"total"}, or \code{"none"}
#' @param expected logical; whether to include expected counts under independence
#' @param margins vector specifying the margins to include. Use \code{1} or
#' \code{"rows"} for row margins, \code{2} or \code{"cols"} for column
#' margins, or both; \code{NULL} includes none.
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} will
#' be tabled versus rhs (\code{table(lhs, rhs)})
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the formula
#' \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations to be
#' used
#' @param na.action a function which indicates what should happen when the data
#' contain NAs. Defaults to \code{getOption("na.action")}.
#' @param blockSep logical, defining if an empty row should be introduced between
#' the table rows. Default is FALSE, if only a table with one single
#' description (either frequencies or percents) should be returned and
#' \code{TRUE} in any other case. 
#' @param \dots further arguments passed to \code{print.PercTable()}
#' 
#' @return an object of class \code{"PercTable"} containing the requested
#' frequency and percentage tables
#' 
#' 
#' @references Agresti, Alan (2007) \emph{Introduction to categorical data
#' analysis}. NY: John Wiley and Sons, Section 2.4.5\cr
#' 
#' @examples
#' 
#' tab <- as.table(apply(HairEyeColor, c(1,2), sum))
#' 
#' percTable(tab, col.vars=2)
#' 
#' percTable(tab, col.vars=2, margins=c(1,2))
#' percTable(tab, col.vars=2, margins=2)
#' percTable(tab, col.vars=2, margins=1)
#' percTable(tab, col.vars=2, margins=NULL)
#' 
#' percTable(tab, col.vars=2, prop="none")
#' 
#' # just the percentages without absolute values
#' percTable(tab, col.vars=2, prop=c("total","rows"), freq=FALSE)
#' 
#' # just the row percentages
#' percTable(tab, freq= FALSE, prop="rows")
#' 
#' # just the expected frequencies
#' percTable(tab, prop="none", expected = TRUE)
#' 
#' 
#' # rearrange output such that freq are inserted as columns instead of rows
#' percTable(tab, col.vars=c(3,2))
#' 
#' # putting the areas in rows
#' percTable(tab, col.vars=c(3,1), prop="total", margins=c(1,2))
#' 
#' # formula interface with subset
#' percTable(driver ~ area, data=Pizza, subset=wine_delivered==0)
#' 
#' # sort the table by rows, order first column (Zurich), then third, then row.names (0)
#' percTable(sortX(tab, ord=c(1,3,0)))
#' 
#' # reverse the row variables, so that absolute frequencies and percents
#' # are not nested together
#' percTable(tab, row.vars=c(3, 1))
#' 
#' # the vector interface
#' percTable(x=Pizza$driver, y=Pizza$area)
#' percTable(x=Pizza$driver, y=Pizza$area, prop="rows", 
#' margins=c("rows","cols"))
#' 
#' @seealso [table], [ftable], [proportions], [addmargins], 
#' [setDescToolsXOption], [pharos::style]\cr 
#' There are similar functions in [sfsmisc::printTable2] and
#' package \pkg{vcd} [vcd::table2d_summary], both lacking some of
#' the flexibility we needed here. 
#' 
#' @family frequency  
#' @concept frequency-table
#'
#' @export
percTable <- function (...) UseMethod("percTable")


#' @rdname percTable
#' @export 
percTable.default <- function (x, y = NULL, ...) {
  
  # all arguments which match percTable.table or the print function
  percTableArgs <- names(c(formals(percTable.table), 
                           formals(print.PercTable)))

  # Split the *evaluated* dots. The previous version carried the unevaluated
  # expressions from match.call(expand.dots = FALSE)$... through do.call(),
  # which then evaluated them in do.call's default envir = parent.frame().
  # That works, but it makes the result depend on which frame parent.frame()
  # refers to after UseMethod() dispatch; list(...) forces each promise in its
  # own environment and needs no such assumption. No behaviour change intended.
  dotArgs <- list(...)
  nms <- names(dotArgs)
  if (is.null(nms)) nms <- rep("", length(dotArgs))

  ptArgs  <- dotArgs[nms %in% percTableArgs]
  tabArgs <- dotArgs[!(nms %in% percTableArgs)]

  tab <- if (is.null(y))
    do.call(table, c(list(x), tabArgs))
  else
    do.call(table, c(list(x, y), tabArgs))

  if (length(dim(tab)) != 2L)
    stop(gettextf(
      paste("percTable() needs a two-dimensional table; tabulating the given",
            "argument(s) yielded %d dimension(s). Use freq() for a single",
            "variable."),
      length(dim(tab))), domain = NA)

  do.call(percTable, c(list(x = tab), ptArgs))
  
}

# PercTable.data.frame <- function(x, ...){  sapply(x, PercTable, ...) }


#' @rdname percTable
#' @export
percTable.formula <- function(formula, data, subset, na.action, ...) {
  
  if (missing(formula) || (length(formula) != 3L) || 
      (length(attr(terms(formula[-2L]), "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  
    
  ## IMPORTANT!!
  ## --- capture subset / na.action HERE ---
  subset_expr <- if (!missing(subset)) substitute(subset) else NULL
  na_expr     <- if (!missing(na.action)) substitute(na.action) else NULL
  
  pf <- resolveFormula(
    formula   = formula,
    data      = data,
    subset    = subset_expr,
    na.action = na_expr,
    allowed   = c("two-sample-independent", "n-sample-independent")
  )
  
  y <- do.call("percTable", c(list(table(pf$mf)), list(...)))
  attr(y, "data.name") <- pf$data.name
  y
  
}




#' @rdname percTable
#' @export
percTable.table <- function(x, freq=TRUE, 
                            prop = c("rows", "cols", "total"), 
                            expected = FALSE, ...) {
  
  
  if (!is.array(x) ||
      length(dim(x)) != 2 ||
      any(dim(x) == 0) ||
      !isWholeLike(x, isNonNegative = TRUE)) {
    
    stop("'x' must be a 2-dimensional array of non-negative integer-like counts.")
  }
  
  prop <- match.arg(prop, c("rows","cols","total","none"), several.ok = TRUE)

  if ("none" %in% prop && length(prop) > 1L)
    stop("'prop = \"none\"' cannot be combined with other proportions.")
  
  res <- list()
  
  if (freq)
    res$freq <- x
  
  if ("total" %in% prop)
    res$perc <- proportions(x)
  
  if ("rows" %in% prop)
    res$p.row <- proportions(x, margin = 1)
  
  if ("cols" %in% prop)
    res$p.col <- proportions(x, margin = 2)
  
  if (expected)
    res$expected <- expFreq(x)

  if (length(res) == 0L)
    stop("nothing to show: at least one of 'freq', 'prop' or 'expected' is needed.")
  
  res$.printArgs <- list(...)
  
  class(res) <- "PercTable"
  return(res)
  
}



#' @rdname percTable
#' @export
print.PercTable <- function(x,
                            margins = NULL,
                            col.vars = NULL,
                            row.vars = NULL,
                            justify = NULL,
                            blockSep = NULL,
                            ...) {
  
  .printArgs <- x$.printArgs
  x$.printArgs <- NULL
  
  tables <- x
  
  # argument control sequence:
  # 1. given argument in print function
  # 2. set argument in calculating funtion via .printArgs
  # 3. default value
  
  mar <- margins %||% .printArgs[["margins"]] %||% NULL
  justify <- justify %||% .printArgs[["justify"]] %||% "right"
  blockSep <- blockSep %||% .printArgs[["blockSep"]] %||% (length(tables) > 1)
  
  row.vars <- row.vars %||% .printArgs[["row.vars"]] 
  col.vars <- col.vars %||% .printArgs[["col.vars"]] 

  # a single table has two dimensions only -- the third (the "idx" dimension
  # holding freq/perc/...) exists only once several tables are stacked
  nd <- if (length(tables) > 1L) 3L else 2L
  if (is.numeric(row.vars)) row.vars <- row.vars[row.vars <= nd]
  if (is.numeric(col.vars)) col.vars <- col.vars[col.vars <= nd]
  if (length(row.vars) == 0L) row.vars <- NULL
  if (length(col.vars) == 0L) col.vars <- NULL
  
  if(is.null(row.vars)){
    if(is.null(col.vars)){
      # neither row.vars, nor col.vars provided, set defaults
      row.vars <- setdiff(seq_len(nd), 2L)
      col.vars <- 2L
    } else
      # default row.vars  with given col.vars 
      row.vars <- setdiff(seq_len(nd), col.vars)
  } else {      
    if(is.null(col.vars))
      # default col.vars with given row.vars
      col.vars <- setdiff(seq_len(nd), row.vars)
    # else: both are defined by the user
  }
  
  if (!is.null(mar)) {

    if (!is.numeric(mar)) {
      marIn <- mar
      mar <- match(mar, c("rows", "cols"))
      if (anyNA(mar))
        stop(gettextf("invalid margin(s): %s. Use 1/\"rows\" and/or 2/\"cols\".",
                      paste(sQuote(marIn[is.na(mar)]), collapse = ", ")),
             domain = NA)
    }
    if (!all(mar %in% c(1L, 2L)))
      stop("'margins' must be 1 (\"rows\") and/or 2 (\"cols\").")
    mar <- sort(unique(as.integer(mar)))

    tables <- .addPercTableMargins(tables, mar)
  }  
  
  abstab <- names(tables) %in% c("freq", "expected")
  tables[abstab] <- lapply(tables[abstab], fm, fmt="abs.sty")
  ptab <-  names(tables) %in% c("perc", "p.row", "p.col")
  tables[ptab] <- lapply(tables[ptab], fm, fmt="per.sty")
  
  if (!is.null(mar)) {
    hasFreq <- "freq" %in% names(tables)
    hasPerc <- "perc" %in% names(tables)
    condTabs <- names(tables) %in% c("p.row", "p.col")
    tables[condTabs] <- lapply(names(tables[condTabs]), function(nm) {
      tab <- tables[[nm]]
      if (hasPerc) {
        # perc already carries both marginal distributions
        if (1 %in% mar) tab[nrow(tab), ] <- "."
        if (2 %in% mar) tab[, ncol(tab)] <- "."
      } else if (!hasFreq) {
        # no frequency table: the margins are plain sums of proportions.
        # For p.row the rows sum to 1, so the added *column* is the 100%
        # column and informative, while the added *row* sums proportions
        # across rows and is not a distribution. For p.col it is the other
        # way round.
        if (nm == "p.row" && 1 %in% mar) tab[nrow(tab), ] <- "."
        if (nm == "p.col" && 2 %in% mar) tab[, ncol(tab)] <- "."
      }
      # hasFreq && !hasPerc: both margins were replaced by the marginal
      # distributions above and are shown as they are.
      tab
    })
  }
  
  if (length(tables) == 1) {
    out <- ftable(tables[[1]], row.vars = row.vars, col.vars = col.vars)
  } else {
    arr <- bedrock::abind(tables, along = 3)
    out <- ftable(arr, col.vars=col.vars, row.vars=row.vars)
  }
  
  out[] <- format(as.matrix(out), justify=justify, 
                  width=max(nchar(out))+2)
  
  
  txt <- capture.output(print(out))
  
  if( blockSep ){
    # Blockstart: Rows not starting with spaces
    blockIdx <- grep("^[^[:space:]]", txt)
    
    # remove the first
    blockIdx <- blockIdx[-1]
    
    offset <- -1
    for (i in blockIdx) {
      pos <- i + offset
      txt <- append(txt, "", after = pos)
      offset <- offset + 1
    }
  }
  
  cat(paste(txt, collapse="\n"), "\n")

  # NOTE: unlike the usual print() contract this returns the formatted lines,
  # not its argument. Left unchanged -- see REVIEW.md, open question (P6).
  invisible(txt)
  
}




#' @rdname percTable
#' @export
percTable.matrix <- percTable.table



# == internal helper functions ================================================

# Adds the requested margins to every table of a PercTable list. In the
# percentage tables the margin must hold the MARGINAL DISTRIBUTION, i.e. the
# row/column sums of the frequency table divided by the grand total.
#
# The grand total is sum(freq). It used to be read as the corner element of
# the marginalised frequency table, which is the grand total only when both
# margins were added: with margins = 1 that corner is the total of the last
# COLUMN, with margins = 2 the total of the last ROW. For
# percTable(HairEyeColor-table, margins = 1) the sum row therefore read
# 343.8%, 335.9%, 145.3%, 100.0% instead of 37.2%, 36.3%, 15.7%, 10.8%.
.addPercTableMargins <- function(tables, mar) {

  hasFreq <- "freq" %in% names(tables)
  total <- if (hasFreq) sum(tables[["freq"]]) else NA_real_
  freqWithMar <- if (hasFreq) addmargins(tables[["freq"]], margin = mar) else NULL

  tableNames <- names(tables)

  out <- lapply(tableNames, function(nm) {
    tab <- tables[[nm]]
    if (hasFreq && nm %in% c("p.row", "p.col", "perc")) {
      result <- addmargins(tab, margin = mar)
      if (1 %in% mar)
        result[nrow(result), ] <- freqWithMar[nrow(freqWithMar), ] / total
      if (2 %in% mar)
        result[, ncol(result)] <- freqWithMar[, ncol(freqWithMar)] / total
      result
    } else {
      addmargins(tab, margin = mar)
    }
  })

  names(out) <- tableNames
  out
}
