
#' Percentage Table
#' 
#' Prints a 2-way contingency table along with percentages, marginal, and
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
#' style in which numbers are formatted is selected by \code{\link[aurora]{style}()} from
#' the DescToolsX options. Absolute frequencies will use \code{style("abs.sty")} and
#' \code{style("per.sty")} will do it for the percentages. The options can be changed
#' with \code{style(abs, digits=5)} which is basically a \code{"style"}-object
#' containing any format information used in \code{\link[aurora]{fm}}.
#' 
#' \code{Margins()} returns a list containing all the one dimensional margin
#' tables of a n-dimensional table along the given dimensions. It uses
#' \code{\link{margin.table}()} for all the dimensions and adds the appropriate
#' percentages.
#' 
#' @name percTable
#' @inheritParams Association
#' @aliases percTable percTable.default percTable.table percTable.formula percTable.matrix print.PercTable
#' 
#' @param row.vars a vector of row variables (see Details). 
#' @param col.vars a vector of column variables (see Details). If this is left
#' to \code{NULL} the table structure will be preserved. 
#' @param justify either \code{"left"} or \code{"right"} for defining the
#' alignment of the table cells. 
#' @param freq boolean. Should absolute frequencies be included? Defaults to
#' TRUE. 
#' @param prop a string defining the conditional propotions to be displayed. 
#' Can be \code{"rows"} for rowwise percentages \code{"cols"} for columnwise
#' or both of them.
#' produces a table output with row and column
#' percentages. 
#' @param expected the expected counts under the null hypothesis.
#' @param margins a vector, consisting out of 1 and/or 2. Defines the margin
#' sums to be included.  1 stands for row margins, 2 for column margins, c(1,2)
#' for both. Default is \code{NULL} (none). 
#' @param formula a formula of the form \code{lhs ~ rhs} where \code{lhs} will
#' be tabled versus rhs (\code{table(lhs, rhs)}).
#' @param data an optional matrix or data frame (or similar: see
#' \code{\link{model.frame}}) containing the variables in the formula
#' \code{formula}.  By default the variables are taken from
#' \code{environment(formula)}.
#' @param subset an optional vector specifying a subset of observations to be
#' used.
#' @param na.action a function which indicates what should happen when the data
#' contain NAs. Defaults to \code{getOption("na.action")}.
#' @param blockSep logical, defining if an empty row should be introduced between
#' the table rows. Default is FALSE, if only a table with one single
#' description (either frequencies or percents) should be returned and
#' \code{TRUE} in any other case. 
#' @param \dots the dots are passed to \code{print.PercTable()} 
#' 
#' @return Returns an object of class \code{"ftable"}. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{freq}}, \code{\link{table}}, \code{\link{ftable}},
#' \code{\link{proportions}}, \code{\link{addmargins}},
#' \code{\link{setDescToolsXOption}}, \code{\link[aurora]{style}}\cr There are similar
#' functions in package \pkg{sfsmisc} \code{\link[sfsmisc]{printTable2}} and
#' package \pkg{vcd} \code{\link[vcd]{table2d_summary}}, both lacking some of
#' the flexibility we needed here. \cr
#' @references Agresti, Alan (2007) \emph{Introduction to categorical data
#' analysis}. NY: John Wiley and Sons, Section 2.4.5\cr
#' @keywords multivariate
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
#' percTable(tab, col.vars=2, prob=NULL)
#' 
#' # just the percentages without absolute values
#' percTable(tab, col.vars=2, prob=c("total","rows"), freq=FALSE)
#' 
#' # just the row percentages
#' percTable(tab, freq= FALSE, prob="rows")
#' 
#' # just the expected frequencies and the standard residuals
#' percTable(tab, prob=NULL, expected = TRUE)
#' 
#' 
#' # rearrange output such that freq are inserted as columns instead of rows
#' percTable(tab, col.vars=c(3,2))
#' 
#' # putting the areas in rows
#' percTable(tab, col.vars=c(3,1), prob="total", margins=c(1,2))
#' 
#' # formula interface with subset
#' percTable(driver ~ area, data=d.pizza, subset=wine_delivered==0)
#' 
#' # sort the table by rows, order first column (Zurich), then third, then row.names (0)
#' percTable(sortX(tab, ord=c(1,3,0)))
#' 
#' # reverse the row variables, so that absolute frequencies and percents
#' # are not nested together
#' percTable(tab, row.vars=c(3, 1))
#' 
#' # the vector interface
#' percTable(x=d.pizza$driver, y=d.pizza$area)
#' percTable(x=d.pizza$driver, y=d.pizza$area, prop="rows", 
#' margins=c("rows","cols"))
#' 
#' # one dimensional x falls back to the function freq()
#' # percTable(x=d.pizza$driver)
#' 
 

#' @family freq.tables
#' @concept frequency-analysis
#' @concept descriptive-statistics
#' @concept table-manipulation
#'
#'
#' @export
percTable <- function (...) UseMethod("percTable")


#' @rdname percTable
#' @export 
percTable.default <- function (x, y = NULL, ...) {
  
  # all arguments which match percTable.table or print function
  percTableArgs <- names(c(formals(percTable.table), 
                           formals(print.PercTable)))
  
  # all dot arguments
  dot.args <- match.call(expand.dots=FALSE)$...
  # the dot arguments which match PercTable.table
  pt.args <- dot.args[names(dot.args) %in% percTableArgs ]
  # the dot arguments which DO NOT match PercTable.table
  tab.args <- dot.args[names(dot.args) %notin% percTableArgs ]

  if(is.null(y)){
    tab <- do.call("table", append(list(x), tab.args) )
  } else {
    tab <- do.call("table", append(list(x, y), tab.args) )
  }
  
  do.call( "percTable", append(list(x=tab), pt.args) )
  
}

# PercTable.data.frame <- function(x, ...){  sapply(x, PercTable, ...) }


#' @rdname percTable
#' @export
percTable.formula <- function(formula, data, subset, na.action, ...) {
  
  # this is taken basically from wilcox.test.formula
  
  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L))
    stop("'formula' missing or incorrect")
  m <- match.call(expand.dots = FALSE)
  if (is.matrix(eval(m$data, parent.frame())))
    m$data <- as.data.frame(data)
  m[[1L]] <- as.name("model.frame")
  m$... <- NULL
  mf <- eval(m, parent.frame())
  DNAME <- paste(names(mf), collapse = " by ")
  
  DATA <- list(table(mf))
  do.call("percTable", c(DATA, list(...)))
}



#' @rdname percTable
#' @export
percTable.table <- function(x, freq=TRUE, 
                            prop = c("rows", "cols", "total"), 
                            expected = FALSE, ...) {
  
  
  if (!is.array(x) ||
      length(dim(x)) != 2 ||
      any(dim(x) == 0) ||
      !isWholeLike(x, nonNegative = TRUE)) {
    
    stop("'x' must be a 2-dimensional array of non-negative integer-like counts.")
  }
  
  prop <- match.arg(prop, c("rows","cols","total","none"), several.ok = TRUE)
  
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
  
  if(is.null(row.vars)){
    if(is.null(col.vars)){
      # neither row.vars, nor col.vars provided, set defaults
      row.vars <- c(1, 3)
      col.vars <- 2
    } else
      # default row.vars  with given col.vars 
      row.vars <- setdiff(1:3, col.vars)
  } else {      
    if(is.null(col.vars))
      # default col.vars with given row.vars
      col.vars <- setdiff(1:3, row.vars)
    # else: both are defined by the user
  }
  
  if (!is.null(mar)) {
    if(!is.numeric(mar))
      mar <- match(mar, c("rows", "cols"))
    
    hasFreq <- "freq" %in% names(tables)
    
    tableNames <- names(tables)  # Namen vorher sichern
    tables <- lapply(tableNames, function(nm) {
      tab <- tables[[nm]]
      if (nm %in% c("p.row", "p.col", "perc")) {
        if (hasFreq) {
          freqWithMar <- addmargins(tables[["freq"]], margin = mar)
          total <- freqWithMar[nrow(freqWithMar), ncol(freqWithMar)]
          result <- addmargins(tab, margin = mar)
          if (1 %in% mar)
            result[nrow(result), ] <- freqWithMar[nrow(freqWithMar), ] / total
          if (2 %in% mar)
            result[, ncol(result)] <- freqWithMar[, ncol(freqWithMar)] / total
          result
        } else {
          addmargins(tab, margin = mar)
        }
      } else {
        addmargins(tab, margin = mar)
      }
    })
    names(tables) <- tableNames  # Namen direkt wiederverwenden
  }  
  
  abstab <- names(tables) %in% c("freq", "expected")
  tables[abstab] <- lapply(tables[abstab], fm, fmt="abs.sty")
  ptab <-  names(tables) %in% c("perc", "p.row", "p.col")
  tables[ptab] <- lapply(tables[ptab], fm, fmt="per.sty")
  
  if (!is.null(mar)) {
    hasPerc <- "perc" %in% names(tables)
    condTabs <- names(tables) %in% c("p.row", "p.col")
    tables[condTabs] <- lapply(names(tables[condTabs]), function(nm) {
      tab <- tables[[nm]]
      if (nm == "p.row") {
        if (hasPerc) {
          # perc already shows all margins → suppress both in p.row
          if (1 %in% mar) tab[nrow(tab), ] <- "."
          if (2 %in% mar) tab[, ncol(tab)] <- "."
        } else {
          # standalone p.row: sum column is meaningless (adds to >100%)
          if (2 %in% mar) tab[, ncol(tab)] <- "."
        }
      }
      if (nm == "p.col") {
        if (hasPerc) {
          # perc already shows all margins → suppress both in p.col
          if (1 %in% mar) tab[nrow(tab), ] <- "."
          if (2 %in% mar) tab[, ncol(tab)] <- "."
        } else {
          # standalone p.col: sum row is meaningless (adds to >100%)
          if (1 %in% mar) tab[nrow(tab), ] <- "."
        }
      }
      tab
    })
  }
  
  if (length(tables) == 1) {
    out <- ftable(tables[[1]])
  } else {
    arr <- abind(tables, along = 3)
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
  
  invisible(txt)
  
}




#' @rdname percTable
#' @export
percTable.matrix <- percTable.table

