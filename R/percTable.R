
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
#' style in which numbers are formatted is selected by \code{\link{style}()} from
#' the DescTools options. Absolute frequencies will use \code{style("abs.sty")} and
#' \code{style("per.sty")} will do it for the percentages. The options can be changed
#' with \code{style(abs, digits=5)} which is basically a \code{"style"}-object
#' containing any format information used in \code{\link{fm}}.
#' 
#' \code{Margins()} returns a list containing all the one dimensional margin
#' tables of a n-dimensional table along the given dimensions. It uses
#' \code{\link{margin.table}()} for all the dimensions and adds the appropriate
#' percentages.
#' 
#' @name percTable
#' @aliases PercTable PercTable.default PercTable.table PercTable.formula
#' PercTable.matrix Margins print.PercTable
#' @param x,y objects which can be interpreted as factors (including character
#' strings).  x and y will be tabulated via \code{table(x, y)}.\cr If x is a
#' matrix, it will be coerced to a table via as.table(x). 
#' @param tab a r x c-contingency table %% ~~Describe \code{tab} here~~
#' @param row.vars a vector of row variables (see Details). 
#' @param col.vars a vector of column variables (see Details). If this is left
#' to \code{NULL} the table structure will be preserved. 
#' @param justify either \code{"left"} or \code{"right"} for defining the
#' alignment of the table cells. 
#' @param freq boolean. Should absolute frequencies be included? Defaults to
#' TRUE. 
#' @param rfrq a string with 3 characters, each of them being 1 or 0. The first
#' position means total percentages, the second means row percentages and the
#' third column percentages. "011" produces a table output with row and column
#' percentages. 
#' @param expected the expected counts under the null hypothesis.
#' @param residuals the Pearson residuals, (observed - expected) /
#' sqrt(expected).
#' @param stdres standardized residuals, (observed - expected) / sqrt(V), where
#' V is the residual cell variance (for the case where x is a matrix, n * p *
#' (1 - p) otherwise).
#' @param margins a vector, consisting out of 1 and/or 2. Defines the margin
#' sums to be included.  1 stands for row margins, 2 for column margins, c(1,2)
#' for both. Default is \code{NULL} (none). 
#' @param digits integer. With how many digits shoud the relative frequencies
#' be formatted? Default can be set by \code{DescToolsOptions(digits=x)}.
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
#' @param vsep logical, defining if an empty row should be introduced between
#' the table rows. Default is FALSE, if only a table with one single
#' description (either frequencies or percents) should be returned and
#' \code{TRUE} in any other case. 
#' @param \dots the dots are passed from \code{PercTable.default()} to
#' \code{PercTable.table()} and from \code{Margins} to the function
#' \code{\link{Freq}}. 
#' 
#' @return Returns an object of class \code{"ftable"}. 
#' 
#' @author Andri Signorell <andri@@signorell.net> 
#' @seealso \code{\link{Freq}}, \code{\link{table}}, \code{\link{ftable}},
#' \code{\link{prop.table}}, \code{\link{addmargins}},
#' \code{\link{setDescToolsXOption}}, \code{\link{style}}\cr There are similar
#' functions in package \pkg{sfsmisc} \code{\link[sfsmisc]{printTable2}} and
#' package \pkg{vcd} \code{\link[vcd]{table2d_summary}}, both lacking some of
#' the flexibility we needed here. \cr
#' @references Agresti, Alan (2007) \emph{Introduction to categorical data
#' analysis}. NY: John Wiley and Sons, Section 2.4.5\cr
#' @keywords multivariate
#' @examples
#' 
#' tab <- table(driver=d.pizza$driver, area=d.pizza$area)
#' 
#' percTable(tab=tab, col.vars=2)
#' 
#' percTable(tab=tab, col.vars=2, margins=c(1,2))
#' percTable(tab=tab, col.vars=2, margins=2)
#' percTable(tab=tab, col.vars=2, margins=1)
#' percTable(tab=tab, col.vars=2, margins=NULL)
#' 
#' percTable(tab=tab, col.vars=2, rfrq="000")
#' 
#' # just the percentages without absolute values
#' percTable(tab=tab, col.vars=2, rfrq="110", freq=FALSE)
#' 
#' # just the row percentages in percent format (pfmt = TRUE)
#' percTable(tab, freq= FALSE, rfrq="010", pfmt=TRUE, digits=1)
#' 
#' # just the expected frequencies and the standard residuals
#' percTable(tab=tab, rfrq="000", expected = TRUE, stdres = TRUE)
#' 
#' 
#' # rearrange output such that freq are inserted as columns instead of rows
#' percTable(tab=tab, col.vars=c(3,2), rfrq="111")
#' 
#' # putting the areas in rows
#' percTable(tab=tab, col.vars=c(3,1), rfrq="100", margins=c(1,2))
#' 
#' # formula interface with subset
#' percTable(driver ~ area, data=d.pizza, subset=wine_delivered==0)
#' 
#' # sort the table by rows, order first column (Zurich), then third, then row.names (0)
#' percTable(tab=sortX(tab, ord=c(1,3,0)))
#' 
#' # reverse the row variables, so that absolute frequencies and percents
#' # are not nested together
#' percTable(tab, row.vars=c(3, 1))
#' 
#' # the vector interface
#' percTable(x=d.pizza$driver, y=d.pizza$area)
#' percTable(x=d.pizza$driver, y=d.pizza$area, margins=c(1,2), rfrq="000", useNA="ifany")
#' 
#' # one dimensional x falls back to the function Freq()
#' percTable(x=d.pizza$driver)
#' 
#' # the margin tables
#' # Margins(Titanic)
#' 

#' @rdname percTable
#' @export
percTable <- function (...) UseMethod("percTable")


#' @rdname percTable
#' @export 
percTable.default <- function (x, y = NULL, ...) {
  
  # all dot arguments
  dot.args <- match.call(expand.dots=FALSE)$...
  # the dot arguments which match PercTable.table
  pt.args <- dot.args[names(dot.args) %in% names(formals(percTable.table))]
  # the dot arguments which DO NOT match PercTable.table
  tab.args <- dot.args[names(dot.args) %nin% names(formals(percTable.table))]
  
  if(is.null(y)){
    tab <- do.call("table", append(list(x), tab.args) )
  } else {
    tab <- do.call("table", append(list(x, y), tab.args) )
  }
  do.call( "percTable", append(list(tab=tab), pt.args) )
  
}

# PercTable.data.frame <- function(x, ...){  sapply(x, PercTable, ...) }

#' @rdname percTable
#' @export
percTable.matrix <- function(x, ...){  percTable(as.table(x), ...) }


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
percTable.table <- function(tab, row.vars=NULL, col.vars = NULL, justify = "right"
                            , freq=TRUE, rfrq="100",
                            expected = FALSE, residuals = FALSE, stdres = FALSE, margins = NULL
                            , digits = NULL, ...) {
  
  # labels = c("Sum", "freq", "perc", "p.row", "p.col"),
  
  # example:
  # tab <- table(d.pizza[,c("driver","operator")])
  # PercTable(tab, rfrq="110", margins=c(1,2))
  
  # create prop tables and format them
  fmt.tab <- function(x, perc, w) {
    
    if(perc==1) {
      px <- addmargins(prop.table(addmargins(x, 1), 1), 2)
      if(1 %nin% margins) px <- px[,-ncol(px)]
      if(2 %nin% margins) px <- px[-nrow(px),]
      class(px) <- "table"
    } else if(perc==2) {
      px <- addmargins(prop.table(addmargins(x, 2), 2), 1)
      if(1 %nin% margins) px <- px[,-ncol(px)]
      if(2 %nin% margins) px <- px[-nrow(px),]
      class(px) <- "table"
    } else {
      px <- prop.table(x)
      if(!is.null(margins)) px <- addmargins(px, if(length(dim(x))==1) {1} else {3 - margins} )
    }
    
    # get the percent format from global option
    px[] <- fm(px, fmt=style("per.sty", digits=digits))
    
    # set 100% margins to some zero value
    # but only if main percentages are requested
    if(perc==1 & (1 %in% margins)) px[, ncol(px)] <- zero
    if(substr(rfrq, 1, 1)=="1")
      if(perc==2 & (1 %in% margins)) px[, ncol(px)] <- zero
    if(substr(rfrq, 1, 1)=="1")
      if(perc==1 & (2 %in% margins)) px[nrow(px), ] <- zero
    if(perc==2 & (2 %in% margins)) px[nrow(px), ] <- zero
    
    px
  }
  
  
  # set zero element
  zero <- "."
  
  # set default
  if(is.null(col.vars) && is.null(row.vars))
    col.vars <- 2
  
  tlst <- list(freq=tab)
  
  # overwrite percents if only 1-dim table
  if(length(dim(tab)) == 1) rfrq <- paste(sum(as.numeric(rfrq) > 0), "00", sep="")
  
  if(unlist(strsplit(rfrq, NULL))[1] == "1")
    tlst[["perc"]] <- fmt.tab(tab, perc=0)
  if(unlist(strsplit(rfrq, NULL))[2] == "1")
    tlst[["p.row"]] <- fmt.tab(tab, perc=1)
  if(unlist(strsplit(rfrq, NULL))[3] == "1")
    tlst[["p.col"]] <- fmt.tab(tab, perc=2)
  
  # flip 1 to 2 and 2 to 1 in margins with: 3 - margins
  if(!is.null(margins)) tlst[["freq"]] <- addmargins(tab, if(length(dim(tab))==1) {1} else {3 - margins})
  
  # format tab as.character
  tlst[["freq"]][] <- fm(tlst[["freq"]], fmt=style("abs.sty"))
  if(freq == FALSE) tlst[["freq"]] <- NULL
  
  if(any(is.na(tab))){
    na.tab <- tab
    na.tab[] <- NA
    r.chisq <- list(expected=na.tab, residuals=na.tab, stdres=na.tab)
  } else {
    suppressWarnings(r.chisq <- chisq.test(tab))
  }
  
  if(expected == TRUE) {
    tlst[["exp"]] <- fm(r.chisq$expected, fmt=style("num.sty"))
    if(1 %in% margins) tlst[["exp"]] <- cbind(tlst[["exp"]], Sum=zero)
    if(2 %in% margins) tlst[["exp"]] <- rbind(tlst[["exp"]], Sum=zero)
  }
  if(residuals == TRUE){
    tlst[["res"]] <- fm(r.chisq$residuals, fmt=style("num.sty"))
    if(1 %in% margins) tlst[["res"]] <- cbind(tlst[["res"]], Sum=zero)
    if(2 %in% margins) tlst[["res"]] <- rbind(tlst[["res"]], Sum=zero)
  }
  if(stdres == TRUE) {
    tlst[["stdres"]] <-  fm(r.chisq$stdres, fmt=style("num.sty"))
    if(1 %in% margins) tlst[["stdres"]] <- cbind(tlst[["stdres"]], Sum=zero)
    if(2 %in% margins) tlst[["stdres"]] <- rbind(tlst[["stdres"]], Sum=zero)
  }
  
  if(length(tlst) == 1){
    ftab <- ftable(tlst[[1]])
    
  } else {
    # if(length(dim(tab)) > 1){
    #   if(is.null(vsep))
    #     vsep <- length(tlst) > 1
    #   if(vsep)         # insert a separator line
    #     tlst[[""]] <- matrix("", nrow=nrow(tlst[[1]]), ncol=ncol(tlst[[1]]))
    # }
    
    # build a table array, such as to be able to pass it to ftable afterwards...
    if(length(dim(tab))==1){
      ma <- do.call("cbind", tlst)
    } else {
      ma <- do.call("abindX", c(tlst, along = 3, use.dnns = TRUE))
    }
    ftab <- ftable(ma, col.vars=col.vars, row.vars=row.vars)
    
  }
  
  justify <- match.arg(justify, c("left","right"))
  if(justify == "right")
    # align the whole stuff to the right
    ftab[] <- strAlign(ftab, "\\r")
  
  # names(attr(ftab, "row.vars"))[1] <- names(dimnames(tab))[1]
  # if(length(names(attr(ftab, "row.vars"))) == 2)
  #   names(attr(ftab, "row.vars"))[2] <- ""
  # names(attr(ftab, "col.vars")) <- names(dimnames(tab))[2]
  
  res <- list(ftab=ftab, tlst=tlst)
  vsep <- inDots(..., arg="vsep", default=ifelse(length(dim(tab)) == 1, FALSE, NA))
  if(!is.na(vsep)) res[["vsep"]] <- vsep
  
  class(res) <- c("PercTable")
  
  return(res)
  
}


#' @rdname percTable
#' @export
print.PercTable <- function(x, vsep=NULL, ...){
  
  # vsep <- Coalesce(vsep, x[["vsep"]], (length(x[["tlst"]]) > 1))
  # replaced by 0.99.21
  vsep <- Coalesce(vsep, x[["vsep"]], (length(attr(x[["ftab"]], "row.vars")) > 1))
  
  x <- x[["ftab"]]
  
  # cat(paste(c(rep(" ", times=max(nchar(c(names(attr(x, "row.vars")), attr(x, "row.vars")[[1]])), na.rm=TRUE) +
  #                   ifelse(length(attr(x, "row.vars")) == 2, max(nchar(attr(x, "row.vars")[[2]]), na.rm=TRUE) + 2, 0) + 1),
  #             names(attr(x, "col.vars"))), collapse=""), sep="\n")
  #
  # names(attr(x, "col.vars")) <- NULL
  #
  # txt <- capture.output(print(x, ...))
  # if(vsep)
  #   txt[StrLeft(txt,1) != " "][-1] <- paste("\n", txt[StrLeft(txt,1) != " "][-1], sep="")
  #
  # cat(txt, sep="\n")
  
  
  # replaced by 0.99.24, in order to have the table "line breaked" if necessary
  
  # get number of name cols
  nc <- length(attr(x, "row.vars"))
  x <- gsub("\"", "", format(x, method="col.compact"))
  for(i in 1:nc){
    x[, i] <- strPad(x[, i], max(nchar(x[,i])))
  }
  rn <- if(nc > 1) apply(x[, 1:nc], 1, paste, collapse=" ") else x[, 1]
  rownames(x) <- rn
  if(vsep){
    iempty <- which(substr(rownames(x), 1, 1) != " ")[-1]
    x <- do.call(rbind, lapply(splitAt(1:nrow(x), iempty), function(i) rbind(x[i,], "")))
  }
  
  x <- x[, -(1:nc)]
  colnames(x) <- rep("", ncol(x))
  
  print(x, quote=FALSE, right=TRUE, ...)
  
}

