#' Describe Data
#'
#' Produce summaries of various types of variables. Descriptive statistics
#' and plots are chosen automatically depending on the class of `x`.
#' The intention is to provide a fast but rich summary with minimal typing.
#'
#' `desc()` is an S3 generic that computes basic descriptive statistics
#' depending on the class of its input. The result is an object of class
#' `"desc"` with a more specific subclass such as
#' `"desc.numeric"`, `"desc.factor"` or `"desc.data.frame"`.
#'
#' For numeric vectors, summary statistics such as mean and standard deviation
#' are computed. For factors, frequency tables are returned. For data frames,
#' `desc()` is applied column-wise.
#'
#' @name desc
#' @aliases desc desc.data.frame desc.list
#' 
#' @param x object to be described
#' 
#' @param main character string, `NULL`, or `NA`, defining the main
#' title. By default (`main = NULL`) the title will
#' be composed as: <variable name> (<class(es)>). 
#' If `NA`, no title is printed.
#'
#' @param plotit logical. Should a plot be created? The plot type depends 
#' on the classes of the variables. Default can be defined by 
#' the option `plotit`, if it does not exist then it's set to `TRUE`.
#' 
#' @param verbose
#' integer controlling verbosity of table output.
#' One of `1` (minimal), `2` (default), `3` (extensive).
#' Applies to tables only.
#' 
#' @param conf.level confidence level of the interval (default 0.95).
#' If set to `NA`, no confidence interval is calculated.
#'
#' @details
#' `desc` is a **generic function**. It dispatches to the
#' method of the class of its first argument.
#'
#' Typing `?desc` + TAB at the prompt lists all available methods.
#' You usually call `desc(x)`, but direct calls like
#' `desc.numeric(x)` are also possible.
#'
#' **Univariate descriptions**
#' \itemize{
#'   \item Numeric variables: [desc.numeric()]
#'   \item Factors and character vectors: [desc.factor()]
#'   \item Boolean variables: [desc.logical()]
#'   \item Contingency tables: [desc.table()]
#'   \item Dates: [desc.Date()]
#'   \item Time series: [desc.ts()]
#' }
#'
#' **Bivariate descriptions**
#' \itemize{
#'   \item numeric ~ numeric: [desc.nn()]
#'   \item numeric ~ qualitative: [desc.nq()]
#'   \item qualitative ~ numeric: [desc.qn()]
#'   \item qualitative ~ qualitative: [desc.qq()] 
#'      (wrapper around [desc.table()])
#' }
#' 
#' **Design**
#' The `desc` system separates:
#' \itemize{
#'   \item computation (internal `.desc_*` functions)
#'   \item printing (`print.Desc.*`)
#'   \item visualization (`plot.Desc.*`)
#' }
#'  
#' @return an object of class `"Desc"` with a subclass determined by the
#' input, such as `"Desc.numeric"` or `"Desc.qn"`
#'
#' @seealso [summary()], [plot()]

#' @rdname desc

#' @family descriptive  
#' @concept summary
#'
#'
#' @export
desc <- function(x, ...) {
  UseMethod("desc")
}


#' @rdname desc
#' @method desc list
#' @export
desc.list <- function(x, ...) {
  
  res <- lapply(names(x), function(nm) {
    desc(x[[nm]], main=nm, ...)
  })
  
  names(res) <- names(x)
  
  structure(
    list(
      data = res,
      n = length(x)
    ),
    class = c("Desc.list", "Desc")
  )
}


#' @rdname desc
#' @method print Desc.list
#' @export
print.Desc.list <- function(x, ...) {
  
  if(!is.null(x$abstract))
    print(x$abstract)
  
  for (nm in names(x$data)) {
    print(x$data[[nm]])
  }
  
  invisible(x)
}


#' @rdname desc
#' @method desc data.frame
#' @export
desc.data.frame <- function(x, ...) {
  
  res <- desc.list(as.list(x), ...)
  
  abst <- abstract(x)
      attr(abst, which="main") <- 
        cli::style_bold(gettextf("Describe %s (%s):", 
                                 deparse(substitute(x)),
                                 class(x)))
  res$abstract <- abst     
  
  return(res)
      
}



#' @rdname desc
#' @exportS3Method
print.Desc <- function(x, ...) {
  
  for (i in seq_along(x)) {
    print(x[[i]], ...)
    
    if(x[[i]]$meta$plotit)  plot(x[[i]])
    
  }
  
  invisible(x)
}



#' @rdname desc
#' @exportS3Method
plot.Desc <- function(x, ...) {
  
  for (i in seq_along(x)) {
    plot(x[[i]], ...)
  }
  
  invisible(x)
}



# == internal helper functions ===============================================

.descMeta <- function(x, xname, main, plotit, verbose) {

    list(
      xname      = xname,
      label      = label(x),
      main       = main %||% xname,
      class      = class(x),
      classlabel = paste(class(x), collapse = ","),
      plotit     = plotit %||% .getOption("plotit", FALSE),
      call       = match.call(),
      timestamp  = Sys.time(),
      verbose    = verbose %||% getOption("Desc.verbose", 2)
    )

}



.printHeader <- function(meta) {
  
  cat(lineSep(), "\n")
  
  if (!identical(meta$main, NA)) {
    
    header <- gettextf("%s (%s)", meta$main, 
                       paste(naReplace(meta$class, "-"), collapse=", ")) 
    
    if (.hasColor()) 
      header <- cli::style_bold(header)
    
    cat(header) 
  }
  
  if (!(is.null(meta$label) || is.na(meta$label))) {
    cat(" :", strwrap(meta$label, indent = 2, exdent = 2), sep = "\n")
    cat("\n")  
  } else {
    cat("\n")
  }
  
  cat("\n")  

}



.ChisqWarning <- function(){
  cat(cli::col_red("\nWarning message:\n  Exp. counts < 5: Chi-squared approx. may be incorrect!!\n\n"))
}









