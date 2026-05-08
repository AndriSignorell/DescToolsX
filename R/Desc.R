#' Describe Data
#'
#' Produce summaries of various types of variables. Descriptive statistics
#' and plots are chosen automatically depending on the class of \code{x}.
#' The intention is to provide a fast but rich summary with minimal typing.
#'
#' \code{desc()} is an S3 generic that computes basic descriptive statistics
#' depending on the class of its input. The result is an object of class
#' \code{"desc"} with a more specific subclass such as
#' \code{"desc.numeric"}, \code{"desc.factor"} or \code{"desc.data.frame"}.
#'
#' For numeric vectors, summary statistics such as mean and standard deviation
#' are computed. For factors, frequency tables are returned. For data frames,
#' \code{desc()} is applied column-wise.
#'
#' @name desc
#' @aliases desc desc.data.frame desc.list
#' 
#' @param x object to be described. 
#' 
#' @param main Character string (character|\code{NULL}|\code{NA}) 
#' defining the main title. By default (\code{main = NULL}) the title will 
#' be composed as: <variable name> (<class(es)>). 
#' If \code{NA}, no title is printed.
#'
#' @param plotit logical. Should a plot be created? The plot type depends 
#' on the classes of the variables. Default can be defined by 
#' the option \code{plotit}, if it does not exist then it's set to \code{TRUE}.
#' 
#' @param verbose
#' Integer controlling verbosity of table output.
#' One of \code{1} (minimal), \code{2} (default), \code{3} (extensive).
#' Applies to tables only.
#' 
#' @param conf.level Confidence level of the interval (default 0.95). 
#' If set to \code{NA} (which is the default) no confidence interval 
#' will be calculated.
#'
#' @details
#' \code{desc} is a \strong{generic function}. It dispatches to the
#' method of the class of its first argument.
#'
#' Typing \code{?desc} + TAB at the prompt lists all available methods.
#' You usually call \code{desc(x)}, but direct calls like
#' \code{desc.numeric(x)} are also possible.
#'
#' \strong{Univariate descriptions}
#' \itemize{
#'   \item Numeric variables: \code{\link{desc.numeric}}
#'   \item Factors and character vectors: \code{\link{desc.factor}}
#'   \item Boolean variables: \code{\link{desc.logical}}
#'   \item Contingency tables: \code{\link{desc.table}}
#'   \item Dates: \code{\link{desc.Date}}
#'   \item Time series: \code{\link{desc.ts}}
#' }
#'
#' \strong{Bivariate descriptions}
#' \itemize{
#'   \item numeric ~ numeric: \code{\link{desc.nn}}
#'   \item numeric ~ qualitative: \code{\link{desc.nq}}
#'   \item qualitative ~ numeric: \code{\link{desc.qn}}
#'   \item qualitative ~ qualitative: \code{\link{desc.qq}} 
#'      (wrapper around \code{\link{desc.table}})
#' }
#' 
#' \strong{Design}
#' The \code{desc} system separates:
#' \itemize{
#'   \item computation (internal \code{.desc_*} functions)
#'   \item printing (\code{print.Desc.*})
#'   \item visualization (\code{plot.Desc.*})
#' }' 
#'  
#' @return An object of class \code{"Desc"} with a subclass depending on
#'   the input type.
#'   @return An object of class \code{"Desc"} with a subtype depending on
#'   the input (e.g. \code{"Desc.numeric"}, \code{"Desc.qn"}).
#'
#' @seealso \code{\link{summary}}, \code{\link{plot}}

#' @rdname desc
#' @family desc
#' @concept data-description
#' @concept descriptive-statistics
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











