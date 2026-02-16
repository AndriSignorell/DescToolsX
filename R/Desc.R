#' Describe Data
#'
#' Produce summaries of various types of variables. Descriptive statistics
#' and plots are chosen automatically depending on the class of \code{x}.
#' The intention is to provide a fast but rich summary with minimal typing.
#'
#' \code{Desc()} is an S3 generic that computes basic descriptive statistics
#' depending on the class of its input. The result is an object of class
#' \code{"Desc"} with a more specific subclass such as
#' \code{"Desc.numeric"}, \code{"Desc.factor"} or \code{"Desc.data.frame"}.
#'
#' For numeric vectors, summary statistics such as mean and standard deviation
#' are computed. For factors, frequency tables are returned. For data frames,
#' \code{Desc()} is applied column-wise.
#'
#' @name Desc
#' @aliases Desc Desc.data.frame
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
#' \code{Desc} is a \strong{generic function}. It dispatches to the
#' method of the class of its first argument. Typing
#' \code{?Desc} + TAB at the prompt should present a choice of links: the help
#' pages for each of these \code{Desc} methods. You don't need to 
#' use the full name of the method although you may if you wish; 
#' i.e., \code{Desc(x)} is idiomatic R but you can bypass method 
#' dispatch by going direct if you wish: \code{Desc.numeric(x)}.

#' The most frequent object types are supported, find more specific help in:
#' \verb{  }Numeric variables: \code{\link{Desc.numeric}}\cr
#' \verb{  }Factors and character vectors: \code{\link{Desc.factor}}\cr
#' \verb{  }Boolean or dichotomous variables: \code{\link{Desc.logical}}\cr
#' \verb{  }Contingency tables and frequencies: \code{\link{Desc.table}}\cr
#' \verb{  }Calender date variables: \code{\link{Desc.Date}}\cr
#' \verb{  }Time series: \code{\link{Desc.ts}}\cr
#' 
#' 
#' @return An object of class \code{"Desc"} with a subclass depending on
#'   the input type.
#'
#' @seealso \code{\link{summary}}, \code{\link{plot}}
#' @keywords descriptive statistics


#' @rdname Desc
#' @export
Desc <- function(x, ...) {
  UseMethod("Desc")
}


#' @rdname Desc
#' @method Desc list
#' @export
Desc.list <- function(x, ...) {
  
  res <- lapply(names(x), function(nm) {
    Desc(x[[nm]], main=nm, ...)
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


#' @rdname Desc
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


#' @rdname Desc
#' @method Desc data.frame
#' @export
Desc.data.frame <- function(x, ...) {
  
  res <- Desc.list(as.list(x), ...)
  
  abst <- abstract(x)
      attr(abst, which="main") <- 
        cli::style_bold(gettextf("Describe %s (%s):", 
                                 deparse(substitute(x)),
                                 class(x)))
  res$abstract <- abst     
  
  return(res)
      
}


#' @rdname Desc
#' @export
descX <- Desc

#' @rdname Desc
#' @export
desc <- Desc




# == internal helper functions ===============================================

.descMeta <- function(x, xname, main, plotit, verbose) {
  
  list(
    xname      = xname,
    label      = Label(x),
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
    
    header <- gettextf("%s (%s)", meta$main, naReplace(meta$class, "-")) 
    
    if (.has_color()) 
      header <- cli::style_bold(header)
    
    cat(header) 
  }
  
  if (!is.null(meta$label)) {
    cat(" :", strwrap(meta$label, indent = 2, exdent = 2), sep = "\n")
    cat("\n")  
  } else {
    cat("\n")
  }
  
  cat("\n")  
  
  
}












