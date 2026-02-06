
#' Format Styles
#' 
#' Interface for format templates, defined as a list consisting of any accepted
#' format features in \code{fm()}.  This enables to define templates globally
#' and easily change or modify them later. 
#' 
#' \code{style()} can either create new styles or edit existing ones.
#' \code{style()} can be used to create new styles. It takes any of the
#' arguments from \code{fm()} and combines them to an object of class
#' \code{"Style"}, which then can be handed over to \code{fm()} as argument
#' \code{fmt}. \cr Following will define a new format template named
#' "\code{num.sty}".  Passed to \code{fm()} this will result in a number
#' displayed with 2 fixed digits and a comma as big mark: \preformatted{num.sty
#' <- style(digits=2, big.mark=",") fm(12222.89345, fmt=num.sty) = 12,222.89}
#' This is the same result as if the arguments would have been supplied
#' directly, but helps to avoid boilerplate code: \cr \code{fm(12222.89345,
#' digits=2, big.mark=",")}.
#' 
#' To edit a style we can provide \code{style()} with its name and overwrite,
#' resp. add new format options.  \code{style("num.sty", digits=1, sci=10)}
#' will use the current version of the numeric format and change the digits to
#' 1 and the threshold to switch to scientifc presentation to numbers > 1e10
#' and < 1e-10.
#' 
#' \code{styles()} returns all found style definitions in the global
#' environment or in the options.
#' 
#' The styles can be stored as options for convenience.  To store a new format
#' we use the default \code{options()} approach: \code{options(num.sty =
#' style(digits=1, big.mark=" "))} Defined styles in the options can be passed
#' on to \code{fm()} simply by their name.
#' 
#' Many report functions (e.g. \code{\link[DescTools]{TOne}()}) in \bold{DescTools} use
#' three default formats for counts (named \code{"abs.sty"}), numeric values
#' (\code{"num.sty"}) and percentages (\code{"per.sty"}).
#' 
#' @name style
#' @aliases style styles
#' @param x an object of class \code{Style} or a the name of a style, defined
#' either in the global enviroment or in the options.
#' @param digits integer, the desired (fixed) number of digits after the
#' decimal point. Unlike \code{\link{formatC}} you will always get this number
#' of digits even if the last digit is 0.  Negative numbers of digits round to
#' a power of ten (\code{digits=-2} would round to the nearest hundred).
#' @param ldigits number of leading zeros. \code{ldigits=3} would make sure
#' that at least 3 digits on the left side will be printed, say \code{3.4} will
#' be printed as \code{003.4}. Setting \code{ldigits} to \code{0} will yield
#' results like \code{.452} for \code{0.452}. The default \code{NULL} will
#' leave the numbers as they are (meaning at least one 0 digit).
#' @param sci integer. The power of 10 to be set when deciding to print numeric
#' values in exponential notation. Fixed notation will be preferred unless the
#' number is larger than 10^scipen. If just one value is set it will be used
#' for the left border 10^(-scipen) as well as for the right one (10^scipen). A
#' negative and a positive value can also be set independently. Default is
#' \code{getOption("scipen")}, whereas \code{scipen=0} is overridden.
#' @param big.mark character; if not empty used as mark between every 3
#' decimals before the decimal point. Default is "" (none).
#' @param outdec character, specifying the decimal mark to be used. If not
#' provided, the default set as \code{OutDec} option is used.
#' @param na.form character, string specifying how \code{NA}s should be
#' specially formatted.  If set to \code{NULL} (default) no special action will
#' be taken.
#' @param zero.form character, string specifying how zeros should be specially
#' formatted. Useful for pretty printing 'sparse' objects.  If set to
#' \code{NULL} (default) no special action will be taken.
#' @param fmt either a format string, allowing to flexibly define special
#' formats or an object of class \code{style}, consisting of a list of
#' \code{fdm} arguments. See Details.
#' @param p_eps a numerical tolerance used mainly for formatting p values,
#' those less than p_eps are formatted as "\verb{\code{< [p_eps]}}" (where '\verb{[p_eps]}'
#' stands for \code{format(p_eps, digits))}.  Default is \code{0.001}.
#' @param width integer, the defined fixed width of the strings.
#' @param align the character on whose position the strings will be aligned.
#' Left alignment can be requested by setting \code{sep = "\\l"}, right
#' alignment by \code{"\\r"} and center alignment by \code{"\\c"}. Mind the
#' backslashes, as if they are omitted, strings would be aligned to the
#' \bold{character} l, r or c respectively. The default is \code{NULL} which
#' would just leave the strings as they are.\cr This argument is send directly
#' to the function \code{\link{strAlign}()} as argument \code{sep}.
#' @param lang optional value setting the language for the months and daynames.
#' Can be either \code{"local"} for current locale or \code{"en"} for english.
#' If left to \code{NULL}, the DescToolsOption \code{"lang"} will be searched
#' for and if not found \code{"local"} will be taken as default.
#' @param label a description for the style
#' @param \dots further arguments to be passed to or from methods.
#' @return \code{style()} returns an object of class \code{Style}\cr
#' \code{styles()} returns a list of styles
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{fm}()}
#' @keywords IO
#' @examples
#' 
#' # use style() to get and define new formats stored as option
#' num.sty <- style(digits=2, big.mark=" ")
#' abs.sty <- style(digits=0, big.mark=" ")
#' dat.sty <- style(fmt="MM, dd yyyy")
#' 
#' num.sty                             # displays the details of the style
#' # editing styles
#' style("abs.sty")                    # looks for format "abs.sty"
#' style("nexist")                     # return for nonexisting style
#' style("abs.sty", big.mark="")       # get Style("abs") and overwrite big.mark
#' style("abs.sty", na.form="-")       # get Style("abs") and add user defined na.form
#' 
#' styles()                            # all defined formats
#' styles()[c("num.sty", "abs.sty")]   # numeric and integer styles
#' 
#' # define totally new format and store as option
#' options(nob.sty=style(digits=5, na.form="nodat"))
#' 
#' # using styles
#' fm(314.1563, fmt=abs.sty)
#' fm(314.1563, fmt=num.sty)
#' 
#' fm(Today(), fmt=dat.sty)
#' 

#' @rdname style
#' @export
styles <- function(){
  
  # all styles found in environment
  env <- ls(envir = .GlobalEnv)
  
  # check if completely empty
  if(!identical(env, character(0))){
    
    found <- env[
      sapply(env, function(x) 
        inherits(get(x, envir = .GlobalEnv), "Style"))
    ]
    
    res_env <- setNamesX(lapply(found, get), names=found)
    res_env <- lapply(res_env, function(x) {
      attr(x, "source") <- "GlobalEnv"
      x
    })
  } else {
    res_env <- NULL
  }
  
  # all styles defined in R options
  opt <- options()
  res_opt <- opt[sapply(opt, class) == "Style"]
  res_opt <- lapply(res_opt, function(x) {
    attr(x, "source") <- "options"
    x
  })
  
  # return all found styles 
  res <- append(res_env, res_opt)
  
  return(res)
  
}



# defined styles (defaults in options()) used by reporting functions
# abs.sty <- Coalesce(
#                Styles("abs"),
#                Style(digits=0, big.mark = .thousands_sep))
# num.sty <- Coalesce(
#                Styles("num"), 
#                Style(digits=3, big.mark = .thousands_sep))
# perc.sty <- Coalesce(
#                 Styles("perc"),
#                 Style(fmt="%", digits=1))
# pval.sty <- Coalesce(
#                 Styles("pval"),
#                 Style(fmt="pval", p_eps=3))



#' @rdname style
#' @export
style <- function( x, digits = NULL, ldigits = NULL, sci = NULL
                   , big.mark=NULL, outdec = NULL
                   , na.form = NULL, zero.form = NULL
                   , fmt = NULL, p_eps = NULL
                   , width = NULL, align = NULL
                   , lang = NULL
                   , label = NULL
                   , ...){
  
  
  # following does not much more than return the non null provided arguments 
  # in a new class <style>
  
  # all function arguments, same arguments as Fm() uses
  # (for default values, we would use: a <- formals(get("Style", pos=1)))
  
  # so get all arguments from the Style() function
  a <- methods::formalArgs(style)
  
  # remove dots name from the list
  a <- a[a %nin% c("x","label","...")]
  
  # get the values of all the arguments
  v <- sapply(a, dynGet)
  
  # get rid of NULLs and append dots again
  res <- c(v[!sapply(v, is.null)],
           unlist(match.call(expand.dots=FALSE)$...))    
  
  sty <- NA
  if(!missing(x)){
    if(inherits(x, "Style"))
      sty <- x
    else if(is.character(x)){
      if(x %in% names(styles()))
        sty <- styles()[[x]]
    }
    
    if(!identical(sty, NA)) {
      # a style with the given <name> has been found in the options
      # overwrite or append separately provided arguments
      sty[names(res)] <- res
    } else {
      warning("Style x could not be found!")
    } 
    
    res <- sty    
  }
  
  if(!is.null(label))
    Label(res) <- label
  
  class(res) <- "Style"
  return(res)
  
}




#' @rdname style
#' @export
print.Style <- function(x, ...){
  
  CollapseList <- function(x){
    z <- x
    # opt <- options(useFancyQuotes=FALSE); on.exit(options(opt))
    z[unlist(lapply(z, inherits, "character"))] <- shQuote(z[unlist(lapply(z, inherits, "character"))])
    z <- paste(names(z), "=", z, sep="", collapse = ", ")
    
    return(z)
  }
  
  cat(gettextf("Format name:    %s%s\n", attr(x, "fmt_name"), 
               ifelse(identical(attr(x, "default"), TRUE), " (default)", "")),  
      gettextf("Description:   %s\n", Label(x)),
      gettextf("Definition:    %s\n", CollapseList(x)),
      gettextf("Example:       %s\n", fm(pi * 1e5, fmt=x)),
      sep = ""
  )
  if(!is.null(attr(x, "source"))){
    cat(cli::col_silver(gettextf("(Source:       %s)\n", attr(x, "source"))))
  }
  
  
}

