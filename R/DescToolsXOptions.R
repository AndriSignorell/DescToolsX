
#' Set DescToolsX Options
#'
#' Set one or more package options. Each supplied name is stored with the
#' `"DescToolsX."` prefix and can be read with [getOption()].
#'
#' Options used by descriptive methods include:
#' \describe{
#'   \item{`plotit`}{whether descriptive functions create plots by
#'     default}
#'   \item{`footnote`}{characters used as footnote markers}
#'   \item{`lang`}{language used for month and weekday names}
#' }
#'
#' @param \dots named option values
#'
#' @return a named list containing the previous values, invisibly
#'
#' @seealso [options()], [getOption()]
#'
#' @examples
#' old <- getOption("DescToolsX.plotit")
#' setDescToolsXOption(plotit = FALSE)
#' getOption("DescToolsX.plotit")
#' options(DescToolsX.plotit = old)
#' 



#' @family utils  
#' @concept programming
#'
#'
#' @export
setDescToolsXOption <- function(...) {
  opts <- list(...)
  if (length(opts) == 0L || is.null(names(opts)) || any(names(opts) == ""))
    stop("all options must be supplied as name = value")
  names(opts) <- paste0("DescToolsX.", names(opts))
  # options() returns the previous values invisibly
  options(opts)
}


# internal getOption wrapper for DescToolsX options
.getOption <- function(name, default = NULL) {
  getOption(paste0("DescToolsX.", name), default)
}

