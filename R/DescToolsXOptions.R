
#' Set DescToolsX Options
#'
#' Set one or more package options. Each supplied name is stored with the
#' \code{"DescToolsX."} prefix and can be read with \code{\link{getOption}()}.
#'
#' Options used by descriptive methods include:
#' \describe{
#'   \item{\code{plotit}}{whether descriptive functions create plots by
#'     default}
#'   \item{\code{footnote}}{characters used as footnote markers}
#'   \item{\code{lang}}{language used for month and weekday names}
#' }
#'
#' @param \dots named option values
#'
#' @return a named list containing the previous values, invisibly
#'
#' @seealso \code{\link{options}}, \code{\link{getOption}}
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
  stopifnot(length(opts) > 0)
  names(opts) <- paste0("DescToolsX.", names(opts))
  options(opts)
  # invisible(NULL)
}


# internal getOption wrapper for DescToolsX options
.getOption <- function(name, default = NULL) {
  getOption(paste0("DescToolsX.", name), default)
}

