
#' Create a Factor Variable by Cutting an Age Variable
#'
#' Dividing the range of an age variable `x` into intervals is a frequent
#' task in data analysis. The commonly used function [cut()] has
#' unfavourable default values for this. `cutAge()` is a convenient
#' wrapper for cutting age variables in groups of e.g. 10 years with more
#' suitable defaults.
#'
#'
#' @param x continuous variable
#' @param breaks either a numeric vector of two or more unique cut points or a
#' single number (greater than or equal to 2) giving the number of intervals
#' into which x is to be cut. Default is 10-year intervals from 0 to 90.
#' @param right logical, indicating if the intervals should be closed on the
#' right (and open on the left) or vice versa.  Default is `FALSE` -
#' unlike in [cut()]!
#' @param ordered_result logical: should the result be an ordered factor?
#' Default is `TRUE` - unlike in [cut()]!
#' @param full logical; whether to retain empty levels at the
#' edges of the distribution
#' @param labels labels for the levels. When set to `TRUE` the age ranges
#' will be 00-09, 10-19, 20-29, etc.
#' @param \dots further arguments passed to [cut()], for example
#' to change the labels
#' @return a factor, or an integer vector of level codes when
#' `labels = FALSE`
#'
#' Values which fall outside the range of breaks are coded as `NA`, as are
#' `NaN` and `NA` values.
#'
#' @seealso [cut()], [seq()]
#'
#' @examples
#'
#' set.seed(1)
#' desc(cutAge(sample(0:100, size = 100, replace = TRUE)))
#'
#' # readable labels
#' table(cutAge(c(3, 17, 42, 67, 95), labels = TRUE))
#'
#' # drop the empty groups at both ends
#' table(cutAge(c(42, 47, 51), labels = TRUE, full = FALSE))
#'
#' @family cut
#' @concept binning
#' @concept demographics
#' @export
cutAge <- function(x, breaks = c(seq(from = 0, to = 90, by = 10), Inf),
                   right = FALSE, ordered_result = TRUE, full = TRUE,
                   labels = NULL, ...) {

  if (identical(labels, TRUE)) {

    lower <- head(breaks, -1)
    upper <- head(breaks[-1], -1) - 1

    # formatC(), not fm(ldigits=): ldigits does not zero-pad, so the
    # labels came out as "0-9" rather than the documented "00-09". The
    # width follows the largest finite break, so breaks beyond 99 widen
    # every label consistently instead of ragged.
    wd <- max(2L, nchar(as.character(as.integer(
      max(c(lower, upper)[is.finite(c(lower, upper))])))))

    pad <- function(v) formatC(as.integer(v), width = wd, flag = "0")

    labels <- paste(pad(lower), c(pad(upper), ".."), sep = "-")
  }

  res <- cut(x, breaks = breaks,
             right = right, ordered_result = ordered_result,
             labels = labels, ...)

  if (!full && is.factor(res)) {

    used <- which(tabulate(as.integer(res), nbins = nlevels(res)) != 0)

    if (length(used)) {
      keep <- levels(res)[seq(min(used), max(used))]

      # factor() drops the ordering unless it is asked for again, so
      # cutAge(x, full = FALSE) silently returned an unordered factor
      # despite ordered_result = TRUE.
      res <- factor(res, levels = keep, ordered = is.ordered(res))
    }
  }

  return(res)
}
