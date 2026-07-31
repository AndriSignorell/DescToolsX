
#' Cut an Integer Variable into Intervals
#'
#' A \code{\link{cut}} method for integer vectors. \code{cut.default()}
#' labels its intervals in the mathematical \code{"(a,b]"} notation, which
#' is right for a continuous variable but reads awkwardly for counts: for
#' integers, \code{"11-20"} says the same thing more plainly than
#' \code{"(10,20]"}.
#'
#' @details
#' The integer labels are only constructed when \code{labels} is
#' \code{NULL} \emph{and} \code{breaks} is a vector of whole numbers. A
#' scalar \code{breaks} (a number of intervals) is passed straight to
#' \code{\link{cut.default}}, which computes the cut points itself, and
#' fractional break points fall back to the default interval notation as
#' well - \code{"34.3-66.6"} would suggest an integer range that does not
#' exist.
#'
#' Infinite outer breaks are rendered as \code{".."}, so
#' \code{breaks = c(0, 10, Inf)} yields \code{"1-10"} and \code{"11-.."}.
#'
#' @param x an integer vector
#' @param breaks either a vector of cut points or a single number giving
#'   the number of intervals
#' @param labels labels for the levels. The integer-style labels described
#'   above are used when this is \code{NULL}.
#' @param include.lowest,right,ordered_result,... passed to
#'   \code{\link{cut.default}}
#'
#' @return a factor of the same length as \code{x}
#'
#' @seealso \code{\link{cut}}, \code{\link{cutAge}}, \code{\link{cutQ}}
#'
#' @examples
#' x <- as.integer(c(1, 5, 10, 11, 20, 21))
#'
#' cut(x, breaks = c(0, 10, 20, Inf))
#'
#' # left-closed intervals shift the labels accordingly
#' cut(x, breaks = c(0, 10, 20, Inf), right = FALSE)
#'
#' # a scalar breaks is left to cut.default()
#' cut(x, breaks = 3)
#'
#' @family cut
#' @concept binning
#' @method cut integer
#' @export
cut.integer <- function(x, breaks, labels = NULL, include.lowest = FALSE,
                        right = TRUE, ordered_result = FALSE, ...) {

  # labels are constructed using "(a,b]" interval notation in cut.default,
  # which is perfectly fine for numeric variables, but not well suited for
  # integers, for which an explicit formulation is more appropriate

  .fmInf <- function(z) {
    out <- format(z, trim = TRUE, scientific = FALSE)
    out[!is.finite(z)] <- ".."
    out
  }

  # Only construct integer-style labels when breaks is already a vector of
  # whole numbers - a scalar is delegated to cut.default(), which computes
  # the break points first, and fractional breaks cannot be described by
  # an integer range.
  wholeBreaks <- length(breaks) > 1L &&
    all(is.infinite(breaks) | breaks %% 1 == 0)

  if (is.null(labels) && wholeBreaks) {

    from <- head(breaks, -1)
    to   <- breaks[-1]

    labels <- if (right)
      paste(.fmInf(from + 1), .fmInf(to), sep = "-")
    else
      paste(.fmInf(from), .fmInf(to - 1), sep = "-")

    # duplicated labels would make factor() fail with a message that
    # points at cut() rather than at the breaks
    if (anyDuplicated(labels))
      labels <- NULL
  }

  cut.default(x = x, breaks = breaks, labels = labels,
              include.lowest = include.lowest, right = right,
              ordered_result = ordered_result, ...)
}
