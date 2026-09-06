
#' Count Work Days Between Two Dates
#'
#' Returns the number of work days between two dates taking into account the
#' provided holiday dates.
#'
#' The function is vectorised so that multiple initial and final dates can be
#' supplied. The date vectors are recycled if their lengths differ.
#'
#' @details
#' Both endpoints are counted, so `countWorkDays(d, d)` is 1 for a
#' working day and 0 otherwise. A `to` that lies before `from`
#' yields 0.
#'
#' Holidays outside the interval are ignored, as are holidays falling on a
#' non-work day; duplicated holiday dates are counted once.
#'
#' @param from initial dates
#' @param to final dates
#' @param holiday a vector of dates (or strings coercible with
#' [as.Date()]) to exclude
#' @param nonworkdays a character vector containing the English three-letter
#' weekday abbreviations to be treated as non-work days, i.e. a subset of
#' `c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")`. Default is
#' `c("Sat", "Sun")`.
#'
#' @return an integer vector
#'
#' @examples
#'
#' from <- as.Date("2019-01-01") + rep(0, 10)
#' to   <- as.Date("2019-01-15") + seq(0, 9)
#'
#' countWorkDays(from, to)
#'
#' # which weekdays are involved
#' x <- seq(min(from), max(to), by = "days")
#' head(data.frame(date = x, day = weekday(x, fmt = "dd", lang = "en")))
#'
#' # two holidays inside the period reduce the count by two
#' countWorkDays(from = min(x), to = max(x))
#' countWorkDays(from = min(x), to = max(x),
#'               holiday = c("2019-01-07", "2019-01-08"))
#'
#' # a single day
#' countWorkDays(as.Date("2019-01-05"), as.Date("2019-01-05"))  # Saturday: 0
#'
#' @family date.time
#' @concept date-time
#' @export
countWorkDays <- function(from, to,
                          holiday = NULL, nonworkdays = c("Sat", "Sun")) {

  dayAbb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

  # a typo such as "Sunday" used to be silently ignored, quietly turning
  # every weekend day into a working day
  if (!is.character(nonworkdays) || anyNA(nonworkdays) ||
      !all(nonworkdays %in% dayAbb))
    stop("'nonworkdays' must be a subset of ",
         paste(dQuote(dayAbb, FALSE), collapse = ", "))

  nonworkdays <- unique(nonworkdays)

  if (length(nonworkdays) == 7L)
    stop("'nonworkdays' cannot cover every day of the week")

  from <- as.Date(from)
  to   <- as.Date(to)

  if (!is.null(holiday))
    holiday <- unique(as.Date(holiday))

  if (length(from) == 0L || length(to) == 0L)
    return(integer(0))

  ll <- recycle(from = from, to = to)

  # seq_len(): 1:0 ran the loop once with i = 1 on an empty result
  res <- integer(attr(ll, "maxdim"))
  for (i in seq_len(attr(ll, "maxdim")))
    res[i] <- .workDays(ll$from[i], ll$to[i],
                        holiday = holiday, nonworkdays = nonworkdays)

  return(res)
}



# == internal helper functions =================================================

.workDays <- function(from, to,
                      holiday = NULL, nonworkdays = c("Sat", "Sun")) {

  if (is.na(from) || is.na(to))
    return(NA_integer_)

  # seq() with by = 1 rejects a start date after the end date, so a
  # reversed pair used to abort the whole call with "wrong sign in 'by'"
  if (to < from)
    return(0L)

  d <- as.integer(to - from)
  w <- d %/% 7

  res <- w * (7 - length(nonworkdays)) +
    sum(weekday(seq(from + w * 7, to, 1), fmt = "dd", lang = "en") %notin% nonworkdays)

  if (!is.null(holiday) && length(holiday)) {
    # count holidays in period
    h <- holiday[holiday %[]% c(from, to)]
    if (length(h))
      res <- res - sum(weekday(h, fmt = "dd", lang = "en") %notin% nonworkdays)
  }

  return(as.integer(res))
}
