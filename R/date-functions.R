
#' Basic Date Functions
#'
#' Convenience wrappers for extracting date/time components and performing
#' common date calculations - a readable alternative to [format()]
#' and its cryptic format codes.
#'
#' @section Date component extractors:
#' \tabular{lll}{
#'   **Function**    \tab **Returns**                                          \tab **Range / Notes** \cr
#'   `year`        \tab Year of a date or `ym` object                     \tab `yyyy` \cr
#'   `quarter`     \tab Quarter of the year                                    \tab 1-4 \cr
#'   `month`       \tab Month of the year (numeric, abbreviated, or full name) \tab 1-12; S3 dispatch for `ym` \cr
#'   `week`        \tab Week of the year                                       \tab ISO 8601 or US convention \cr
#'   `day`         \tab Day of the month (readable/writable)                   \tab 1-31 \cr
#'   `weekday`     \tab Day of the week (numeric, abbreviated, or full name)   \tab 1 = Mon ... 7 = Sun \cr
#'   `yearDay`     \tab Day of the year                                        \tab 1-366 \cr
#'   `yearWeek`    \tab Compact year-week integer                              \tab `yyyyww` (ISO or US) \cr
#'   `yearMonth`   \tab Compact year-month integer                             \tab `yyyymm` \cr
#' }
#'
#' @section Time component extractors (POSIXct/POSIXlt):
#' \tabular{ll}{
#'   **Function**  \tab **Returns** \cr
#'   `hour`      \tab Hour (0-23) \cr
#'   `minute`    \tab Minute (0-59) \cr
#'   `second`    \tab Second (0-60) \cr
#'   `timezone`  \tab Time zone string \cr
#'   `now`       \tab Current date and time (`Sys.time()`) \cr
#'   `today`     \tab Current date (`Sys.Date()`) \cr
#' }
#'
#' @section Logical tests:
#' \tabular{ll}{
#'   **Function**    \tab **Returns** \cr
#'   `isWeekend`   \tab `TRUE` if `x` falls on Saturday or Sunday \cr
#'   `isLeapYear`  \tab `TRUE` if the year of `x` is a leap year \cr
#' }
#'
#' @section Date arithmetic:
#' \tabular{ll}{
#'   **Function**        \tab **Description** \cr
#'   `diffDays360`     \tab Days between two dates using the 360-day calendar convention \cr
#'   `lastDayOfMonth`  \tab Last calendar day of the month of `x` \cr
#'   `yearDays`        \tab Total number of days in the year of `x` (365 or 366) \cr
#'   `monthDays`       \tab Number of days in the month of `x` (28-31) \cr
#' }
#'
#' @section Language for month and weekday names:
#' `month()` and `weekday()` respect the `"lang"` option.  Set
#' `options(lang = "en")` to always get English names, or
#' `options(lang = "local")` for the current system locale.  When the
#' option is absent, `"local"` is used as default.
#'
#' @name date_functions
#'
#' @aliases year quarter month week day day<- weekday yearDay yearWeek yearMonth isWeekend isLeapYear hour minute second now today diffDays360 lastDayOfMonth timezone yearDays monthDays month.ym year.ym
#' 
#' @param x a `Date`, `POSIXct`, `POSIXlt`, or `ym` object
#' to evaluate
#' @param fmt format code controlling the output of `month()` and
#'   `weekday()`.\cr
#'   For `month()`: `"m"` = integer (default), `"mm"` =
#'   abbreviated name, `"mmm"` = full name.\cr
#'   For `weekday()`: `"d"` = integer (default), `"dd"` =
#'   abbreviated name, `"ddd"` = full name.
#' @param lang language for names returned by `month()` and
#'   `weekday()`.  Either `"local"` (current system locale, the
#'   default) or `"en"` (English).  Falls back to the `"lang"`
#'   option if set; otherwise `"local"` is used.
#' @param ... further arguments passed to methods. `year()` is generic
#'   and carries them for the sake of S3 consistency; none of the methods
#'   currently uses them.
#' @param stringsAsFactors logical; if `TRUE` (default), character results
#'   from `month()` and `weekday()` are returned as ordered factors
#'   whose levels follow calendar order
#' @param value replacement value for the `day<-` assignment function
#' @param startDate,endDate start and end dates for `diffDays360()`
#' @param method calculation convention. For `diffDays360()` either
#'   `"eu"` (European, default) or `"us"` (US); for `week()`
#'   and `yearWeek()` either `"iso"` (ISO 8601, default) or
#'   `"us"`. The two sets are not interchangeable.
#'
#' @return a vector whose type depends on the function: numeric for
#' integer-valued components, an ordered factor or character vector when
#' `fmt` requests names, logical for `isWeekend()` and
#' `isLeapYear()`, `Date` for `today()` and
#' `lastDayOfMonth()`, and `POSIXct` for `now()`
#'
#' @seealso [strptime()], [DateTimeClasses()],
#'   [as.POSIXlt()], [countWorkDays()]
#'
#'
#' @examples
#' x <- today()   # equivalent to Sys.Date()
#'
#' year(x)
#' quarter(x)
#'
#' # month: numeric, abbreviated, full name
#' month(x)
#' month(x, fmt = "mm",  lang = "en")
#' month(x, fmt = "mm",  lang = "local")
#' month(x, fmt = "mmm", lang = "en")
#' month(x, fmt = "mmm", lang = "local")
#'
#' week(x)
#' week(x, method = "us")
#'
#' # day is both readable and writable
#' day(x)
#' day(x) <- 20
#' x
#'
#' # weekday: numeric, abbreviated, full name
#' weekday(x)
#' weekday(x, fmt = "dd",  lang = "en")
#' weekday(x, fmt = "ddd", lang = "local")
#'
#' yearDay(x)
#' yearWeek(x)
#' yearMonth(x)
#'
#' isWeekend(x)
#' isLeapYear(x)
#' isLeapYear(2000L)
#'
#' # month names for a weekly time sequence
#' month(seq(Sys.Date(), Sys.Date() + 150, by = "weeks"), fmt = "mm")
#'
#' # last day of month for several dates
#' lastDayOfMonth(as.Date(c("2014-10-12", "2013-01-31", "2011-12-05")))
#'
#' # days in month / year
#' monthDays(x)
#' yearDays(x)
#'
#' # 360-day calendar difference. The two conventions agree here ...
#' diffDays360(as.Date("2023-01-31"), as.Date("2023-03-31"))
#' diffDays360(as.Date("2023-01-31"), as.Date("2023-03-31"), method = "us")
#'
#' # ... but not here, which is the point of having both
#' diffDays360(as.Date("2023-01-31"), as.Date("2023-02-28"))
#' diffDays360(as.Date("2023-01-31"), as.Date("2023-02-28"), method = "us")
#' 
#' 



#' @rdname date_functions
#' @family date.time
#' @concept date-time
#' @export
year <- function(x, ...){
  UseMethod("year")
}

#' @rdname date_functions
#' @method year ym
#' @export
year.ym <- function(x, ...){ unclass(x) %/% 100L }

#' @rdname date_functions
#' @method year default
#' @export
year.default <- function(x, ...){ as.POSIXlt(x)$year + 1900L }





#' @rdname date_functions
#' @export
month <- function(x, fmt = c("m", "mm", "mmm"), 
                  lang = .getOption("lang"), stringsAsFactors = TRUE) {
  UseMethod("month")
}


#' @rdname date_functions
#' @method month ym
#' @export
month.ym <- function(x, fmt = c("m", "mm", "mmm"), 
                     lang = .getOption("lang"), stringsAsFactors = TRUE) {
  # unclass(x - year(x) * 100)   
  x <- .asDateInTz(x)
  NextMethod()
}

#' @rdname date_functions
#' @method month default
#' @export
month.default <- function(x, fmt = c("m", "mm", "mmm"), 
                          lang = .getOption("lang"), stringsAsFactors = TRUE) {
  
  res <- as.POSIXlt(x)$mon + 1L
  
  switch(match.arg(arg = fmt, choices = c("m", "mm", "mmm")),
         m = { res },
         mm = {
           # res <- as.integer(format(x, "%m"))
           switch(match.arg(arg = lang, choices = c("local", "en")),
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- ordered(res, levels=1L:12L, labels=format(ISOdate(2000L, 1L:12L, 1L), "%b"))
                  },
                  en = {
                    res <- ordered(res, levels=1L:12L, labels=month.abb)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         },
         mmm = {
           # res <- as.integer(format(x, "%m"))
           switch(match.arg(arg = lang, choices = c("local", "en")),
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- ordered(res, levels=1L:12L, labels=format(ISOdate(2000L, 1L:12L, 1L), "%B"))
                  },
                  en = {
                    res <- ordered(res, levels=1L:12L, labels=month.name)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         })
  return(res)
}




#' @rdname date_functions
#' @export
week <- function(x, method = c("iso", "us")){
  
  # dd <- seq(as.Date("1970-01-01"), as.Date("2030-01-01"), by="days")
  # 
  # identical(week(dd, "us"), as.integer(lubridate::week(dd)))
  # microbenchmark::microbenchmark(
  #   DescToolsX = week(dd, "us"),
  #   lubridate = lubridate::week(dd)
  # )
  # 
  # identical(week(dd, "iso"), as.integer(lubridate::isoweek(dd)))
  # microbenchmark::microbenchmark(
  #   DescToolsX = week(dd, "iso"),
  #   lubridate = lubridate::isoweek(dd)
  # )
  # --> We are superfast!!
  
  # cast x to date, such as being able to handle POSIX-Dates automatically
  x <- .asDateInTz(x)
  
  method <- match.arg(method, c("iso", "us"))
  switch(method,
         "iso" = {
           wn <- .Call("_DescToolsX_isoWeek_cpp", x, PACKAGE="DescToolsX")
         },
         "us"={
           wn <- .Call("_DescToolsX_usWeek_cpp", x, PACKAGE="DescToolsX")
         }
  )
  return(wn)
  
}


# day <- function(x){ as.integer(format(as.Date(x), "%d") ) }
#' @rdname date_functions
#' @export
day <- function(x){ as.POSIXlt(x)$mday }


# Accessor for day, as defined by library(lubridate)
#' @rdname date_functions
#' @export
"day<-" <- function(x, value) {

  # "+" adds DAYS to a Date but SECONDS to a POSIXct, so the plain
  # x + (value - day(x)) shifted a date-time by a few seconds instead of
  # to the requested day of the month, silently and without error.
  if (inherits(x, "POSIXt"))
    x + (value - day(x)) * 86400
  else
    x + (value - day(x))
}


#' @rdname date_functions
#' @export
weekday <- function (x, fmt = c("d", "dd", "ddd"), 
                     lang = .getOption("lang"), stringsAsFactors = TRUE) {
  
  # deliberately no as.Date() here: as.POSIXlt() already breaks a Date
  # down in UTC and a POSIXct in its own zone, which is exactly right.
  # Coercing first would push a timestamp onto the previous day.
  res <- as.POSIXlt(x)$wday
  res <- replace(res, res==0, 7)
  
  switch(match.arg(arg = fmt, choices = c("d", "dd", "ddd")),
         d = { res },
         dd = {
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
           switch(match.arg(arg = lang, choices = c("local", "en")),
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- ordered(res, levels=1:7, labels=format(ISOdate(2000, 1, 3:9), "%a"))
                  },
                  en = {
                    res <- ordered(res, levels=1:7, labels=day.abb)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         },
         ddd = {
           # weekdays in current locale, Sunday : Saturday, format(ISOdate(2000, 1, 2:8), "%A")
           switch(match.arg(arg = lang, choices = c("local", "en")),
                  local = {
                    # months in current locale:  format(ISOdate(2000, 1:12, 1), "%b")
                    res <- ordered(res, levels=1:7, labels=format(ISOdate(2000, 1, 3:9), "%A"))
                  },
                  en = {
                    res <- ordered(res, levels=1:7, labels=day.name)
                  })
           if(!stringsAsFactors) res <- as.character(res)
         })
  return(res)
}





#' @rdname date_functions
#' @export
quarter <- function (x) {
  # Berechnet das Quartal eines Datums
  # y <- as.numeric( format( x, "%Y") )
  # paste(y, "Q", (as.POSIXlt(x)$mon)%/%3 + 1, sep = "")
  # old definition is counterintuitive...
  return((as.POSIXlt(x)$mon) %/% 3L + 1L)
}



#' @rdname date_functions
#' @export
today <- function() Sys.Date()

#' @rdname date_functions
#' @export
now <- function() Sys.time()

#' @rdname date_functions
#' @export
hour <- function(x) {
  # strptime(x, "%H")
  as.POSIXlt(x)$hour
}

#' @rdname date_functions
#' @export
minute <- function(x) {
  #  strptime(x, "%M")
  as.POSIXlt(x)$min
}

#' @rdname date_functions
#' @export
second <- function(x) {
  #  strptime(x, "%S")
  as.POSIXlt(x)$sec
}


#' @rdname date_functions
#' @export
timezone <- function(x) {
  as.POSIXlt(x)$zone
}


#' @rdname date_functions
#' @export
yearMonth <- function(x){
  # returns the yearmonth representation of a date x

  # reduce x to a Date first, as week() and yearWeek() do: the compiled
  # routine reads the underlying double as DAYS since the epoch, so a
  # POSIXct (seconds since the epoch) used to be interpreted as a date
  # roughly 86400 times too far in the future - no error, just a wrong
  # year and month.
  x <- .asDateInTz(x)

  return(.Call("_DescToolsX_usYearmonth_cpp", x, PACKAGE="DescToolsX"))

}


#' @rdname date_functions
#' @export
yearWeek <- function(x, method = c("iso", "us")){
  
  # cast x to date, such as being able to handle POSIX-Dates automatically
  x <- .asDateInTz(x)
  
  method <- match.arg(method, c("iso", "us"))
  switch(method,
         "iso" = {
           res <- .Call("_DescToolsX_isoYearweek_cpp", x, PACKAGE="DescToolsX") 
           
         },
         "us"={
           res <- .Call("_DescToolsX_usYearweek_cpp", x, PACKAGE="DescToolsX") 
         }
  )
  
  return(res)
  
}



#' @rdname date_functions
#' @export
yearDay <- function(x) {
  # return(as.integer(format(as.Date(x), "%j")))
  
  # As ?POSIXlt reveals, a $yday suffix to a POSIXlt date (or even a vector of such) 
  # will convert to day of year. 
  # Beware that POSIX counts Jan 1 as day 0, so you might want to add 1 to the result.
  return(as.POSIXlt(x)$yday + 1L)
}




#' @rdname date_functions
#' @export
diffDays360 <- function(startDate, endDate, method=c("eu","us")){
  
  # source: http://en.wikipedia.org/wiki/360-day_calendar
  startDate <- as.Date(startDate)
  endDate <- as.Date(endDate)
  
  d1 <- day(startDate)
  m1 <- month(startDate)
  y1 <- year(startDate)
  d2 <- day(endDate)
  m2 <- month(endDate)
  y2 <- year(endDate)
  
  method = match.arg(method)
  switch(method,
         "eu" = {
           # The former version adjusted startDate/endDate here, but d1
           # and d2 had already been read off above and were never
           # recomputed - so the whole European rule was dead code and
           # "eu" silently returned the unadjusted difference. Only the
           # "us" branch below worked, because it assigns to d1/d2
           # directly. Adjust the day numbers, as the convention says.
           if(any(d1 == 31L)) d1[d1 == 31L] <- 30L
           if(any(d2 == 31L)) d2[d2 == 31L] <- 30L
         }
         , "us" ={
           endFeb1 <- day(startDate + 1L) == 1L & month(startDate + 1L) == 3L
           endFeb2 <- day(endDate + 1L)   == 1L & month(endDate + 1L)   == 3L

           # vectorised: the former if()s made the function scalar-only
           # and error out on vector input under R >= 4.2
           d2[endFeb1 & endFeb2] <- 30L

           adj1 <- d1 == 31L | endFeb1
           d2[adj1 & d2 == 31L] <- 30L
           d1[adj1] <- 30L
         }
  )

  return( (y2-y1)*360L + (m2-m1)*30L + d2-d1)
  
}


#' @rdname date_functions
#' @export
lastDayOfMonth <- function(x){
  z <- addMonths(x, 1L)
  day(z) <- 1L
  return(z - 1L)
}



#' @rdname date_functions
#' @export
yearDays <- function (x) {
  # return the number of days in the specific year of x
  x <- as.POSIXlt(x)
  # [] on every component: a plain x$min <- 0 replaces the whole vector
  # with a single element and leaves the POSIXlt fields of unequal
  # length. isdst is reset so that a DST transition cannot shift the
  # constructed midnight into the previous day.
  x$mon[] <- x$mday[] <- x$sec[] <- x$min[] <- x$hour[] <- 0
  x$isdst[] <- -1L
  x$year <- x$year + 1
  return(as.POSIXlt(as.POSIXct(x))$yday + 1)
}


#' @rdname date_functions
#' @export
monthDays <- function (x) {
  # return the number of days in the specific month of x
  x <- as.POSIXlt(x)
  x$mday[] <- x$sec[] <- x$min[] <- x$hour[] <- 0
  x$isdst[] <- -1L
  x$mon <- x$mon + 1
  return(as.POSIXlt(as.POSIXct(x))$mday)
}




#' @rdname date_functions
#' @export
isWeekend <- function(x) {
  x <- as.POSIXlt(x)
  x$wday > 5L | x$wday < 1L
}


#' @rdname date_functions
#' @export
isLeapYear <- function(x){

  # Dispatch on the class, not on isWholeLike(): a bare year vector
  # containing NA is not "whole like", so isLeapYear(c(2020L, NA)) fell
  # into the Date branch, where as.Date() read 2020 as days since the
  # epoch and answered for 1975.
  if(!inherits(x, c("Date", "POSIXt")) && is.numeric(x)){

    if(any(x %% 1 != 0, na.rm = TRUE))
      stop("a numeric 'x' must contain whole years")

    return(.Call("_DescToolsX_isLeapYearInt_cpp", as.integer(x),
                 PACKAGE="DescToolsX"))
  }

  # .asDateInTz() for the same reason as in yearMonth(): the compiled
  # routine reads days since the epoch, and the calendar day has to be
  # the one the timestamp's own zone shows.
  .Call("_DescToolsX_isLeapYearDate_cpp", .asDateInTz(x), PACKAGE="DescToolsX")

}


# == internal helper functions =================================================

# Which calendar day is this value on?
#
# The compiled routines below read days since the epoch, so a POSIXct has
# to be reduced to a Date first. as.Date() alone is not enough: since
# R 4.3 as.Date.POSIXct() defaults to tz = "UTC", so a timestamp shortly
# after midnight in a positive-offset zone falls back onto the previous
# day -
#
#   x <- as.POSIXct("2019-01-01 00:30:00", tz = "Europe/Zurich")
#   as.Date(x)             # "2018-12-31"
#   format(x, "%Y-%m-%d")  # "2019-01-01"
#
# - and week(), yearWeek(), yearMonth() and isLeapYear() then answered for
# the wrong day. The calendar day of a timestamp follows its own zone,
# which is also what format() reports. Mirror of .toWallClock() in
# pharos's fm.R, one step simpler: only the date is needed here, not a
# wall-clock instant.
#' @noRd
.asDateInTz <- function(x) {

  if(inherits(x, "Date"))
    return(x)

  if(!inherits(x, "POSIXt"))
    return(as.Date(x))

  if(inherits(x, "POSIXlt"))
    x <- as.POSIXct(x)

  tz <- attr(x, "tzone")
  if(is.null(tz) || !nzchar(tz[1L]))
    tz <- Sys.timezone()

  as.Date(x, tz = tz)
}
