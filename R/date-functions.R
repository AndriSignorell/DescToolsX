
#' Basic Date Functions
#'
#' Convenience wrappers for extracting date/time components and performing
#' common date calculations — a readable alternative to \code{\link{format}()}
#' and its cryptic format codes.
#'
#' @section Date component extractors:
#' \tabular{lll}{
#'   \bold{Function}    \tab \bold{Returns}                                          \tab \bold{Range / Notes} \cr
#'   \code{year}        \tab Year of a date or \code{ym} object                     \tab \code{yyyy} \cr
#'   \code{quarter}     \tab Quarter of the year                                    \tab 1–4 \cr
#'   \code{month}       \tab Month of the year (numeric, abbreviated, or full name) \tab 1–12; S3 dispatch for \code{ym} \cr
#'   \code{week}        \tab Week of the year                                       \tab ISO 8601 or US convention \cr
#'   \code{day}         \tab Day of the month (readable/writable)                   \tab 1–31 \cr
#'   \code{weekday}     \tab Day of the week (numeric, abbreviated, or full name)   \tab 1 = Mon … 7 = Sun \cr
#'   \code{yearDay}     \tab Day of the year                                        \tab 1–366 \cr
#'   \code{yearWeek}    \tab Compact year-week integer                              \tab \code{yyyyww} (ISO or US) \cr
#'   \code{yearMonth}   \tab Compact year-month integer                             \tab \code{yyyymm} \cr
#' }
#'
#' @section Time component extractors (POSIXct/POSIXlt):
#' \tabular{ll}{
#'   \bold{Function}  \tab \bold{Returns} \cr
#'   \code{hour}      \tab Hour (0–23) \cr
#'   \code{minute}    \tab Minute (0–59) \cr
#'   \code{second}    \tab Second (0–60) \cr
#'   \code{timezone}  \tab Time zone string \cr
#'   \code{now}       \tab Current date and time (\code{Sys.time()}) \cr
#'   \code{today}     \tab Current date (\code{Sys.Date()}) \cr
#' }
#'
#' @section Logical tests:
#' \tabular{ll}{
#'   \bold{Function}    \tab \bold{Returns} \cr
#'   \code{isWeekend}   \tab \code{TRUE} if \code{x} falls on Saturday or Sunday \cr
#'   \code{isLeapYear}  \tab \code{TRUE} if the year of \code{x} is a leap year \cr
#' }
#'
#' @section Date arithmetic:
#' \tabular{ll}{
#'   \bold{Function}        \tab \bold{Description} \cr
#'   \code{diffDays360}     \tab Days between two dates using the 360-day calendar convention \cr
#'   \code{lastDayOfMonth}  \tab Last calendar day of the month of \code{x} \cr
#'   \code{yearDays}        \tab Total number of days in the year of \code{x} (365 or 366) \cr
#'   \code{monthDays}       \tab Number of days in the month of \code{x} (28–31) \cr
#' }
#'
#' @section Language for month and weekday names:
#' \code{month()} and \code{weekday()} respect the \code{"lang"} option.  Set
#' \code{options(lang = "en")} to always get English names, or
#' \code{options(lang = "local")} for the current system locale.  When the
#' option is absent, \code{"local"} is used as default.
#'
#' @name date_functions
#'
#' @aliases year quarter month week day day<- weekday yearDay yearWeek yearMonth isWeekend isLeapYear hour minute second now today diffDays360 lastDayOfMonth timezone yearDays monthDays month.ym year.ym
#' 
#' @param x A \code{Date}, \code{POSIXct}, \code{POSIXlt}, or \code{ym} object
#'   to be evaluated.
#' @param fmt Format code controlling the output of \code{month()} and
#'   \code{weekday()}.\cr
#'   For \code{month()}: \code{"m"} = integer (default), \code{"mm"} =
#'   abbreviated name, \code{"mmm"} = full name.\cr
#'   For \code{weekday()}: \code{"d"} = integer (default), \code{"dd"} =
#'   abbreviated name, \code{"ddd"} = full name.
#' @param lang Language for names returned by \code{month()} and
#'   \code{weekday()}.  Either \code{"local"} (current system locale, the
#'   default) or \code{"en"} (English).  Falls back to the \code{"lang"}
#'   option if set; otherwise \code{"local"} is used.
#' @param stringsAsFactors Logical; if \code{TRUE} (default), character results
#'   from \code{month()} and \code{weekday()} are returned as ordered factors
#'   whose levels follow calendar order.
#' @param value Replacement value for the \code{day<-} assignment function.
#' @param startDate,endDate Start and end dates for \code{diffDays360()}.
#' @param method Calculation convention: \code{"eu"} (European, default) or
#'   \code{"us"} (US) for \code{diffDays360()} and \code{week()}/
#'   \code{yearWeek()}.
#'
#' @return
#' All extractor functions return a vector of the same length as \code{x}:
#' numeric for integer-valued components, ordered factor or character when
#' \code{fmt} requests names.  \code{isWeekend()} and \code{isLeapYear()}
#' return a logical vector.
#'
#' @author Andri Signorell \email{andri@@signorell.net}
#'
#' @seealso \code{\link{strptime}}, \code{\link{DateTimeClasses}},
#'   \code{\link{as.POSIXlt}}, \code{\link{countWorkDays}}
#'
#' @keywords chron
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
#' # 360-day calendar difference
#' diffDays360(as.Date("2023-01-31"), as.Date("2023-03-31"))
#' diffDays360(as.Date("2023-01-31"), as.Date("2023-03-31"), method = "us")
#' 
#' 



#' @rdname date_functions
#' @family date.utils
#' @concept date-handling
#' @concept data-manipulation
#'
#'
#' @export
year <-  function(x){
  UseMethod("year")
}

#' @rdname date_functions
#' @method year ym
#' @export
year.ym  <- function(x){  unclass(round((x/100)))   }

#' @rdname date_functions
#' @method year default
#' @export
year.default <- function(x){ as.POSIXlt(x)$year + 1900L }





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
  x <- as.Date(x)
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
  x <- as.Date(x)
  
  method <- match.arg(method, c("iso", "us"))
  switch(method,
         "iso" = {
           wn <- .Call("_DescToolsX_isoWeek", x, PACKAGE="DescToolsX")
         },
         "us"={
           wn <- .Call("_DescToolsX_usWeek", x, PACKAGE="DescToolsX")
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
"day<-" <- function(x, value) { x <- x + (value - day(x)) }


#' @rdname date_functions
#' @export
weekday <- function (x, fmt = c("d", "dd", "ddd"), 
                     lang = .getOption("lang"), stringsAsFactors = TRUE) {
  
  # x <- as.Date(x)
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
  # x <- as.POSIXlt(x)
  # return(as.ym((x$year + 1900L)*100L + x$mon + 1L))
  
  return(.Call("_DescToolsX_usYearmonth", x, PACKAGE="DescToolsX")) 
  
}


#' @rdname date_functions
#' @export
yearWeek <- function(x, method = c("iso", "us")){
  
  # cast x to date, such as being able to handle POSIX-Dates automatically
  x <- as.Date(x)
  
  method <- match.arg(method, c("iso", "us"))
  switch(method,
         "iso" = {
           res <- .Call("_DescToolsX_isoYearweek", x, PACKAGE="DescToolsX") 
           
         },
         "us"={
           res <- .Call("_DescToolsX_usYearweek", x, PACKAGE="DescToolsX") 
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
           if(day(startDate)==31L) startDate <- startDate-1L
           if(day(endDate)==31L) endDate <- endDate-1L
         }
         , "us" ={
           if( (day(startDate+1L)==1L & month(startDate+1L)==3L) &
               (day(endDate+1L)==1L & month(endDate+1L)==3L)) d2 <- 30L
           if( d1==31L ||
               (day(startDate+1L)==1L & month(startDate+1L)==3L)) {
             d1 <- 30L
             if(d2==31L) d2 <- 30L
           }
           
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
  x$mon[] <- x$mday[] <- x$sec[] <- x$min <- x$hour <- 0
  x$year <- x$year + 1
  return(as.POSIXlt(as.POSIXct(x))$yday + 1)
}


#' @rdname date_functions
#' @export
monthDays <- function (x) {
  # return the number of days in the specific month of x
  x <- as.POSIXlt(x)
  x$mday[] <- x$sec[] <- x$min <- x$hour <- 0
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
  
  if(!isWholeLike(x))
    .Call("_DescToolsX_isLeapYearDate", x, PACKAGE="DescToolsX")
  else 
    .Call("_DescToolsX_isLeapYearInt", x, PACKAGE="DescToolsX")
  
}



