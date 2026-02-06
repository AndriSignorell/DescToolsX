
#' Basic Date Functions
#' 
#' Some more date functions for making daily life a bit easier. The first ones
#' extract a specific part of a given date, others check some conditions. 
#' 
#' These functions are mainly convenience wrappers for the painful
#' \code{format()} and its strange codes...\cr Based on the requested time
#' component, the output is as follows:\cr \tabular{ll}{ \code{Year}\tab
#' returns the year of a date or a yearmonth (\code{yyyymm}) in (\code{yyyy})
#' format .\cr \code{Quarter}\tab returns the quarter of the year (1 to 4) for
#' the input date. \cr \code{Month}\tab returns the month of the year (1 to 12)
#' for the input date or for a yearmonth. \cr \code{Week}\tab returns the week
#' of the year for the input date (0 to 53), as defined in ISO8601. \cr
#' \code{Weekday}\tab returns the week day of the input date. (1 - Monday, 2 -
#' Tuesday, ... 7 - Sunday). (Names and abbreviations are either english or in
#' the current locale!)\cr \code{YearDay}\tab returns the day of the year
#' numbering (1 to 366). \cr \code{Day}\tab returns the day of the month (1 to
#' 31). \cr \code{YearDay}\tab returns the yearday representation (yyyyddd) of
#' a date as long integer. \cr \code{YearWeek}\tab returns the yearweek
#' representation (yyyyww) of a date as long integer. \cr \code{YearMonth}\tab
#' returns the yearmonth representation (yyyymm) of a date as long integer. \cr
#' \code{Hour}, \code{Minute}, \code{Second} \tab returns the hour, minute or
#' second from a POSIXlt object. \cr \code{Timezone} \tab returns the timezone
#' from a POSIXlt object. \cr \code{HmsToMinute} \tab converts the time parts
#' of a POSIXlt object to minutes.\cr \code{Today}, \code{Now}\tab returns the
#' current date, resp. the current date and time.\cr
#' 
#' \code{isWeekend} \tab returns \code{TRUE}, if the date x falls on a weekend.
#' \cr \code{isLeapYear} \tab returns \code{TRUE}, if the year of the date x is
#' a leap year. \cr } The day can not only be extracted, but as well be
#' defined. See examples. \tabular{ll}{ \code{DiffDays360} \tab calculates the
#' difference between 2 dates using the 360-days convention.\cr
#' \code{LastDayOfMonth} \tab returns the last day of the month of the given
#' date(s). \cr \code{YearDays} \tab returns the total number of days of the
#' given date(s). \cr \code{MonthDays} \tab returns the numer of days of the
#' month of the given date(s). \cr } The language in \code{Weekday()} and
#' \code{Month()} can be set with an option as well.  These functions will
#' check for an existing option named \code{"lang"} and take this value if it
#' exists. So simply set \code{option(lang="engl")} if the results should
#' always be reported in English.
#' 
#' @name date_functions
#' @aliases Year Quarter Month Week Day Day<- Weekday YearDay YearWeek
#' YearMonth isWeekend isLeapYear Hour Minute Second Now Today DiffDays360
#' LastDayOfMonth Timezone YearDays MonthDays Month.ym Year.ym
#' @param x the date to be evaluated. 
#' @param fmt format string, defines how the month or the weekday are to be
#' formatted. Defaults to \code{"m"}, resp. \code{"d"}. Is ignored for other
#' functions. 
#' @param value new value 
#' @param lang optional value setting the language for the months and daynames.
#' Can be either \code{"local"} for current locale or \code{"engl"} for
#' english. If left to \code{NULL}, the option \code{"lang"} will be searched
#' for and if not found \code{"local"} will be taken as default. 
#' @param stringsAsFactors logical. Defines if the result should be coerced to
#' a factor, using the local definitions as levels.  The result would be an
#' ordered factor. Default is TRUE.
#' @param start_d,end_d the start, resp. end date for \code{DiffDays360}. 
#' @param method one out of \code{"eu", "us"}, setting either European or
#' US-Method calculation mode. Default is \code{"eu"}. 
#' @return a vector of the same dimension as x, consisting of either numeric
#' values or characters depending on the function used.
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{strptime}}, \code{\link{DateTimeClasses}},
#' \code{\link{as.POSIXlt}} 
#' @keywords chron
#' @examples
#' 
#' x <- Today()    # the same as Sys.Date() but maybe easier to remember..
#' 
#' Year(x)
#' Quarter(x)
#' 
#' Month(x)
#' Month(x, fmt = "mm", lang="en")
#' Month(x, fmt = "mm", lang="local")
#' Month(x, fmt = "mmm", lang="en")
#' Month(x, fmt = "mmm", lang="local")
#' 
#' Week(x)
#' 
#' Day(x)
#' Day(x) <- 20
#' x
#' 
#' Weekday(x)
#' Weekday(x, fmt = "dd", lang="en")
#' Weekday(x, fmt = "dd", lang="local")
#' Weekday(x, fmt = "ddd", lang="en")
#' Weekday(x, fmt = "ddd", lang="local")
#' 
#' YearDay(x)
#' 
#' isWeekend(x)
#' 
#' isLeapYear(x)
#' 
#' # let's generate a time sequence by weeks
#' Month(seq(from=as.Date(Sys.Date()), to=Sys.Date()+150, by="weeks"), fmt="mm")
#' 
#' LastDayOfMonth(as.Date(c("2014-10-12","2013-01-31","2011-12-05")))
#' 
#' 



#' @rdname date_functions
#' @export
Year <-  function(x){
  UseMethod("Year")
}

#' @rdname date_functions
#' @method Year ym
#' @export
Year.ym  <- function(x){  unclass(round((x/100)))   }

#' @rdname date_functions
#' @method Year default
#' @export
Year.default <- function(x){ as.POSIXlt(x)$year + 1900L }





#' @rdname date_functions
#' @export
Month <- function(x, fmt = c("m", "mm", "mmm"), 
                  lang = .getOption("lang"), stringsAsFactors = TRUE) {
  UseMethod("Month")
}


#' @rdname date_functions
#' @method Month ym
#' @export
Month.ym <- function(x, fmt = c("m", "mm", "mmm"), 
                     lang = .getOption("lang"), stringsAsFactors = TRUE) {
  # unclass(x - Year(x) * 100)   
  x <- as.Date(x)
  NextMethod()
}

#' @rdname date_functions
#' @method Month default
#' @export
Month.default <- function(x, fmt = c("m", "mm", "mmm"), 
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
Week <- function(x, method = c("iso", "us")){
  
  # dd <- seq(as.Date("1970-01-01"), as.Date("2030-01-01"), by="days")
  # 
  # identical(Week(dd, "us"), as.integer(lubridate::week(dd)))
  # microbenchmark::microbenchmark(
  #   DescTools = Week(dd, "us"),
  #   lubridate = lubridate::week(dd)
  # )
  # 
  # identical(Week(dd, "iso"), as.integer(lubridate::isoweek(dd)))
  # microbenchmark::microbenchmark(
  #   DescTools = Week(dd, "iso"),
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


# Day <- function(x){ as.integer(format(as.Date(x), "%d") ) }
#' @rdname date_functions
#' @export
Day <- function(x){ as.POSIXlt(x)$mday }


# Accessor for Day, as defined by library(lubridate)
#' @rdname date_functions
#' @export
"Day<-" <- function(x, value) { x <- x + (value - Day(x)) }


#' @rdname date_functions
#' @export
Weekday <- function (x, fmt = c("d", "dd", "ddd"), 
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
Quarter <- function (x) {
  # Berechnet das Quartal eines Datums
  # y <- as.numeric( format( x, "%Y") )
  # paste(y, "Q", (as.POSIXlt(x)$mon)%/%3 + 1, sep = "")
  # old definition is counterintuitive...
  return((as.POSIXlt(x)$mon) %/% 3L + 1L)
}



#' @rdname date_functions
#' @export
Today <- function() Sys.Date()

#' @rdname date_functions
#' @export
Now <- function() Sys.time()

#' @rdname date_functions
#' @export
Hour <- function(x) {
  # strptime(x, "%H")
  as.POSIXlt(x)$hour
}

#' @rdname date_functions
#' @export
Minute <- function(x) {
  #  strptime(x, "%M")
  as.POSIXlt(x)$min
}

#' @rdname date_functions
#' @export
Second <- function(x) {
  #  strptime(x, "%S")
  as.POSIXlt(x)$sec
}


#' @rdname date_functions
#' @export
Timezone <- function(x) {
  as.POSIXlt(x)$zone
}


#' @rdname date_functions
#' @export
YearMonth <- function(x){
  # returns the yearmonth representation of a date x
  # x <- as.POSIXlt(x)
  # return(as.ym((x$year + 1900L)*100L + x$mon + 1L))
  
  return(.Call("_DescToolsX_usYearmonth", x, PACKAGE="DescToolsX")) 
  
}


#' @rdname date_functions
#' @export
YearWeek <- function(x, method = c("iso", "us")){
  
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
YearDay <- function(x) {
  # return(as.integer(format(as.Date(x), "%j")))
  
  # As ?POSIXlt reveals, a $yday suffix to a POSIXlt date (or even a vector of such) 
  # will convert to day of year. 
  # Beware that POSIX counts Jan 1 as day 0, so you might want to add 1 to the result.
  return(as.POSIXlt(x)$yday + 1L)
}




#' @rdname date_functions
#' @export
DiffDays360 <- function(start_d, end_d, method=c("eu","us")){
  
  # source: http://en.wikipedia.org/wiki/360-day_calendar
  start_d <- as.Date(start_d)
  end_d <- as.Date(end_d)
  
  d1 <- Day(start_d)
  m1 <- Month(start_d)
  y1 <- Year(start_d)
  d2 <- Day(end_d)
  m2 <- Month(end_d)
  y2 <- Year(end_d)
  
  method = match.arg(method)
  switch(method,
         "eu" = {
           if(Day(start_d)==31L) start_d <- start_d-1L
           if(Day(end_d)==31L) end_d <- end_d-1L
         }
         , "us" ={
           if( (Day(start_d+1L)==1L & Month(start_d+1L)==3L) &
               (Day(end_d+1L)==1L & Month(end_d+1L)==3L)) d2 <- 30L
           if( d1==31L ||
               (Day(start_d+1L)==1L & Month(start_d+1L)==3L)) {
             d1 <- 30L
             if(d2==31L) d2 <- 30L
           }
           
         }
  )
  
  return( (y2-y1)*360L + (m2-m1)*30L + d2-d1)
  
}


#' @rdname date_functions
#' @export
LastDayOfMonth <- function(x){
  z <- AddMonths(x, 1L)
  Day(z) <- 1L
  return(z - 1L)
}



#' @rdname date_functions
#' @export
YearDays <- function (x) {
  # return the number of days in the specific year of x
  x <- as.POSIXlt(x)
  x$mon[] <- x$mday[] <- x$sec[] <- x$min <- x$hour <- 0
  x$year <- x$year + 1
  return(as.POSIXlt(as.POSIXct(x))$yday + 1)
}


#' @rdname date_functions
#' @export
MonthDays <- function (x) {
  # return the number of days in the specific month of x
  x <- as.POSIXlt(x)
  x$mday[] <- x$sec[] <- x$min <- x$hour <- 0
  x$mon <- x$mon + 1
  return(as.POSIXlt(as.POSIXct(x))$mday)
}





#' @rdname date_functions
#' @export
Zodiac <- function(x, lang = c("en","de"), stringsAsFactors = TRUE) {
  
  switch(match.arg(lang, choices=c("en","de"))
         , en = {z <- c("Capricorn","Aquarius","Pisces","Aries","Taurus","Gemini","Cancer","Leo","Virgo","Libra","Scorpio","Sagittarius","Capricorn") }
         , de =  {z <- c("Steinbock","Wassermann","Fische","Widder","Stier","Zwillinge","Krebs","Loewe","Jungfrau","Waage","Skorpion","Schuetze","Steinbock") }
  )
  
  # i <- cut(DescTools::Month(x)*100 + DescTools::Day(x),
  #          breaks=c(0,120,218,320,420,520,621,722,822,923,1023,1122,1221,1231))
  i <- cut(Month(x) * 100 + DescToolsX::Day(x), 
           breaks = c(0, 120, 218, 320, 420, 520, 621, 
                      722, 823, 922, 1023, 1122, 1222, 1231), 
           right=FALSE, include.lowest = TRUE)
  
  if(stringsAsFactors){
    res <- i
    levels(res) <- z
  } else {
    res <- z[i]
  }
  return(res)
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
  
  if(!isWhole(x))
    .Call("_DescToolsX_isLeapYearDate", x, PACKAGE="DescToolsX")
  else 
    .Call("_DescToolsX_isLeapYearInt", x, PACKAGE="DescToolsX")
  
}



