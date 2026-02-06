

#' DescToolsX Constants
#'
#' English abbreviated and full weekday names, starting with Monday.
#' Metric prefixes with name, abbreviation and dimension.
#'
#' @name constants
#' @format A character vector of length 7.
#' @keywords internal


#' @rdname constants
#' @export
day.abb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")


#' @rdname constants
#' @export
day.name <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")


#' @rdname constants
#' @export
d.prefix <- data.frame(
  pref = c("yotta", "zetta", "exa", "peta", "tera", "giga", "mega", "kilo", 
           "hecto", "deca", "deci", "centi", "milli", "micro", "nano", 
           "pico", "femto", "atto", "zepto", "yocto"), 
  abbr = c("Y", "Z", "E", "P", "T", "G", "M", "k", "h", "da", "d", "c", 
           "m", "u", "n", "p", "f", "a", "z", "y"), 
  mult = c(1e+24, 1e+21, 1e+18, 1e+15, 1e+12, 1000000000, 1000000, 1000, 100, 
           10, 0.1, 0.01, 0.001, 0.000001, 0.000000001, 1e-12, 1e-15, 
           1e-18, 1e-21, 1e-24)
)

