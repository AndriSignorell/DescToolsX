
#' Generation by Birth Year
#' 
#' Yields the generation of a person based on the year of birth.
#' 
#' The generations are defined as:\cr
#' 
#' \tabular{ll}{ 
#' \bold{birth year} \verb{ } \tab \bold{label}\cr 
#' \code{1946-1964} \verb{ } \tab Babyboomer\cr 
#' \code{1965-1979}
#' \tab Generation X \cr \code{1980-1995} \tab Generation Y - also known as
#' Millennials\cr \code{1996-2010} \tab Generation Z \cr \code{2011 and later}
#' \tab Generation Alpha\cr }
#'
#' The last class is left open at the top. The table formerly gave it as
#' 1946-2025 while the code used \code{Inf}, so a birth year of 2026 was
#' documented as \code{NA} and returned as \code{"Gen Alpha"}. Naming the
#' successor generation is not settled enough to hard-code an upper bound.
#' 
#' @param year year of birth
#' 
#' @return ordered factor with levels 
#' \code{c("Babyboomer", "Gen X",
#' "Millennial", "Gen Z", "Gen Alpha")}\cr
#' Values which fall outside the range
#' of breaks are coded as \code{NA}, as are \code{NaN} and \code{NA} values.
#' 
#' @seealso \code{\link{cutAge}}
#' @examples
#' 
#' generation(c(1946, 1964, 1972, 2001, 2003, 2018, 2026))
#' 
#' @family date.time
#' @concept date-time
#' @concept categorization
#' @export
generation <- function(year){
  
  # Babyboomer   (1946-1964)
  # Generation X (1965-1979)
  # Generation Y (1980-1995) - also called Millennials
  # Generation Z (1996-2010)
  # Generation Alpha (ab 2011-2025)
  
  # ordered_result, spelled out: 'ordered' only worked through partial
  # matching against cut.default()'s formal
  cut(year,
      breaks = c(1946, 1965, 1980, 1996, 2011, Inf), right = FALSE,
      labels = c("Babyboomer", "Gen X", "Millennial", "Gen Z", "Gen Alpha"),
      ordered_result = TRUE)
  
}
