
#' Generation by Birth Year
#' 
#' Yields the generation of a person based on the year of birth.
#' 
#' The generations are defined as:\cr
#' 
#' \tabular{ll}{ 
#' **birth year** \verb{ } \tab **label**\cr 
#' `1946-1964` \verb{ } \tab Babyboomer\cr 
#' `1965-1979`
#' \tab Generation X \cr `1980-1995` \tab Generation Y - also known as
#' Millennials\cr `1996-2010` \tab Generation Z \cr `2011 and later`
#' \tab Generation Alpha\cr }
#'
#' The last class is left open at the top. The table formerly gave it as
#' 1946-2025 while the code used `Inf`, so a birth year of 2026 was
#' documented as `NA` and returned as `"Gen Alpha"`. Naming the
#' successor generation is not settled enough to hard-code an upper bound.
#' 
#' @param year year of birth
#' 
#' @return ordered factor with levels 
#' `c("Babyboomer", "Gen X",
#' "Millennial", "Gen Z", "Gen Alpha")`\cr
#' Values which fall outside the range
#' of breaks are coded as `NA`, as are `NaN` and `NA` values.
#' 
#' @seealso [cutAge()]
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
