
#' Generation by Birthyear
#' 
#' Yields the generation of a person based on the year of birth.
#' 
#' The generations are defined as:\cr
#' 
#' \tabular{ll}{ \code{1946-1964} \verb{ } \tab Babyboomer\cr \code{1965-1979}
#' \tab Generation X \cr \code{1980-1995} \tab Generation Y – also known as
#' Millennials\cr \code{1996-2010} \tab Generation Z \cr \code{2011-2025} \tab
#' Generation Alpha\cr }
#' 
#' @param year year of birth
#' @return Ordered factor with levels \code{c("Babyboomer", "Gen X",
#' "Millennials", "Gen Z", "Gen Alpha")}\cr Values which fall outside the range
#' of breaks are coded as \code{NA}, as are \code{NaN} and \code{NA} values.
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{cutAge}}, \code{\link{zodiac}}
#' @examples
#' 
#' generation(c(1946, 1964, 1972, 2001, 2003, 2018, 2026))
#' 


#' @export
generation <- function(year){
  
  # Babyboomer   (1946-1964)
  # Generation X (1965-1979)
  # Generation Y (1980-1995) – also called Millennials
  # Generation Z (1996-2010)
  # Generation Alpha (ab 2011-2025)
  
  cut(year,
      breaks=c(1946, 1965, 1980, 1996, 2011, Inf), right=FALSE, 
      labels = c("Babyboomer","Gen X","Millennial","Gen Z","Gen Alpha"),
      ordered = TRUE)
  
}

