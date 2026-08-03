#' Calculate the Zodiac of a Date
#'
#' Calculate the sign of zodiac of a date.
#'
#' The really relevant things can sometimes hardly be found. You just
#' discovered such a function... ;-)
#'
#' The following rule to determine zodiac symbols is implemented:
#'
#' \preformatted{Dec. 22 - Jan. 19 : Capricorn | Jan. 20 - Feb. 17 : Aquarius |
#' Feb. 18 - Mar. 19 : Pisces | March 20 - April 19 : Aries | April 20 - May 19 :
#' Taurus | May 20 - June 20 : Gemini | June 21 - July 21 : Cancer | July 22 - Aug.
#' 22 : Leo | Aug 23 - Sept. 21 : Virgo | Sept. 22 - Oct. 22 : Libra | Oct. 23 -
#' Nov. 21 : Scorpio | Nov. 22 - Dec. 21 : Sagittarius }
#'
#' The boundaries are fixed calendar dates; the astronomical dates of the sun's
#' entry into a sign shift by up to a day from year to year.
#'
#' @param x the date to transform, a \code{Date} or anything that
#' \code{month()} and \code{day()} accept
#' @param lang language of the zodiac names, either English (\code{"en"}) or
#' German (\code{"de"})
#' @param stringsAsFactors logical. If set to \code{TRUE} (default) the result
#' will consist of a factor with zodiac signs as levels.
#'
#' @return a character vector or factor containing the zodiac signs
#'
#' @note Based on code from Markus Naepflin, adapted to conform to package standards.
#'
#' @examples
#'
#' zodiac(as.Date(c("1937-07-28", "1936-06-01", "1966-02-25",
#'                  "1964-11-17", "1972-04-25")), lang="de")
#'
#' # the boundary days
#' zodiac(as.Date(c("2015-01-19", "2015-01-20", "2015-12-21", "2015-12-22")))
#'
#' set.seed(1)
#' d <- sample(seq(as.Date("2015-01-01"), as.Date("2015-12-31"), 1), 120)
#' z <- zodiac(d)
#' desc(z)
#'
#' @family date.time
#' @concept date-time
#' @concept categorization
#'
#' @export
zodiac <- function(x, lang = c("en", "de"), stringsAsFactors = TRUE) {

  lang <- match.arg(lang)

  if(!isTRUE(stringsAsFactors) && !isFALSE(stringsAsFactors))
    stop("Argument 'stringsAsFactors' must be TRUE or FALSE.")

  # 13 labels for the 13 intervals below; Capricorn spans the turn of the
  # year and therefore appears twice. levels<-() with duplicated values
  # merges the two into a single level.
  z <- switch(lang,
              en = c("Capricorn", "Aquarius", "Pisces", "Aries", "Taurus",
                     "Gemini", "Cancer", "Leo", "Virgo", "Libra", "Scorpio",
                     "Sagittarius", "Capricorn"),
              de = c("Steinbock", "Wassermann", "Fische", "Widder", "Stier",
                     "Zwillinge", "Krebs", "Loewe", "Jungfrau", "Waage",
                     "Skorpion", "Schuetze", "Steinbock"))

  # month*100 + day, cut with right=FALSE, so every break is the FIRST day
  # of the corresponding sign:
  #   [   0, 120) Jan 01 - Jan 19  Capricorn
  #   [ 120, 218) Jan 20 - Feb 17  Aquarius
  #   [ 218, 320) Feb 18 - Mar 19  Pisces
  #   [ 320, 420) Mar 20 - Apr 19  Aries
  #   [ 420, 520) Apr 20 - May 19  Taurus
  #   [ 520, 621) May 20 - Jun 20  Gemini
  #   [ 621, 722) Jun 21 - Jul 21  Cancer
  #   [ 722, 823) Jul 22 - Aug 22  Leo
  #   [ 823, 922) Aug 23 - Sep 21  Virgo
  #   [ 922,1023) Sep 22 - Oct 22  Libra
  #   [1023,1122) Oct 23 - Nov 21  Scorpio
  #   [1122,1222) Nov 22 - Dec 21  Sagittarius
  #   [1222,1231] Dec 22 - Dec 31  Capricorn
  i <- cut(month(x) * 100 + day(x),
           breaks = c(0, 120, 218, 320, 420, 520, 621,
                      722, 823, 922, 1023, 1122, 1222, 1231),
           right = FALSE, include.lowest = TRUE)

  if(stringsAsFactors){
    res <- i
    levels(res) <- z
  } else {
    res <- z[as.integer(i)]
  }

  res

}
