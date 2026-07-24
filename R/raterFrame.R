
#' Create a Data.frame for Interrater Agreement
#'
#' Creates a \code{data.frame} for a formula \code{rating ~ subjects | raters}
#' with the subjects in rows and the raters in columns as base structure for
#' interrater agreement (IRA) functions.
#'
#' Assessments made by raters are typically - and appropriately - stored and
#' organized in databases. Data originating from databases are usually in long
#' format. Converting this long format into a wide format suitable for analysis
#' is a complex task, as it is not just a matter of simply displaying
#' consecutive values in a new column (as in usual "from-long-to-wide"
#' operations), but of assigning the values to the correct subjects, which
#' requires a sequential join.\cr The present function supports this process by
#' converting long-format data into a wide format that can be used by
#' subsequent inter-rater agreement functions. Missing values are marked as
#' \code{NA}s.
#'
#' \code{na.action} is applied to the \emph{wide} frame, i.e. per subject:
#' \code{na.omit} removes subjects with at least one missing rating. The
#' \code{"na.action"} attribute of the result carries a \code{"values"}
#' attribute with the identifiers of the omitted subjects.
#'
#' @param formula something like \code{rating ~ subjects | raters}
#' @param data the data
#' @param subset potential subset, evaluated in the long-format data
#' @param na.action what should happen with missings, applied per subject
#'   (i.e. to the rows of the wide result)
#' @param dropSubj logical; whether to drop the subject column (default
#'   \code{FALSE})
#' @return a \code{data.frame} of class \code{"raterFrame"} with subjects in rows and
#'   raters in columns
#'   
#' @seealso [bedrock::resolveFormula]
#' 
#' @examples
#'
#' d.long <- data.frame(
#'      expand.grid(subj=as.character(1:5), rater=LETTERS[1:3]),
#'      rating = c(1, 4, 5, 7, 2, 2, 5, 6, 7, 1, 1, 4, 6, 6, 2))
#'
#' # default rater frame
#' raterFrame(rating ~ subj | rater, data=d.long)
#'
#' # introduce some NAs
#' raterFrame(rating ~ subj | rater, data=d.long[-c(3, 6), ])
#'
#' # omit cases containing NAs
#' raterFrame(rating ~ subj | rater, data=d.long[-c(3, 6), ],
#'            na.action=na.omit)
#'
#' # omit the subject column
#' raterFrame(rating ~ subj | rater, data=d.long, dropSubj=TRUE)
#'
#' @family assoc.agreement
#' @concept agreement
#' @concept rater-data
#'
#'
#' @export
raterFrame <- function(formula, data, subset, na.action, dropSubj = FALSE) {

  # capture subset unevaluated, following the resolveFormula() contract
  # (avoids the collision with base::subset)
  subset_expr <- if (!missing(subset)) substitute(subset) else NULL

  # na.pass here on purpose: na.action must act per *subject*, i.e. on the
  # wide frame after reshaping -- missing combinations do not even exist as
  # rows in the long format
  r <- resolveFormula(formula, data,
                      subset    = subset_expr,
                      na.action = na.pass,
                      allowed   = "n-sample-dependent")

  mf <- r$mf   # columns: rating, subject, rater (order guaranteed)

  dname <- gettextf("%s by %s (rows) and %s (columns)",
                    names(mf)[1L], names(mf)[2L], names(mf)[3L])

  # --- long -> wide -------------------------------------------------------
  m <- reshape(mf, idvar = names(mf)[2L], timevar = names(mf)[3L],
               direction = "wide")

  # order rows by subject, columns by rater
  m <- m[order(m[[1L]]), ]
  m <- cbind(m[, 1L, drop = FALSE], m[, -1L][, order(colnames(m)[-1L])])

  # strip the "<rating>." prefix from the rater columns
  # (fixed-prefix removal, robust against regex metacharacters in the name)
  pfx <- paste0(names(mf)[1L], ".")
  hit <- startsWith(colnames(m), pfx)
  colnames(m)[hit] <- substring(colnames(m)[hit], nchar(pfx) + 1L)
  rownames(m) <- NULL

  # --- na.action on the wide frame ---------------------------------------
  if (!missing(na.action)) {
    subj <- m[[names(mf)[2L]]]
    m <- na.action(m)
    # provide the identifiers of omitted subjects
    attr(attr(m, "na.action"), "values") <-
      subj[as.integer(attr(m, "na.action"))]
  }

  attr(m, "data.name") <- dname

  # remove the subject column if not required
  if (dropSubj)
    m <- m[, -1L, drop = FALSE]

  class(m) <- c("raterFrame", class(m))

  m
}


