
#' Create a Data.frame for Interrater Agreement
#'
#' Creates a `data.frame` for a formula `rating ~ subjects | raters`
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
#' `NA`s.
#'
#' `na.action` is applied to the *wide* frame, i.e. per subject:
#' `na.omit` removes subjects with at least one missing rating. The
#' `"na.action"` attribute of the result carries a `"values"`
#' attribute with the identifiers of the omitted subjects; it is absent if
#' nothing was omitted.
#'
#' @param formula something like `rating ~ subjects | raters`
#' @param data the data
#' @param subset potential subset, evaluated in the long-format data
#' @param na.action what should happen with missings, applied per subject
#'   (i.e. to the rows of the wide result)
#' @param dropSubj logical; whether to drop the subject column (default
#'   `FALSE`)
#' @return a `data.frame` of class `"raterFrame"` with subjects in rows and
#'   raters in columns. The name of the subject column is kept in the
#'   `"subject"` attribute (`NA` if it was dropped).
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
#' @family agreement
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

  subjName <- names(mf)[2L]

  dname <- gettextf("%s by %s (rows) and %s (columns)",
                    names(mf)[1L], names(mf)[2L], names(mf)[3L])

  # reshape() keeps only the first of several rows with the same
  # subject/rater combination and merely warns; for a function whose whole
  # purpose is the sequential join that has to be an error
  dup <- duplicated(mf[, c(2L, 3L)])
  if (any(dup))
    stop(gettextf(
      paste("%d duplicated subject/rater combination(s) in the data, e.g. %s.",
            "Each rater may rate each subject only once."),
      sum(dup),
      paste(utils::head(paste(mf[dup, 2L], mf[dup, 3L], sep = "/"), 3L),
            collapse = ", ")), domain = NA)

  # --- long -> wide -------------------------------------------------------
  m <- reshape(mf, idvar = names(mf)[2L], timevar = names(mf)[3L],
               direction = "wide")

  # order rows by subject, columns by rater
  m <- m[order(m[[1L]]), ]
  # drop = FALSE: with a single rater m[, -1L] would collapse to a vector and
  # the column reordering would fail with "incorrect number of dimensions"
  m <- cbind(m[, 1L, drop = FALSE],
             m[, -1L, drop = FALSE][, order(colnames(m)[-1L]), drop = FALSE])

  # strip the "<rating>." prefix from the rater columns
  # (fixed-prefix removal, robust against regex metacharacters in the name)
  pfx <- paste0(names(mf)[1L], ".")
  hit <- startsWith(colnames(m), pfx)
  colnames(m)[hit] <- substring(colnames(m)[hit], nchar(pfx) + 1L)
  rownames(m) <- NULL

  # --- na.action on the wide frame ---------------------------------------
  naAct <- NULL
  if (!missing(na.action)) {
    subj <- m[[subjName]]
    m <- na.action(m)
    naAct <- attr(m, "na.action")
    # na.pass, identity and na.omit-with-nothing-to-omit leave no attribute;
    # attr<-() on NULL is an error, so guard before touching it
    if (!is.null(naAct))
      attr(naAct, "values") <- subj[as.integer(naAct)]
  }

  # remove the subject column if not required
  if (dropSubj)
    m <- m[, -1L, drop = FALSE]

  # attributes LAST: `[.data.frame` keeps only names, row.names and class, so
  # anything set before the dropSubj subsetting was silently discarded
  attr(m, "data.name") <- dname
  attr(m, "subject") <- if (dropSubj) NA_character_ else subjName
  if (!is.null(naAct))
    attr(m, "na.action") <- naAct

  class(m) <- c("raterFrame", class(m))

  m
}
