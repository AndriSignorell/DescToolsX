
#' Display Compact Abstract of a Data Frame
#'
#' Compactly display the content and structure of a `data.frame`, including
#' variable labels. `str()` is optimized for lists and its output is
#' relatively technical, when it comes to e.g. attributes. `summary()` on
#' the other hand already calculates some basic statistics.
#'
#' The levels of a factor and describing variable labels (as created by
#' [bedrock::label()]) will be wrapped within the columns.
#'
#' The first 4 columns are printed with the needed fix width, the last 2
#' (Levels and Labels) are wrapped within the column. The width is calculated
#' depending on the width of the screen as given by `getOption("width")`.
#'
#' `toWord` has an interface for the class `Abstract`.
#'
#' @name abstract
#'
#' @param x a `data.frame` to be described
#' @param sep the separator for concatenating the levels of a factor
#' @param zeroForm a symbol to be used when a variable has zero NAs
#' @param maxLevels integer; maximum number of factor levels to display.
#'        Default is 5. Set this to `Inf` if all levels are needed.
#' @param maxVars integer; maximum number of variables (rows) to display.
#'        Default is `Inf`, meaning all variables.
#' @param truncate logical; whether level names exceeding the column
#'        width should be truncated. Default is `TRUE`.
#'
#' @return a data frame of class `Abstract` with columns:
#'
#' \describe{
#'   \item{`Nr`}{column number}
#'   \item{`Class`}{column class}
#'   \item{`ColName`}{column name}
#'   \item{`NAs`}{number of missing values}
#'   \item{`Levels`}{factor levels, if applicable}
#'   \item{`Label`}{descriptive column label}
#' }
#'
#' When printing, the `Label` column is hidden if no labels are set.
#'
#' @seealso [utils::str()], [base::summary()], [columnWrap()],
#' [DescToolsX::desc()]
#'
#' @examples
#'
#' d.mydata <- CO2
#'
#' # let's use describing labels
#' label(d.mydata) <- "CO2 contains data from an experiment on the cold
#' tolerance of the grass species Echinochloa crus-galli."
#'
#' label(d.mydata$Plant) <- "an ordered factor with levels Qn1 < Qn2 < Qn3 < ... < Mc1
#' giving a unique identifier for each plant."
#'
#' label(d.mydata$Type) <- "a factor with levels Quebec Mississippi giving the
#' origin of the plant"
#'
#' abstract(d.mydata)
#'
#' @family data.inspection
#' @concept summary
#' @export
abstract <- function(x, sep = ", ", zeroForm = ".", maxLevels = 5,
                     maxVars = Inf, truncate = TRUE) {

  # deparse BEFORE x is touched: assigning to a formal replaces its
  # promise, after which substitute() would return the value - i.e. the
  # whole data frame - instead of the call.
  mainTxt <- gsub(" +", " ", paste(deparse(substitute(x)), collapse = " "))

  if (!is.data.frame(x))
    x <- as.data.frame(x)

  # unlist() on an empty list returns NULL, not character(0), so with zero
  # columns every derived column below vanished from the data.frame and
  # the reordering by name failed with "undefined columns selected".
  if (ncol(x) == 0L) {
    res <- data.frame(Nr = integer(0), Class = character(0),
                      ColName = character(0), NAs = character(0),
                      Levels = character(0), Label = character(0),
                      stringsAsFactors = FALSE)
    attr(res, "main")     <- mainTxt
    attr(res, "nrow")     <- nrow(x)
    attr(res, "ncol")     <- 0L
    attr(res, "complete") <- 0L
    attr(res, "truncate") <- truncate
    class(res) <- append(class(res), "Abstract", after = 0)
    return(res)
  }

  if (length(maxLevels) != 1L || (!is.na(maxLevels) && maxLevels < 1))
    stop("'maxLevels' must be a single number >= 1 (or Inf/NA for all)")

  if (length(maxVars) != 1L || (!is.na(maxVars) && maxVars < 0))
    stop("'maxVars' must be a single non-negative number (or Inf for all)")

  shortclass <- function(x) {
    z <- unlist(lapply(x, function(z) paste(class(z), collapse = ", ")))
    res <- tolower(substr(z, 1, 3))
    res <- gsub("cha", "chr", res)
    # z <- c("integer", "date", "numeric", "factor", "logical", "ordered")
    return(res)
  }

  nRow <- nrow(x)

  res <- data.frame(
    nr = seq_along(x),
    class = shortclass(x),
    varname = colnames(x),
    label = unlist(lapply(lapply(x, label), coalesceX, "-")),
    levels = unlist(lapply(
      x,
      function(z) {
        if (nlevels(z) > 0) {
          nShow <- if (is.na(maxLevels) || is.infinite(maxLevels))
            nlevels(z)
          else
            min(nlevels(z), maxLevels)

          # seq_len(), not 1:nShow - the latter counts backwards from 1
          # for nShow == 0 and would index level 0 and level 1.
          idx <- seq_len(nShow)

          txt <- gettextf(
            "(%s): %s", nlevels(z),
            paste(idx, "-", levels(z)[idx],
                  sep = "", collapse = sep
            )
          )

          if (nShow < nlevels(z)) {
            txt <- paste(txt, ", ...", sep = "")
          }

          txt
        } else {
          ""
        }
      }
    )),
    NAs = unlist(lapply(x, function(z) sum(is.na(z)))),
    stringsAsFactors = FALSE
  )

  res$NAs <- ifelse(res$NAs != 0,
                    paste(res$NAs, " (",
                          fm(res$NAs / max(nRow, 1L), fmt = "%", digits = 1), ")",
                          sep = ""
                    ), zeroForm
  )

  rownames(res) <- NULL
  res <- res[, c("nr", "class", "varname", "NAs", "levels", "label")]
  colnames(res) <- c("Nr", "Class", "ColName", "NAs", "Levels", "Label")

  # seq_len() again: a data frame without columns gave 1:0 and hence two
  # rows, the first of them all NA.
  res <- res[seq_len(min(nrow(res), maxVars)), , drop = FALSE]

  attr(res, "main") <- mainTxt
  attr(res, "nrow") <- nRow
  attr(res, "ncol") <- ncol(x)
  # complete.cases can not be constructed with lists in data.frames
  attr(res, "complete") <-
    ifelse(all(vapply(x, is.atomic, logical(1L))), sum(complete.cases(x)), NA)
  attr(res, "truncate") <- truncate

  if (!is.null(attr(x, "label"))) {
    attr(res, "label") <- attr(x, "label")
  }

  class(res) <- append(class(res), "Abstract", after = 0)

  return(res)
}



#' @param width console width. If `NULL`, defaults to
#'        [options("width")][base::options()].
#' @param print.gap integer; number of spaces between columns
#' @param ... further arguments passed to the `print` method
#' @rdname abstract
#' @method print Abstract
#' @export
print.Abstract <- function(x, width = NULL, truncate = NULL,
                           print.gap = 2, ...) {

  # 'sep' used to sit in this signature but was never read - it belongs to
  # abstract(), where the levels are pasted together.

  # check if there are labels, if there aren't, we will hide the labels column
  lbl_fg <- !all(x["Label"] == "-")

  if (is.null(width)) {
    # the header is part of the column too, so the fixed columns must be
    # at least as wide as their names
    fixed <- vapply(
      seq_len(4L),
      function(i) max(c(nchar(as.character(x[[i]])),
                        nchar(colnames(x)[i])), na.rm = TRUE) + 1,
      numeric(1L))

    nCol <- 4L + 1L + lbl_fg
    rest <- (getOption("width") - (sum(fixed) + nCol * print.gap)) /
      (1 + lbl_fg)

    # a narrow console produced a negative width here, which then reached
    # strTrunc() as a negative maxlen
    rest <- max(rest, 12)

    width <- c(fixed, rep(rest, 1 + lbl_fg))
  }

  opt <- options(max.print = 1e4)
  on.exit(options(opt))


  cat(lineSep(), "\n")
  cat(attr(x, "main"))

  label <- attr(x, "label")

  if (!is.null(label)) {
    cat(" :", strwrap(label, indent = 2, exdent = 2), sep = "\n")
  } else {
    cat("\n")
  }

  cat(gettextf(
    "\ndata frame:\t%s obs. of  %s variables\n\t\t%s complete cases (%s)\n\n",
    attr(x, "nrow"), attr(x, "ncol"), attr(x, "complete"),
    fm(attr(x, "complete") / attr(x, "nrow"), fmt = "%", digits = 1)
  ))

  class(x) <- "data.frame"

  if (!lbl_fg) {
    x["Label"] <- NULL
  }

  if (nrow(x) == 0L) {
    cat("<no variables>\n\n")
    return(invisible(x))
  }

  res <- apply(x, 1, columnWrap, width = width)
  res <- data.frame(
    if (is.matrix(res)) {
      t(res)
    } else {
      do.call(rbind, res)
    },
    stringsAsFactors = FALSE
  )

  colnames(res) <- colnames(x)

  if (coalesceX(truncate, attr(x, "truncate"), TRUE)) {
    res[, ] <- sapply(
      seq_len(ncol(res)),
      function(i) strTrunc(res[, i], maxlen = width[i])
    )
  }

  res$NAs <- strAlign(res$NAs, " ")

  print(x = res, print.gap = print.gap, right = FALSE, row.names = FALSE, ...)
  cat("\n")

  invisible(x)
}
