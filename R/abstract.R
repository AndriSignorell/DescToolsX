
#' Display Compact Abstract of a Data Frame
#'
#' Compactly display the content and structure of a `data.frame`, including
#' variable labels. `str()` is optimized for lists and its output is
#' relatively technical, when it comes to e.g. attributes. `summary()` on
#' the other hand already calculates some basic statistics.
#'
#' The levels of a factor and describing variable labels (as created by
#' \code{\link[bedrock]{label}()}) will be wrapped within the columns.
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
#' @param zeroForm a symbol to be used, when a variable has zero NAs.
#' @param maxLevels (integer, `Inf`) Max. number of factor levels to display.
#'        Default is 5. Set this to `Inf`, if all levels are needed.
#' @param maxVars (integer, `Inf`) Max. number of variables (rows) to
#'        display. Default is `Inf`, meaning all variables.
#' @param truncate logical, defining if level names exceeding the column
#'        width should be truncated. Default is `TRUE`.
#'
#' @return an object of class `Abstract`, essentially a character matrix
#' with 6 columns containing:
#'
#' 1. a column number (`Nr`),
#' 2. the name of the column (`ColName`),
#' 3. the column class (`Class`),
#' 4. the number of NAs (`NAs`),
#' 5. the levels if the variable is a factor (`Levels`),
#' 6. descriptive labels for the column (`Labels`).
#'
#' When printing, the `Labels` column is hidden if no labels are set.
#'
#' @seealso [utils::str()], [base::summary()], [columnWrap()],
#' [DescToolsX::desc()]
#'
#' @keywords print
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

  shortclass <- function(x) {
    z <- unlist(lapply(x, function(z) paste(class(z), collapse = ", ")))
    res <- tolower(substr(z, 1, 3))
    res <- gsub("cha", "chr", res)
    # z <- c("integer", "date", "numeric", "factor", "logical", "ordered")
    return(res)
  }


  res <- data.frame(
    nr = seq_along(x),
    class = shortclass(x),
    varname = colnames(x),
    label = unlist(lapply(lapply(x, label), coalesceX, "-")),
    levels = unlist(lapply(
      x,
      function(z) {
        if (nlevels(z) > 0) {
          maxLevels <- ifelse(is.na(maxLevels) || is.infinite(maxLevels),
                              nlevels(z), min(nlevels(z), maxLevels)
          )

          txt <- gettextf(
            "(%s): %s", nlevels(z),
            paste(1:maxLevels, "-", levels(z)[1:maxLevels],
                  sep = "", collapse = sep
            )
          )

          if (maxLevels < nlevels(z)) {
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
                          fm(res$NAs / dim(x)[1], fmt = "%", digits = 1), ")",
                          sep = ""
                    ), zeroForm
  )

  rownames(res) <- NULL
  res <- res[, c("nr", "class", "varname", "NAs", "levels", "label")]
  colnames(res) <- c("Nr", "Class", "ColName", "NAs", "Levels", "Label")

  res <- res[1:min(nrow(res), maxVars), ]

  attr(res, "main") <-
    gsub(" +", " ", paste(deparse(substitute(x)), collapse = " "))
  attr(res, "nrow") <- dim(x)[1]
  attr(res, "ncol") <- dim(x)[2]
  # complete.cases can not be constructed with lists in data.frames
  attr(res, "complete") <-
    ifelse(all(sapply(x, is.atomic)), sum(complete.cases(x)), NA)
  attr(res, "truncate") <- truncate

  if (!is.null(attr(x, "label"))) {
    attr(res, "label") <- attr(x, "label")
  }

  class(res) <- append(class(res), "Abstract", after = 0)

  return(res)
}



#' @param width Console width. If `NULL`, defaults to
#'        [options("width")][base::options()].
#' @param print.gap (integer) Number of spaces between columns.
#' @param ... Further arguments to `print` method.
#' @rdname abstract
#' @export
print.Abstract <- function(x, sep = NULL, width = NULL,
                           truncate = NULL, print.gap = 2, ...) {
  # check if there are labels, if there aren't, we will hide the labels column
  lbl_fg <- !all(x["Label"] == "-")

  if (is.null(width)) {
    width <- unlist(lapply(x, function(x) {
      max(nchar(as.character(x))) +
        1
    }))[1:4]
    width <-
      c(width, rep((getOption("width") - (sum(width) + 6 * print.gap)) /
                     (1 + lbl_fg), (1 + lbl_fg)))
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
}
