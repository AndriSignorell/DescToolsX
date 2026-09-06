#' Create Table One Describing Baseline Characteristics
#'
#' Create a table summarizing continuous, categorical and dichotomous
#' variables, optionally stratified by one or more variables, while performing
#' adequate statistical tests.
#'
#' In research the characteristics of study populations are often characterised
#' through some kind of a "Table 1", containing descriptives of the used
#' variables, as mean/standard deviation for continuous variables, and
#' proportions for categorical variables. In many cases, a comparison is made
#' between groups within the framework of the scientific question.
#'
#' \figure{tOne.png}{Table 1}
#'
#' Creating such a table can be very time consuming and there's a need for a
#' flexible function that helps us to solve the task. `tOne()` is designed
#' to be easily used with sensible defaults, and yet flexible enough to allow
#' free definition of the essential design elements.
#'
#' This is done by breaking down the descriptive task to three types of
#' variables: quantitative (numeric, integer), qualitative (factor, characters)
#' and dichotomous variables (the latter having exactly two values or levels).
#' Depending on the variable type, the descriptives and the according sensible
#' tests are chosen. By default mean/sd are chosen to describe numeric
#' variables.
#' \preformatted{
#'   FUN = function(x)
#'           gettextf("\%s (\%s)",
#'                    fm(mean(x, na.rm = TRUE), fmt = fmt$num),
#'                    fm(sd(x, na.rm = TRUE), fmt = fmt$num))
#' }
#'
#' Their difference is tested with the Kruskal-Wallis test. For categorical
#' variables the absolute and relative frequencies are calculated and tested
#' with a chi-square test. \cr The tests can be changed with the argument
#' `TEST`. These must be organised as list containing elements named
#' `"num"`, `"cat"` and `"dich"`. Each of them must be a
#' function with arguments `(x, g)`, returning something similar to a
#' p-value.
#' \preformatted{
#'   TEST = list( num = list(fun = function(x, g){
#'       summary(aov(x ~ g))\verb{[[1]][1, "Pr(>F)"]}}, lbl = "ANOVA"),
#'     cat = list(fun = function(x, g){
#'       chisq.test(table(x, g))$p.val}, lbl = "Chi-Square test"),
#'     dich = list(fun = function(x, g){
#'       fisher.test(table(x, g))$p.val}, lbl = "Fisher exact test")
#'   ) }
#'
#' The legend text of the test, which is appended to
#' the table together with the significance codes, can be set with the variable
#' `lbl`.
#'
#' Great importance was attached to the free definition of the number fms.
#' By default, the optionally definable fm templates of **DescToolsX**
#' are used. Deviations from this can be freely passed as arguments to the
#' function. fms can be defined for integers, floating point numbers,
#' percentages and for the p-values of statistical tests. All options of the
#' function [pharos::fm()] are available and can be provided as a list.
#' See examples which show several different implementations.
#' \preformatted{
#'   fmt = list(abs  = "abs.sty",
#'              num  = "num.sty",
#'              per  = "per.sty",
#'              pval = style(fmt = "*", naForm = "   ")
#'              ) }
#'
#' Several tables can be appended using [bedrock::appendX()].
#' This can be useful,
#' if e.g. the `mean/sd` AND `median/IQR` should be displayed together.
#' Another use case is to introduce a delimiter row.
#'
#' The function returns a character matrix as result, which can easily be
#' subset or combined with other matrices. An interface for
#' `toWrd()` is available such that the matrix can be transferred
#' to MS-Word. Both font and alignment are freely selectable in the Word table.
#'
#' @usage tOne(
#'   x, groups = NA, add.length = TRUE,
#'   colnames = NULL, vnames = NULL, total = TRUE,
#'   align = "\\\\l", FUN = NULL, TEST = NULL,
#'   intref = "high",
#'   fmt = list(abs = "abs.sty", num = "num.sty", per = "per.sty",
#'              pval = style(fmt = "*", naForm = "   "))
#' )
#'
#' @name tOne
#'
#' @param x a data.frame containing all the variables to be included in the
#' table. `NA` inserts a title row containing `vnames` only.
#' @param groups the grouping variable
#' @param add.length logical. If set to `TRUE` (default), a row with the
#' group sizes will be inserted as first row of the table.
#' @param colnames a vector of column names for the result table
#' @param vnames a vector of variable names to be placed in the first column
#' instead of the real names
#' @param total logical (default `TRUE`), defines whether the results
#' should also be displayed for the whole, ungrouped variable
#' @param align the character on whose position the strings will be aligned.
#' Left alignment can be requested by setting `sep = "\\l"`, right
#' alignment by `"\\r"` and center alignment by `"\\c"`. Mind the
#' backslashes, as if they are omitted, strings would be aligned to the
#' **character** **l**, **r** or **c** respectively. Default value
#' is `"\\l"`, thus left alignment.
#' @param FUN the function to be used as location and dispersion measure for
#' numeric (including integer) variables (`mean`/`sd` is default,
#' alternatives as `median`/`IQR` are possible by defining a
#' function). See examples.
#'
#' @param TEST a list of functions to be used to test the variables. Must be
#' named as `"num"`, `"cat"` and `"dich"` and be defined as
#' function with arguments `(x, g)`, generating something similar to a
#' p-value. Use `TEST=NA` to suppress test. (See examples.)
#'
#' @param intref one out of `"high"` (default), `"low"` or
#' `"both"`, defining which value of a dichotomous variable should be
#' reported. Usually this will be `1` or `TRUE`. Setting it to
#' `"low"` will report the lower value `0` or `FALSE`,
#' `"both"` reports the variable as a categorical one with all its
#' levels. Dichotomous factors are treated the same way, `"high"`
#' reporting the last and `"low"` the first level.
#'
#' @param fmt fm codes for absolute, numeric and percentage values, and for
#' the p-values of the tests
#'
#' @return a character matrix of class `tOne`
#'
#' @seealso [bedrock::appendX()]
#'
#'
#' @examples
#'
#' opt <- options(scipen = 8)
#'
#' # define some special fms for count data, percentages and numeric results
#' # (those will be supported by tOne)
#' abs.sty <- style(digits = 0, bigMark = "'")   # counts
#' per.sty <- style(digits = 1, fmt = "%")        # percentages
#' num.sty <- style(digits = 1, bigMark = "'")   # numeric
#'
#' tOne(x = Pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")],
#'   groups = Pizza$quality)
#'
#' # the same but no groups now...
#' tOne(x = Pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")])
#'
#' # define median/IQR as describing functions for the numeric variables
#' tOne(iris[, -5], iris[, 5],
#'   FUN = function(x) {
#'     gettextf("%s / %s",
#'       fm(median(x, na.rm = TRUE), digits = 1),
#'       fm(IQR(x, na.rm = TRUE), digits = 3))
#'   }
#' )
#'
#' # replace kruskal.test by ANOVA and report the p.value
#' # Change tests for all the types
#' tOne(x = iris[, -5], groups = iris[, 5],
#'      FUN = function(x) gettextf("%s / %s",
#'             fm(mean(x, na.rm = TRUE), digits = 1),
#'             fm(sd(x, na.rm = TRUE), digits = 3)),
#'
#'      TEST = list(
#'        num = list(fun = function(x, g){summary(aov(x ~ g))[[1]][1, "Pr(>F)"]},
#'                         lbl = "ANOVA"),
#'                cat = list(fun = function(x, g){chisq.test(table(x, g))$p.val},
#'                         lbl = "Chi-Square test"),
#'                dich = list(fun = function(x, g){fisher.test(table(x, g))$p.val},
#'                          lbl = "Fisher exact test")),
#'        fmt = list(abs = "abs.sty", num  = "num.sty", per = "per.sty",
#'                 pval = style(fmt = "*", naForm = "   "))
#' )
#'
#' t1 <- tOne(x     = Pizza[,c("temperature", "driver", "rebate")],
#'            groups   = Pizza$area,
#'            align = " ",
#'            total = FALSE,
#'
#'            FUN = function(x) gettextf("%s / %s (%s)",
#'                                       fm(mean(x, na.rm = TRUE), digits = 1),
#'                                       fm(sd(x, na.rm = TRUE), digits = 3),
#'                                       fm(median(x, na.rm = TRUE), digits = 1)),
#'
#'            TEST = NA,
#'
#'            fmt = list(abs  = style(bigMark = " ", digits=0),
#'                       num  = style(bigMark = " ", digits=1),
#'                       per  = style(fmt=function(x)
#'                           strPad(fm(x, fmt="%", digits=1), width=5, adj = "r")),
#'                       pval = style(fmt = "*", naForm = "   "))
#' )
#' # add a userdefined legend
#' attr(t1, "legend") <- "numeric: mean / sd (median)), factor: n (n%)"
#'
#' t1
#'
#'
#' # dichotomous integer or logical values can be reported by the high or low value
#' set.seed(1)
#' x <- sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE)
#' y <- sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE) == 1
#' z <- factor(sample(x = c(0, 1), size = 100, prob = c(0.3, 0.7), replace = TRUE))
#' g <- sample(x = letters[1:4], size = 100, replace = TRUE)
#' d.set <- data.frame(x = x, y = y, z = z, g = g)
#'
#' tOne(d.set[1:3], d.set$g, intref = "low")
#'
#' tOne(d.set[1:3], d.set$g, intref = "high")
#'
#' # report both levels of the factor
#' tOne(data.frame(z = z), g, intref = "both")
#'
#' options(opt)
#'
#'
#' \dontrun{
#'
#' # Send the whole stuff to Word
#' wrd <- getNewWrd()
#' toWrd(
#'   tOne(x   = Pizza[, c("temperature", "delivery_min", "driver", "wine_ordered")],
#'        groups = Pizza$quality,
#'        fmt = list(num=style(digits=1))
#'        ),
#'   font = list(name="Arial narrow", size=8),
#'   align = c("l","r")      # this will be recycled: left-right-left-right ...
#' )
#' }
#'
#'
#'
#' @rdname tOne
#'
#' @family frequency
#' @concept frequency-table
#' @concept table-summary
#'
#' @export
tOne <- function(x, groups = NA, add.length=TRUE,
                 colnames=NULL, vnames=NULL, total=TRUE,
                 align="\\l", FUN = NULL, TEST = NULL, intref="high",
                 fmt=list(abs  = "abs.sty",
                          num  = "num.sty", per="per.sty",
                          pval = style(fmt = "*", naForm = "   ")) ) {


  # set the fms, take the provided fmt and combine with defaults
  fmt <- c(fmt,
           list(abs  = "abs.sty",
                num  = "num.sty",
                per =  "per.sty",
                pval = style(fmt = "*", naForm = "   ")))
  # use the first instance, so user defined fms are preferred
  # and the standards come into effect if there are no user specifications.
  # NOTE: this must deduplicate by NAME - duplicated(fmt) compares the
  # VALUES and would drop a default whose value happens to coincide with a
  # user supplied one (e.g. fmt=list(num="abs.sty") killed fmt$abs).
  fmt <- fmt[!duplicated(names(fmt))]

  intref <- match.arg(intref, choices = c("high", "low", "both"))

  has_x <- !identical(x, NA)

  if(has_x && mode(x) %in% c("logical", "numeric", "complex", "character"))
    x <- data.frame(x)

  # set the variablenames per row
  if(is.null(vnames)){
    vnames <- if(is.null(colnames(x)))
      paste0("Var", seq_len(max(1L, NCOL(x))))    # NULL colnames: Var1, Var2, ...
    else
      colnames(x)
    default_vnames <- TRUE
  } else {
    default_vnames <- FALSE   # user supplied names are used verbatim
  }

  # creates the table one in a study
  if(is.null(FUN)){
    num_fun <- function(x){
      # the cell for numeric data
      gettextf("%s (%s)",
               fm(mean(x, na.rm=TRUE), fmt=fmt$num),
               fm(sd(x, na.rm=TRUE), fmt=fmt$num))
    }
  } else {
    num_fun <- FUN
  }


  no_groups <- identical(groups, NA)
  if(no_groups){
    # no grouping factor, let's define something appropriate
    groups <- rep(1, if(has_x) nrow(x) else length(groups))
    TEST <- NA
  }

  # the group columns as they will be produced by table()/tapply()
  glev <- if(is.factor(groups)) levels(groups)
          else sort(unique(groups[!is.na(groups)]))
  ngrp <- length(glev)


  if(identical(TEST, NA)){

    TEST <- list(num=list(fun=function(x, g) 1, lbl="None"),
                 cat=list(fun=function(x, g) 1, lbl="None"),
                 dich=list(fun=function(x, g) 1, lbl="None"))
    notest <- TRUE

  } else {

    # the default tests for quantitative and categorical data
    TEST.def <- list(num=list(fun=function(x, g){kruskal.test(x, g)$p.val},
                              lbl="Kruskal-Wallis test"),
                     cat=list(fun=function(x, g){chisq.test(table(x, g))$p.val},
                              lbl="Chi-Square test"),
                     dich=list(fun=function(x, g){fisher.test(table(x, g))$p.val},
                               lbl="Fisher exact test"))

    if(is.null(TEST))  # the defaults
      TEST <- TEST.def

    # define test for the single tests
    if(is.null(TEST[["num"]]))
      TEST[["num"]] <- TEST.def[["num"]]
    if(is.null(TEST[["cat"]]))
      TEST[["cat"]] <- TEST.def[["cat"]]
    if(is.null(TEST[["dich"]]))
      TEST[["dich"]] <- TEST.def[["dich"]]

    notest <- FALSE

  }

  num_test <- TEST[["num"]]$fun
  cat_test <- TEST[["cat"]]$fun
  dich_test <- TEST[["dich"]]$fun


  num_row <- function(x, g, total=TRUE, vname = deparse(substitute(x))){

    res <- fm(num_test(x, g), fmt=fmt$pval)

    return(
      cbind(var=vname, total = num_fun(x), rbind(tapply(x, g, num_fun)),
            paste(res, .FootNote(1)))
    )
  }


  cat_mat <- function(x, g, vname=deparse(substitute(x))){

    if(inherits(x, "character"))
      x <- factor(x)

    tab  <- table(x, g)
    ptab <- prop.table(tab, margin = 2)
    tab  <- addmargins(tab, 2)
    # the total column must use the same denominator as the total counts,
    # i.e. the row sums of the tabulated data. prop.table(table(x)) would
    # include observations with a missing group.
    ptab <- cbind(ptab, Sum = prop.table(tab[, "Sum"]))


    # crunch tab and ptab
    m <- matrix(NA, nrow=nrow(tab), ncol=ncol(tab))
    m[,] <- gettextf("%s (%s)",
                     fm(tab, fmt=fmt$abs),
                     fm(ptab, fmt=fmt$per))
    # totals to the left
    m <- m[, c(ncol(m), seq_len(ncol(m)-1L)), drop=FALSE]

    # set rownames
    m <- cbind( c(vname, paste(" ", levels(x))),
                rbind("", m))
    # add test
    if(nrow(tab)>1)
      p <- cat_test(x, g)
    else
      p <- NA

    m <- cbind(m, c(paste(fm(p, fmt=fmt$pval), ifelse(is.na(p), "", .FootNote(3))),
                    rep("", nrow(tab))))

    colnames(m) <- c("var", "total", head(colnames(tab), -1), "")

    return(m)

  }

  dich_mat <- function(x, g, vname=deparse(substitute(x))){

    tab <- table(x, g)

    if(identical(dim(tab), c(2L, 2L))){
      p <- dich_test(x, g)
      foot <- .FootNote(2)
    } else {
      p <- cat_test(x, g)
      foot <- .FootNote(3)
    }

    ptab <- prop.table(tab, 2)
    tab  <- addmargins(tab, 2)
    ptab <- cbind(ptab, Sum = prop.table(tab[, "Sum"]))

    m <- matrix(NA, nrow=nrow(tab), ncol=ncol(tab))
    m[,] <- gettextf("%s (%s)",
                     fm(tab, fmt=fmt$abs),
                     fm(ptab, fmt=fmt$per))

    # totals to the left
    m <- m[, c(ncol(m), seq_len(ncol(m)-1L)), drop=FALSE]

    m <- rbind(c(vname, m[1,], paste(fm(p, fmt=fmt$pval), foot)))
    colnames(m) <- c("var", "total", head(colnames(tab), -1), "")

    return(m)

  }



  if(has_x) {

    # find description types
    ctype <- sapply(x, function(z) class(z)[1L])
    # should we add "identical type": only one value??
    ctype[sapply(x, isDichotomous, strict=TRUE, na.rm=TRUE)] <- "dich"

    ctype[ctype %in% c("numeric", "integer")] <- "num"
    ctype[ctype %in% c("factor", "ordered", "character")] <- "cat"

    lst <- list()
    for(i in seq_len(ncol(x))){
      if(ctype[i] == "num"){
        lst[[i]] <- num_row(x[,i], groups, vname=vnames[i])

      } else if(ctype[i] == "cat") {
        lst[[i]] <- cat_mat(x[,i], groups, vname=vnames[i])

      } else if(ctype[i] == "dich") {

        if(intref=="both"){
          lst[[i]] <- cat_mat(factor(x[,i]), groups, vname=vnames[i])

        } else {

          # refactor all types, numeric, logic but not factors and let user choose
          # the level to be reported.
          if(!is.factor(x[, i])) {   # should only apply to boolean integer or numerics
            xi <- factor(x[, i])
          } else {
            xi <- x[, i]
          }

          if(intref == "high")
            xi <- relevel(xi, tail(levels(xi), 1))

          if (default_vnames) {
            lst[[i]] <- dich_mat(xi, groups,
                                 vname = gettextf("%s (= %s)", vnames[i],
                                                  head(levels(xi), 1)))
          } else {
            lst[[i]] <- dich_mat(xi, groups, vname = vnames[i])
          }
        }

      } else {
        # unsupported type: an empty row carrying only the variable name.
        # the width must match the other blocks: var + total + groups + test
        lst[[i]] <- rbind(c(vnames[i], rep(NA_character_, ngrp + 2L)))
      }
    }
  } else {
    # x = NA: insert a title row only
    m <- cat_mat(groups, groups, vname = vnames[1L])
    lst <- list(c(vnames[1L], rep("", ncol(m)-1)))
  }

  res <- do.call(rbind, lst)


  if(add.length)
    res <- rbind(c("n", c(fm(sum(!is.na(groups)), fmt=fmt$abs),
                          paste(fm(table(groups), fmt=fmt$abs), " (",
                                fm(prop.table(table(groups)), fmt=fmt$per), ")", sep=""), ""))
                 , res)

  # align the table
  if(align != "\\l")
    res[,-c(1, ncol(res))] <- strAlign(res[,-c(1, ncol(res))], sep = align)

  # drop = FALSE throughout: with a single dichotomous variable and
  # add.length = FALSE the table has one row, and res[, -3] would return a
  # plain vector - print.tOne() and `[.tOne` then fail on the missing dim
  if(no_groups){
    res <- res[, -3, drop=FALSE]
    total <- TRUE
  }

  if(!total)
    res <- res[, -2, drop=FALSE]

  if(notest)
    res <- res[, -ncol(res), drop=FALSE]

  # colnames() is masked by the argument of the same name, but as the latter
  # is not a function, the function is still found in the call below
  if(!is.null(colnames))
    colnames(res) <- rep(colnames, length.out=ncol(res))

  # attributes must be set AFTER the last subsetting, `[` would drop them
  if(!notest)
    attr(res, "legend") <- gettextf("%s) %s, %s) %s, %s) %s\nSignif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1",
                                    .FootNote(1), TEST[["num"]]$lbl, .FootNote(2), TEST[["dich"]]$lbl, .FootNote(3), TEST[["cat"]]$lbl)

  class(res) <- "tOne"
  return(res)
}



#' @rdname tOne
#' @export
print.tOne <- function(x, ...){

  cat("\n")

  if(.hasColor()){

    t1 <- as.data.frame.matrix(unclass(x))
    colnames(t1) <- colnames(x)

    out <- capture.output(print(t1, right=FALSE, print.gap=3, row.names=FALSE))
    cat(cli::style_bold(out[1]), "\n", sep="")

    # print the body without repeating the header
    cat(out[-1], sep="\n")

    if(!is.null(attr(x, "legend"))){
      cat(cli::col_silver("---\n"))
      cat(cli::col_silver(attr(x, "legend")), "\n", sep="")
    }
    cat("\n")


  } else {

    write.table(fm(rbind(colnames(x), unclass(x)), align = "\\l"),
                row.names=FALSE, col.names=FALSE, quote=FALSE)

    if(!is.null(attr(x, "legend"))){
      cat("---\n")
      cat(attr(x, "legend"), "\n")
    }
    cat("\n")

  }

  invisible(x)

}



# subsetting tOne

#' @param x a tOne object to subset
#' @param i rowindex
#' @param j columnindex
#' @param ... further parameters (not used here)
#' @param drop drop the structure in case of total reduction
#'
#' @rdname tOne
#' @export
`[.tOne` <- function(x, i, j, ..., drop=FALSE) {

  # subset main character matrix, don't drop structure by default
  res <- unclass(x)[i, j, drop=drop]

  # `[` does not keep the attributes, restore the relevant ones - but only
  # as long as the result is still a matrix
  if(!is.null(dim(res))){
    attr(res, "legend") <- attr(x, "legend")
    attr(res, "class") <- attr(x, "class")
  }

  return(res)

}



# == internal helper functions =============================================

.FootNote <- function(i){

  # internal function, not exported

  x <- .getOption("footnote")
  if(is.null(x))
    x <- c("'", '"', '""')
  return(x[i])
}

# see also
# \code{\link{WrdTable}()}, \code{\link{ToWrd.tOne}()},
