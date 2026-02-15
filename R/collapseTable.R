
#' Collapse Levels of a Table
#' 
#' Collapse (or re-label) variables in a a contingency table or \code{ftable}
#' object by re-assigning levels of the table variables.
#' 
#' Each of the \code{\dots} arguments must be of the form \code{variable =
#' levels}, where \code{variable} is the name of one of the table dimensions,
#' and \code{levels} is a character or numeric vector of length equal to the
#' corresponding dimension of the table. Missing argument names are allowed and
#' will be interpreted in the order of the dimensions of the table.
#' 
#' @param x A \code{table} or \code{ftable} object
#' @param \dots A collection of one or more assignments of factors of the table
#' to a list of levels
#' @return A \code{table} object (even if the input was an ftable),
#' representing the original table with one or more of its factors collapsed or
#' rearranged into other levels.
#' @author Michael Friendly <friendly@@yorku.ca>, Andri Signorell
#' <andri@@signorell.net>
#' @seealso \code{\link{untable}}
#' 
#' \code{\link[base]{margin.table}} "collapses" a table in a different way, by
#' summing over table dimensions.
#' @keywords manip attribute
#' @examples
#' 
#' # create some sample data in table form
#' sex <- c("Male", "Female")
#' age <- letters[1:6]
#' education <- c("low", 'med', 'high')
#' data <- expand.grid(sex=sex, age=age, education=education)
#' counts <- rpois(36, 100)
#' data <- cbind(data, counts)
#' t1 <- xtabs(counts ~ sex + age + education, data=data)
#' 
#' Desc(t1)
#' 
#' ##                  age   a   b   c   d   e   f
#' ## sex    education
#' ## Male   low           119 101 109  85  99  93
#' ##        med            94  98 103 108  84  84
#' ##        high           81  88  96 110 100  92
#' ## Female low           107 104  95  86 103  96
#' ##        med           104  98  94  95 110 106
#' ##        high           93  85  90 109  99  86
#' 
#' 
#' # collapse age to 3 levels
#' t2 <- collapseTable(t1, age=c("A", "A", "B", "B", "C", "C"))
#' Desc(t2)
#' 
#' ##                  age   A   B   C
#' ## sex    education
#' ## Male   low           220 194 192
#' ##        med           192 211 168
#' ##        high          169 206 192
#' ## Female low           211 181 199
#' ##        med           202 189 216
#' ##        high          178 199 185
#' 
#' 
#' # collapse age to 3 levels and pool education: "low" and "med" to "low"
#' t3 <- collapseTable(t1, age=c("A", "A", "B", "B", "C", "C"),
#'     education=c("low", "low", "high"))
#' Desc(t3)
#' 
#' ##                  age   A   B   C
#' ## sex    education
#' ## Male   low           412 405 360
#' ##        high          169 206 192
#' ## Female low           413 370 415
#' ##        high          178 199 185
#' 
#' 
#' 
#' # change labels for levels of education to 1:3
#' t4 <- collapseTable(t1,  education=1:3)
#' Desc(t4)
#' 
#' ##                  age   a   b   c   d   e   f
#' ## sex    education
#' ## Male   1             119 101 109  85  99  93
#' ##        2              94  98 103 108  84  84
#' ##        3              81  88  96 110 100  92
#' ## Female 1             107 104  95  86 103  96
#' ##        2             104  98  94  95 110 106
#' ##        3              93  85  90 109  99  86
#' 
#' 



#' @export
collapseTable <- function (x, ...) {
  
  # allow unnamed arguments, changed by 0.99.27
  nargs <- length(args <- list(...))
  
  if (!nargs)
    return(x)
  
  # provide variable names for the table, maximum possible names = max_dim(x))
  iArgs <- seq_len(length(dimnames(x)))
  nmc <- paste0("Var", iArgs)
  
  nm <- names(dimnames(x))
  if (any(ng0 <- !nzchar(nm)))
    names(dimnames(x))[ng0] <- nmc[seq(sum(ng0))]
  
  # provide variable names for unnamed arguments
  nm <- names(args)
  if (is.null(nm))
    names(args) <- names(dimnames(x))[seq(nargs)]
  else if (any(ng0 <- !nzchar(nm)))
    names(args)[ng0] <- names(dimnames(x))[names(dimnames(x)) %nin% nm[nzchar(nm)]][seq(sum(ng0))]
  
  if (inherits(x, "ftable"))
    x <- as.table(x)
  
  if (inherits(x, "table")) {
    tvars <- names(dimnames(x))
    x <- as.data.frame.table(x)
    freq <- x[, "Freq"]
    
  } else {
    stop("Argument must be a table or ftable object")
  }
  
  names <- names(args)
  for (i in 1:nargs) {
    vals <- args[[i]]
    nm <- names[[i]]
    if (any(nm == tvars))
      levels(x[[nm]]) <- vals
    else
      warning(nm, " is not among the x variables.")
  }
  
  res <- xtabs(as.formula(paste("freq ~", paste(tvars, collapse = "+"))),
               data = x)
  
  class(res) <- class(res)[class(res) != "xtabs"]
  attr(res, "call") <- NULL
  
  names(dimnames(res))[names(dimnames(res)) %in% nmc] <- ""
  
  return(res)
  
}


