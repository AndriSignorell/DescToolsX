#' Descriptive statistics object
#'
#' Create descriptive summaries for different types of objects.
#'
#' \code{Desc()} is an S3 generic that computes basic descriptive statistics
#' depending on the class of its input. The result is an object of class
#' \code{"Desc"} with a more specific subclass such as
#' \code{"Desc.numeric"}, \code{"Desc.factor"} or \code{"Desc.data.frame"}.
#'
#' For numeric vectors, summary statistics such as mean and standard deviation
#' are computed. For factors, frequency tables are returned. For data frames,
#' \code{Desc()} is applied column-wise.
#'
#' @name Desc
#' @aliases Desc Desc.data.frame
#' @param x An object to be described.
#' @param ... Further arguments passed to methods.
#' 
#' @details
#' \code{Desc} is a \strong{generic function}. It dispatches to the
#' method of the class of its first argument. Typing
#' \code{?Desc} + TAB at the prompt should present a choice of links: the help
#' pages for each of these \code{Desc} methods. You don't need to 
#' use the full name of the method although you may if you wish; 
#' i.e., \code{Desc(x)} is idiomatic R but you can bypass method 
#' dispatch by going direct if you wish: \code{Desc.numeric(x)}.

#' The most frequent object types are supported, find more specific help in:
#' \verb{  }Numeric descriptions: \code{\link{Desc.numeric}}\cr
#' \verb{  }Numeric descriptions: \code{\link{Desc.factor}}\cr
#' \verb{  }Numeric descriptions: \code{\link{Desc.logical}}\cr
#' 
#' 
#' @return An object of class \code{"Desc"} with a subclass depending on
#'   the input type.
#'
#' @seealso \code{\link{summary}}, \code{\link{plot}}
#' @keywords descriptive statistics
NULL




#' @rdname Desc
#' @export
Desc <- function(x, ...) {
  UseMethod("Desc")
}


#' @rdname Desc
#' @export
Desc.data.frame <- function(x, ...) {
  
  res <- lapply(x, Desc)
  
  class(res) <- c("Desc.data.frame", "Desc")
  res
}


#' @rdname Desc
#' @export
descX <- Desc

#' @rdname Desc
#' @export
desc <- Desc
