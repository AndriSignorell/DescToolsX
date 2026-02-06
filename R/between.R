
#' Operators To Check, If a Value Lies Within Or Outside a Given Range
#' 
#' The between and outside operators are used to check, whether a vector of
#' given values x lie within a defined range (or outside respectively). The
#' values can be numbers, text or dates. Ordered factors are supported.
#' 
#' The "BETWEEN" operators basically combine two conditional statements into
#' one and simplify the query process.\cr They are merely a wrapper for:
#' \code{x >= rng[1] & x <= rng[2]}, where the round bracket \code{(} means
#' \emph{strictly greater (>)} and the square bracket \code{[} means
#' \emph{greater or equal (>=)}.  Numerical values of x will be handled by
#' C-code, which is significantly faster than two comparisons in R (especially
#' when x is huge). .\cr \code{%][%} is the negation of \code{%()%}, meaning
#' all values lying outside the given range. Elements on the limits will return
#' \code{TRUE}.
#' 
#' Both arguments, \code{x} and \code{rng}, will be recycled to the highest
#' dimension, which is either the length of the vector (\code{x}) or the number
#' of rows of the matrix (\code{rng}).\cr See also the routines used to check,
#' whether two ranges overlap (\code{\link[DescTools]{Overlap}}, 
#' \code{\link[DescTools]{Interval}}).
#' 
#' \code{%:%} returns all the elements of a vector between the (first found)
#' element \code{rng[1]} and \code{rng[2]}. If no match is found it returns
#' \code{NA}. If \code{rng[2]} occurs before \code{rng[1]} in the vector the
#' elements will be returned in reverse order (which is the same behaviour as
#' the \code{:} operator). \cr \code{%::%} does the same in greedy mood. It
#' uses the first match for \code{from} and the last match for \code{to}. \cr
#' 
#' @name between
#' @aliases between %()% %(]% %[)% %[]% %][% %)[% %](% %)(% %:% %::% 
#' @param x is a variable with at least ordinal scale, usually a numeric value,
#' but can be an ordered factor or a text as well. Texts would be treated
#' alphabetically.
#' @param rng a vector of two values or a matrix with 2 columns, defining the
#' minimum and maximum of the range for x. \cr If rng is a matrix, x or rng
#' will be recycled.
#' @return A logical vector of the same length as x.
#' @author Andri Signorell <andri@@signorell.net> based on C-code by Kevin
#' Ushey <kevinushey@@gmail.com>
#' @seealso \code{\link{if}}, \code{\link{ifelse}}, \code{\link{Comparison}},
#' \code{\link[DescTools]{Overlap}}, \code{\link[DescTools]{Interval}}
#' @keywords logic manip
#' @examples
#' 
#' x <- 1:9
#' x %[]% c(3,5)
#' 
#' # outside
#' x <- 1:9
#' x %][% c(3,5)
#' 
#' c(x,NA) %[]% c(3,5)
#' 
#' x %(]% c(3,5)
#' 
#' # no result when from > to:
#' x %[]% c(5,3)
#' x %(]% c(5,5)
#' 
#' # no problem:
#' ordered(x) %[]% c(3,5)
#' 
#' # not meaningful:
#' factor(x) %[]% c(3,5)
#' 
#' # characters
#' letters[letters %(]% c("d","h")]
#' 
#' data(d.pizza)
#' x <- levels(d.pizza$driver)
#' x %[]% c("C","G")
#' 
#' # select pizzas with a temperature between 50 and 55
#' d.pizza[na.omit(d.pizza$temperature %[]% c(63, 65)),]
#' 
#' # use it with an ordered factor and select all diamonds with
#' #   symmetry between G (included) and X (excluded).
#' # mean(d.diamonds[d.diamonds$symmetry %[)% c("G","X"),"price"])
#' 
#' 
#' # use multiple ranges
#' 2 %[]% cbind(1:4,2:5)
#' 
#' # both arguments are recycled
#' c(2,3) %[]% cbind(1:4,2:5)
#' 
#' 
#' # between operator for vector positions
#' set.seed(4)
#' (x <- sample(LETTERS, size=10, replace=TRUE))
#' # [1] "X" "K" "S" "C" "G" "L" "S" "V" "U" "Z"
#' 
#' # return all elements between "S" and "L" 
#' x %:% c("S","L")
#' # [1] "S" "C" "G" "L"
#'  
#' x %:% c("S","A")
#' # [1] "S" "C" "G" "L" "S" "V" "U" "Z"
#'  
#' x %:% c("A","S")
#' # [1] "X" "K" "S"
#' 
#' # reverted matches return the elements in reverse order
#' x %:% c("G","X")
#' # [1] "G" "C" "S" "K" "X"
#' 
#' # no match results in NA
#' x %:% c("Y","B")
#' 
#' (x <- c("B", "A", "X", "K", "S", "K", "G", "L", "K", "V", "K", "Z"))
#' # lazy
#' x %:% c("A", "K")
#' # greedy
#' x %::% c("A", "K")
#' 
 

#' @rdname between
#' @export
`%[]%` <- function(x, rng)
  .between_core(x, rng, TRUE, TRUE)

#' @rdname between
#' @export
`%[)%` <- function(x, rng)
  .between_core(x, rng, TRUE, FALSE)

#' @rdname between
#' @export
`%(]%` <- function(x, rng)
  .between_core(x, rng, FALSE, TRUE)

#' @rdname between
#' @export
`%()%` <- function(x, rng)
  .between_core(x, rng, FALSE, FALSE)



# outside operators (not exactly the negations)

#' @rdname between
#' @export
`%][%` <- function(x, rng) {
  return(!(x %()% rng))
}

#' @rdname between
#' @export
`%](%` <- function(x, rng) {
  return(!(x %(]% rng))
}

#' @rdname between
#' @export
`%)[%` <- function(x, rng) {
  return(!(x %[)% rng))
}

#' @rdname between
#' @export
`%)(%` <- function(x, rng) {
  return(!(x %[]% rng))
}



# lazy: takes the first matches
#' @rdname between
#' @export
`%:%` <- function(x, rng){
  i <- match(x, rng, nomatch = 0)
  from <- ifelse(length(from <- which(i==1))==0, 1, from)[1]
  to <- ifelse(length(to <- which(i==2))==0, length(x), to)[1]
  
  # why the NA here???  
  # if(from==1 & to==length(x))
  #   NA
  # else
  
  x[from:to]
  
}


# greedy: takes the first and the last
#' @rdname between
#' @export
`%::%` <- function(x, rng){
  i <- match(x, rng, nomatch = 0)
  from <- ifelse(length(from <- which(i==1))==0, 1, from)[1]
  to <- ifelse(length(to <- which(i==2))==0, length(x), tail(to, 1))[1]
  
  # why the NA here???  
  # if(from==1 & to==length(x))
  #   NA
  # else
  
  x[from:to]
  
}



# =====================================================================
# Between operators -- internal helper functions

.between_core <- function(x, rng,
                          left_closed = TRUE,
                          right_closed = TRUE) {
  
  ## --- Matrix bounds (vectorised) ----------------------------------
  if (is.matrix(rng)) {
    
    maxdim <- max(length(x), nrow(rng))
    x   <- rep(x,  length.out = maxdim)
    rng <- rng[rep(seq_len(nrow(rng)), length.out = maxdim), , drop = FALSE]
    
    res <- between_num(
      as.numeric(x),
      as.numeric(rng[, 1L]),
      as.numeric(rng[, 2L]),
      left_closed  = left_closed,
      right_closed = right_closed
    )
    
    res[is.na(x)] <- NA
    return(res)
  }
  
  ## --- Numeric / Date ----------------------------------------------
  if (is.numeric(x) || inherits(x, c("Date", "POSIXct", "POSIXlt")) ) {
    
    res <- between_num(
      as.numeric(x),
      as.numeric(rng[1L]),
      as.numeric(rng[2L]),
      left_closed  = left_closed,
      right_closed = right_closed
    )
    
    res[is.na(x)] <- NA
    return(res)
  }
  
  ## --- Ordered -----------------------------------------------------
  if (is.ordered(x)) {
    
    lo <- match(rng[1L], levels(x))
    hi <- match(rng[2L], levels(x))
    
    res <- between_num(
      as.numeric(x),
      lo, hi,
      left_closed  = left_closed,
      right_closed = right_closed
    )
    
    res[is.na(x)] <- NA
    return(res)
  }
  
  ## --- Character fallback ------------------------------------------
  if (is.character(x)) {
    
    if (left_closed && right_closed)
      return(x >= rng[1L] & x <= rng[2L])
    if (left_closed && !right_closed)
      return(x >= rng[1L] & x <  rng[2L])
    if (!left_closed && right_closed)
      return(x >  rng[1L] & x <= rng[2L])
    return(x > rng[1L] & x < rng[2L])
  }
  
  ## --- Default -----------------------------------------------------
  rep(NA, length(x))
}

