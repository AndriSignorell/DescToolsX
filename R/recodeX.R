
#' Recode a Variable
#' 
#' Combining or rearranging a factor can be tedious if it has many levels.
#' \code{recodeX()} supports this step by accepting a direct definition of new
#' levels by enumerating old levelnames as argument and adding an
#' \code{"elselevel"} option. If new levels are given as integer values they
#' will be translated in the according levels.
#' 
#' 
#' @param x the factor whose levels are to be altered.  If x is
#' \code{character} it will be factorized (using \code{factor} defaults) but
#' returned as \code{character} again.
#' @param \dots the old levels (combined by \code{c}() if there are several)
#' named with the new level:\cr \code{newlevel_a = c("old_a", "old_b"),
#' }\cr\code{newlevel_b = c("old_c", "old_d")}\cr See examples.
#' @param keep vector of levels that should be left untouched.
#' @param elselevel the value for levels, which are not matched by newlevel
#' list.  If this is set to \code{NULL}, the elselevels will be left unchanged.
#' If set to \code{NA} (default) non matched levels will be set to \code{NA}.
#' @param ref the reference level, typically a string.
#' @param use.empty logical. Defines how a new level, which can't be found in
#' x, should be handled.  Should it be left in the level's list or be dropped?
#' The default is \code{FALSE}, which drops empty levels.
#' @param num logical. If set to \code{TRUE} the result will be numeric. This
#' is useful if you want to recode strings to specific numeric values.
#' 
#' @return the factor having the new levels applied.\cr if \code{x} was a
#' \code{character} vector, the result will also be \code{character}
#' 
#' @author Andri Signorell <andri@@signorell.net>
#' @seealso \code{\link{factor}}, \code{\link{levels}}, \code{\link{relevel}},
#' \code{\link{reorder}}\cr There's another solution for this problem in the
#' package \bold{car}.
#' @keywords manip
#' @examples
#' 
#' set.seed(1984)
#' x <- factor(sample(1:15, 20, replace=TRUE))
#' levels(x) <- paste("old", levels(x), sep="_")
#' 
#' y <- recodeX(x,
#'             "new_1"   = c("old_1","old_4","old_5"),
#'             "new_2"   = c("old_6","old_10","old_11"),
#'             "new_3"   = c("old_12","old_13"),
#'             elselevel = "other")
#' data.frame(x=x, y=y)
#' 
#' # Coding NAs, NA is recoded to new_1
#' x[5:6] <- NA
#' x <- x[1:7] 
#' 
#' data.frame(
#'   x, 
#'   RecodeNA = recodeX(x,
#'                     "new_1"   = c("old_4","old_8", NA),
#'                     elselevel = "other"),
#'        
#'   # NAs remain unaffected, unless specified to be processed      
#'   NoRecodeNA = recodeX(x,
#'                       "new_1"   = c("old_4","old_8"),
#'                       elselevel = "other")
#' )         
#' 
#' # keep some levels, collapse others and reset the reference level
#' ff <- factor(c("apple","pear","banana","kiwi",
#'                "mango","peach","grape","plum"))
#' recodeX(ff, 
#'        stone=c("peach", "plum"), 
#'        keep=c("apple","banana"),
#'        elselevel = "other", ref="stone")
#' 
#' 
#' x <- factor(letters[1:6])
#' 
#' z1 <- recodeX(x, AB=c("a","b"), CD=c("c","d"), elselevel="none of these")
#' z2 <- recodeX(x, AB=c("a","b"), CD=c("c","d"), elselevel=NA)
#' z3 <- recodeX(x, AB=c("a","b"), CD=c("c","d"), elselevel=NULL)
#' z4 <- recodeX(x, AB=c("a","b"), GH=c("g","h"), elselevel=NA, use.empty=TRUE)
#' z5 <- recodeX(x, AB=c("a","b"), GH=c("g","h"), elselevel=NA, use.empty=FALSE)
#' 
#' data.frame(z1, z2, z3, z4, z5)
#' 
#' lapply(data.frame(z1, z2, z3, z4, z5), levels)
#' 
#' # empty level GH exists in z4...
#' table(z4, useNA="ifany")
#' # and is dropped in z5
#' table(z5, useNA="ifany")
#' 
#' # use integers to define the groups to collapse
#' set.seed(1972)
#' (likert <- factor(sample(1:10, size=15, replace=TRUE),
#'                   levels=1:10, labels=gettextf("(%s)", 1:10)))
#' recodeX(likert, det=1:6, pas=7:8, pro=9:10)
#' 
#' # or directly turned to numeric
#' recodeX(likert, "1"=1:6, "2"=7:8, "5"=9:10, num=TRUE)
#' 

#' @export
recodeX <- function(x, ..., keep=NULL, elselevel=NA, ref= NULL, 
                   use.empty=FALSE, num=FALSE){
  
  # if x is character, turn it to factor and reconvert it when finished
  if(xchar <- is.character(x)){
    x <- factor(x)
  }
  
  # newlevels <- list(...)
  newlevels <- c(setNamesX(keep, names=keep), list(...))
  
  if( sum(duplicated(unlist(newlevels))) > 0) stop ("newlevels contain non unique values!")
  
  # convert numeric values to according levels if all arguments are passed as numerics
  if(all(is.numeric(unlist(newlevels))))
    newlevels <- lapply(newlevels, function(i) levels(x)[i])
  
  if(is.null(elselevel)) { # leave elselevels as they are
    elselevels <- setdiff(levels(x), unlist(newlevels))
    names(elselevels) <- elselevels
    newlevels <- c(newlevels, elselevels)
    
  } else {
    if(!is.na(elselevel)){
      newlevels[[length(newlevels)+1]] <- setdiff(levels(x), unlist(newlevels))
      names(newlevels)[[length(newlevels)]] <- elselevel
    }
  }
  levels(x) <- newlevels
  if(!use.empty) x <- factor(x)  # delete potentially empty levels
  
  # handle NA levels
  if(any(i <- sapply(lapply(newlevels, is.na), any)))
    x[is.na(x)] <- names(newlevels)[i]
  
  if(!is.null(ref))
    x <- relevel(x, ref=ref)
  
  # x was character, convert to original then
  if(xchar)
    x <- as.character(x)
  
  if(num)
    x <- as.numeric(as.character(x))
  
  return(x)
  
}
