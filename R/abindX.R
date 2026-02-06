
#' Combine Multidimensional Arrays
#' 
#' Base R functions \code{cbind} and \code{rbind} bind columns and rows, but
#' there's no built-in function for binding higher dimensional datastructures
#' like matrices. \code{abindX} takes a sequence of vectors, matrices, or arrays
#' and produces a single array of the same or higher dimension.
#' 
#' The dimensions of the supplied vectors or arrays do not need to be
#' identical, e.g., arguments can be a mixture of vectors and matrices.
#' \code{abindX} coerces arguments by the addition of one dimension in order to
#' make them consistent with other arguments and \code{along=}.  The extra
#' dimension is added in the place specified by \code{along=}.
#' 
#' The default action of abindX is to concatenate on the last dimension, rather
#' than increase the number of dimensions. For example, the result of calling
#' abindX with vectors is a longer vector (see first example below).  This
#' differs from the action of \code{rbind} and cbind which is to return a
#' matrix when called with vectors.  abindX can be made to behave like cbind on
#' vectors by specifying \code{along=2}, and like rbind by specifying
#' \code{along=0}.
#' 
#' The dimnames of the returned object are pieced together from the dimnames of
#' the arguments, and the names of the arguments.  Names for each dimension are
#' searched for in the following order: new.names, argument name, dimnames (or
#' names) attribute of last argument, dimnames (or names) attribute of second
#' last argument, etc.  (Supplying the argument \code{use.first.dimnames=TRUE}
#' changes this to cause \code{abindX} to use dimnames or names from the first
#' argument first.  The default behavior is the same as for \code{rbind} and
#' \code{cbind}: use dimnames from later arguments.)  If some names are
#' supplied for the along dimension (either as argument names or dimnames in
#' arguments), names are constructed for anonymous arguments unless
#' \code{maken.ames=FALSE}.
#' 
#' @param \dots Any number of vectors, matrices, arrays, or data frames. The
#' dimensions of all the arrays must match, except on one dimension (specified
#' by \code{along=}).  If these arguments are named, the name will be used for
#' the name of the dimension along which the arrays are joined.  Vectors are
#' treated as having a dim attribute of length one.
#' 
#' Alternatively, there can be one (and only one) list argument supplied, whose
#' components are the objects to be bound together.  Names of the list
#' components are treated in the same way as argument names.
#' @param along The dimension along which to bind the arrays. The default is
#' the last dimension, i.e., the maximum length of the dim attribute of the
#' supplied arrays.  \code{along=} can take any non-negative value up to the
#' minimum length of the dim attribute of supplied arrays plus one.  When
#' \code{along=} has a fractional value, a value less than 1, or a value
#' greater than N (N is the maximum of the lengths of the dim attribute of the
#' objects to be bound together), a new dimension is created in the result.  In
#' these cases, the dimensions of all arguments must be identical.
#' @param rev.along Alternate way to specify the dimension along which to bind
#' the arrays: \code{along = N + 1 - rev.along}.  This is provided mainly to
#' allow easy specification of \code{along = N + 1} (by supplying
#' \code{rev.along=0}).  If both \code{along} and \code{rev.along} are
#' supplied, the supplied value of \code{along} is ignored.
#' @param new.names If new.names is a list, it is the first choice for the
#' dimnames attribute of the result.  It should have the same structure as a
#' dimnames attribute.  If the names for a particular dimension are
#' \code{NULL}, names for this dimension are constructed in other ways.
#' 
#' If \code{new.names} is a character vector, it is used for dimension names in
#' the same way as argument names are used.  Zero length ("") names are
#' ignored.
#' @param force.array If \code{FALSE}, rbind or cbind are called when possible,
#' i.e., when the arguments are all vectors, and along is not 1, or when the
#' arguments are vectors or matrices or data frames and along is 1 or 2.  If
#' rbind or cbind are used, they will preserve the data.frame classes (or any
#' other class that r/cbind preserve).  Otherwise, abindX will convert objects
#' to class array.  Thus, to guarantee that an array object is returned, supply
#' the argument \code{force.array=TRUE}.  Note that the use of rbind or cbind
#' introduces some subtle changes in the way default dimension names are
#' constructed: see the examples below.
#' @param make.names If \code{TRUE}, the last resort for dimnames for the along
#' dimension will be the deparsed versions of anonymous arguments.  This can
#' result in cumbersome names when arguments are expressions. The default is
#' \code{FALSE}.
#' @param use.first.dimnames When dimension names are present on more than one
#' argument, should dimension names for the result be take from the first
#' available (the default is to take them from the last available, which is the
#' same behavior as \code{rbind} and \code{cbind}.)
#' @param hier.names If \code{TRUE}, dimension names on the concatenated
#' dimension will be composed of the argument name and the dimension names of
#' the objects being bound.  If a single list argument is supplied, then the
#' names of the components serve as the argument names.  \code{hier.names} can
#' also have values \code{"before"} or \code{"after"}; these determine the
#' order in which the argument name and the dimension name are put together
#' (\code{TRUE} has the same effect as \code{"before"}).
#' @param use.dnns (default \code{FALSE}) Use names on dimensions, e.g., so
#' that \code{names(dimnames(x))} is non-empty.  When there are multiple
#' possible sources for names of dimnames, the value of
#' \code{use.first.dimnames} determines the result.
#' @return
#' 
#' An array with a dim attribute calculated as follows.
#' 
#' Let \code{rMin=min(sapply(list(...), function(x) length(dim(x))))} and \cr
#' \code{rMax=max(sapply(list(...), function(x) length(dim(x))))} (where the
#' length of the dimensions of a vector are taken to be 1).  Then \code{rMax}
#' should be equal to or one greater than \code{rMin}.
#' 
#' If \code{along} refers to an existing dimension, then the length of the dim
#' attribute of the result is \code{rMax}.  If \code{along} does not refer to
#' an existing dimension, then \code{rMax} should equal \code{rMin} and the
#' length of the dim attribute of the result will be \code{rMax+1}.
#' 
#' \code{rbind} or \code{cbind} are called to compute the result if (a)
#' \code{force.array=FALSE}; and (b) the result will be a two-dimensional
#' object.
#' @note It would be nice to make \code{abindX()} an S3 generic, but S3 generics
#' cannot dispatch off anonymous arguments.
#' 
#' The ability of \code{abindX()} to accept a single list argument removes much
#' of the need for constructs like \code{do.call("abindX", list.of.arrays)}.
#' Instead, just do \code{abindX(list.of.arrays)}.  The direct construct is
#' preferred because \code{do.call()} construct can sometimes consume more
#' memory during evaluation.
#' @author Tony Plate <tplate@@acm.org> and Richard Heiberger
#' @seealso \code{\link{rbind}}, \code{\link{cbind}}, \code{\link{array}}
#' @keywords manip array
#' @examples
#' 
#' # Five different ways of binding together two matrices
#' x <- matrix(1:12, 3, 4)
#' y <- x + 100
#' dim(abindX(x, y, along=0))     # binds on new dimension before first
#' dim(abindX(x, y, along=1))     # binds on first dimension
#' dim(abindX(x, y, along=1.5))
#' dim(abindX(x, y, along=2))
#' dim(abindX(x, y, along=3))
#' dim(abindX(x, y, rev.along=1)) # binds on last dimension
#' dim(abindX(x, y, rev.along=0)) # binds on new dimension after last
#' 
#' # Unlike cbind or rbind in that the default is to bind
#' # along the last dimension of the inputs, which for vectors
#' # means the result is a vector (because a vector is
#' # treated as an array with length(dim(x))==1).
#' abindX(x=1:4, y=5:8)
#' 
#' # Like cbind
#' abindX(x=1:4, y=5:8, along=2)
#' abindX(x=1:4, matrix(5:20, nrow=4), along=2)
#' abindX(1:4, matrix(5:20, nrow=4), along=2)
#' 
#' # Like rbind
#' abindX(x=1:4, matrix(5:20, nrow=4), along=1)
#' abindX(1:4, matrix(5:20, nrow=4), along=1)
#' 
#' # Create a 3-d array out of two matrices
#' abindX(x=matrix(1:16, nrow=4), y=matrix(17:32, nrow=4), along=3)
#' 
#' # Use of hier.names
#' abindX(x=cbind(a=1:3, b=4:6), y=cbind(a=7:9, b=10:12), hier.names=TRUE)
#' 
#' # Use a list argument
#' abindX(list(x=x, y=x), along=3)
#' # Use lapply(..., get) to get the objects
#' an <- c('x', 'y')
#' names(an) <- an
#' abindX(lapply(an, get), along=3)
#' 


#' @export
abindX <- function(..., along=N, rev.along=NULL, new.names=NULL,
                  force.array=TRUE, make.names=FALSE,
                  use.first.dimnames=FALSE, hier.names=FALSE, use.dnns=FALSE) {
  
  if (is.character(hier.names))
    hier.names <- match.arg(hier.names, c('before', 'after', 'none'))
  else
    hier.names <- if (hier.names) 'before' else 'no'
  arg.list <- list(...)
  if (is.list(arg.list[[1]]) && !is.data.frame(arg.list[[1]])) {
    if (length(arg.list)!=1)
      stop("can only supply one list-valued argument for ...")
    if (make.names)
      stop("cannot have make.names=TRUE with a list argument")
    arg.list <- arg.list[[1]]
    have.list.arg <- TRUE
  } else {
    N <- max(1, sapply(list(...), function(x) length(dim(x))))
    have.list.arg <- FALSE
  }
  if (any(discard <- sapply(arg.list, is.null)))
    arg.list <- arg.list[!discard]
  if (length(arg.list)==0)
    return(NULL)
  N <- max(1, sapply(arg.list, function(x) length(dim(x))))
  
  ## N will eventually be length(dim(return.value))
  if (!is.null(rev.along))
    along <- N + 1 - rev.along
  
  if (along < 1 || along > N || (along > floor(along) && along < ceiling(along))) {
    N <- N + 1
    along <- max(1, min(N+1, ceiling(along)))
  }
  
  ## this next check should be redundant, but keep it here for safety...
  if (length(along) > 1 || along < 1 || along > N + 1)
    stop(paste("\"along\" must specify one dimension of the array,",
               "or interpolate between two dimensions of the array",
               sep="\n"))
  
  if (!force.array && N==2) {
    if (!have.list.arg) {
      if (along==2)
        return(cbind(...))
      if (along==1)
        return(rbind(...))
    } else {
      if (along==2)
        return(do.call("cbind", arg.list))
      if (along==1)
        return(do.call("rbind", arg.list))
    }
  }
  
  if (along>N || along<0)
    stop("along must be between 0 and ", N)
  
  pre <- seq(from=1, len=along-1)
  post <- seq(to=N-1, len=N-along)
  ## "perm" specifies permutation to put join dimension (along) last
  perm <- c(seq(len=N)[-along], along)
  
  arg.names <- names(arg.list)
  if (is.null(arg.names)) arg.names <- rep("", length(arg.list))
  ## if new.names is a character vector, treat it as argument names
  if (is.character(new.names)) {
    arg.names[seq(along=new.names)[nchar(new.names)>0]] <-
      new.names[nchar(new.names)>0]
    new.names <- NULL
  }
  
  ## Be careful with dot.args, because if abindX was called
  ## using do.call(), and had anonymous arguments, the expressions
  ## returned by match.call() are for the entire structure.
  ## This can be a problem in S-PLUS, not sure about R.
  ## E.g., in this one match.call() returns compact results:
  ## > (function(...)browser())(1:10,letters)
  ## Called from: (function(...)  browser())....
  ## b()> match.call(expand.dots=FALSE)$...
  ## list(1:10, letters)
  ## But in this one, match.call() returns evaluated results:
  ## > test <- function(...) browser()
  ## > do.call("test", list(1:3,letters[1:4]))
  ## Called from: test(c(1, 2, 3), c("a", "b....
  ## b(test)> match.call(expand.dots=FALSE)$...
  ## list(c(1, 2, 3), c("a", "b", "c", "d")
  ## The problem here was largely mitigated by making abindX()
  ## accept a single list argument, which removes most of the
  ## need for the use of do.call("abindX", ...)
  
  ## Create deparsed versions of actual arguments in arg.alt.names
  ## These are used for error messages
  if (any(arg.names=="")) {
    if (make.names) {
      ## Create dot.args to be a list of calling expressions for the objects to be bound.
      ## Be careful here with translation to R --
      ## dot.args does not have the "list" functor with R
      ## (and dot.args is not a call object), whereas with S-PLUS, dot.args
      ## must have the list functor removed
      dot.args <- match.call(expand.dots=FALSE)$... ## [[2]]
      if (is.call(dot.args) && identical(dot.args[[1]], as.name("list")))
        dot.args <- dot.args[-1]
      arg.alt.names <- arg.names
      for (i in seq(along=arg.names)) {
        if (arg.alt.names[i]=="") {
          if (object.size(dot.args[[i]])<1000) {
            arg.alt.names[i] <- paste(deparse(dot.args[[i]], 40), collapse=";")
          } else {
            arg.alt.names[i] <- paste("X", i, sep="")
          }
          arg.names[i] <- arg.alt.names[i]
        }
      }
      ## unset(dot.args) don't need dot.args any more, but R doesn't have unset()
    } else {
      arg.alt.names <- arg.names
      arg.alt.names[arg.names==""] <- paste("X", seq(along=arg.names), sep="")[arg.names==""]
    }
  } else {
    arg.alt.names <- arg.names
  }
  
  use.along.names <- any(arg.names!="")
  
  ## need to have here: arg.names, arg.alt.names, don't need dot.args
  
  names(arg.list) <- arg.names
  ## arg.dimnames is a matrix of dimension names, each element of the
  ## the matrix is a character vector, e.g., arg.dimnames[j,i] is
  ## the vector of names for dimension j of arg i
  arg.dimnames <- matrix(vector("list", N*length(arg.names)), nrow=N, ncol=length(arg.names))
  dimnames(arg.dimnames) <- list(NULL, arg.names)
  ## arg.dnns is a matrix of names of dimensions, each element is a
  ## character vector len 1, or NULL
  arg.dnns <- matrix(vector("list", N*length(arg.names)), nrow=N, ncol=length(arg.names))
  dimnames(arg.dnns) <- list(NULL, arg.names)
  dimnames.new <- vector("list", N)
  
  ## Coerce all arguments to have the same number of dimensions
  ## (by adding one, if necessary) and permute them to put the
  ## join dimension last.
  
  ## Create arg.dim as a matrix with length(dim) rows and
  ## length(arg.list) columns: arg.dim[j,i]==dim(arg.list[[i]])[j],
  ## The dimension order of arg.dim is original
  arg.dim <- matrix(integer(1), nrow=N, ncol=length(arg.names))
  
  for (i in seq(len=length(arg.list))) {
    m <- arg.list[[i]]
    m.changed <- FALSE
    
    ## be careful with conversion to array: as.array converts data frames badly
    if (is.data.frame(m)) {
      ## use as.matrix() in preference to data.matrix() because
      ## data.matrix() uses the unintuitive codes() function on factors
      m <- as.matrix(m)
      m.changed <- TRUE
    } else if (!is.array(m) && !is.null(m)) {
      if (!is.atomic(m))
        stop("arg '", arg.alt.names[i], "' is non-atomic")
      ## make sure to get the names of a vector and attach them to the array
      dn <- names(m)
      m <- as.array(m)
      if (length(dim(m))==1 && !is.null(dn))
        dimnames(m) <- list(dn)
      m.changed <- TRUE
    }
    new.dim <- dim(m)
    if (length(new.dim)==N) {
      ## Assign the dimnames of this argument to the i'th column of arg.dimnames.
      ## If dimnames(m) is NULL, would need to do arg.dimnames[,i] <- list(NULL)
      ## to set all elts to NULL, as arg.dimnames[,i] <- NULL does not actually
      ## change anything in S-PLUS (leaves whatever is there) and illegal in R.
      ## Since arg.dimnames has NULL entries to begin with, don't need to do
      ## anything when dimnames(m) is NULL
      if (!is.null(dimnames(m))) {
        arg.dimnames[,i] <- dimnames(m)
        if (use.dnns && !is.null(names(dimnames(m))))
          arg.dnns[,i] <- as.list(names(dimnames(m)))
      }
      arg.dim[,i] <- new.dim
    } else if (length(new.dim)==N-1) {
      ## add another dimension (first set dimnames to NULL to prevent errors)
      if (!is.null(dimnames(m))) {
        ## arg.dimnames[,i] <- c(dimnames(m)[pre], list(NULL), dimnames(m))[post]
        ## is equivalent to arg.dimnames[-N,i] <- dimnames(m)
        arg.dimnames[-along,i] <- dimnames(m)
        if (use.dnns && !is.null(names(dimnames(m))))
          arg.dnns[-along,i] <- as.list(names(dimnames(m)))
        ## remove the dimnames so that we can assign a dim of an extra length
        dimnames(m) <- NULL
      }
      arg.dim[,i] <- c(new.dim[pre], 1, new.dim[post])
      if (any(perm!=seq(along=perm))) {
        dim(m) <- c(new.dim[pre], 1, new.dim[post])
        m.changed <- TRUE
      }
    } else {
      stop("'", arg.alt.names[i], "' does not fit: should have `length(dim())'=",
           N, " or ", N-1)
    }
    
    if (any(perm!=seq(along=perm)))
      arg.list[[i]] <- aperm(m, perm)
    else if (m.changed)
      arg.list[[i]] <- m
  }
  
  ## Make sure all arguments conform
  conform.dim <- arg.dim[,1]
  for (i in seq(len=ncol(arg.dim))) {
    if (any((conform.dim!=arg.dim[,i])[-along])) {
      stop("arg '", arg.alt.names[i], "' has dims=", paste(arg.dim[,i], collapse=", "),
           "; but need dims=", paste(replace(conform.dim, along, "X"), collapse=", "))
    }
  }
  
  ## find the last (or first) names for each dimensions except the join dimension
  if (N>1)
    for (dd in seq(len=N)[-along]) {
      for (i in (if (use.first.dimnames) seq(along=arg.names) else rev(seq(along=arg.names)))) {
        if (length(arg.dimnames[[dd,i]]) > 0) {
          dimnames.new[[dd]] <- arg.dimnames[[dd,i]]
          if (use.dnns && !is.null(arg.dnns[[dd,i]]))
            names(dimnames.new)[dd] <- arg.dnns[[dd,i]]
          break
        }
      }
    }
  
  ## find or create names for the join dimension
  for (i in seq(len=length(arg.names))) {
    ## only use names if arg i contributes some elements
    if (arg.dim[along,i] > 0) {
      dnm.along <- arg.dimnames[[along,i]]
      if (length(dnm.along)==arg.dim[along,i]) {
        use.along.names <- TRUE
        if (hier.names=='before' && arg.names[i]!="")
          dnm.along <- paste(arg.names[i], dnm.along, sep=".")
        else if (hier.names=='after' && arg.names[i]!="")
          dnm.along <- paste(dnm.along, arg.names[i], sep=".")
      } else {
        ## make up names for the along dimension
        if (arg.dim[along,i]==1)
          dnm.along <- arg.names[i]
        else if (arg.names[i]=="")
          dnm.along <- rep("", arg.dim[along,i])
        else
          dnm.along <- paste(arg.names[i], seq(length=arg.dim[along,i]), sep="")
      }
      dimnames.new[[along]] <- c(dimnames.new[[along]], dnm.along)
    }
    if (use.dnns) {
      dnn <- unlist(arg.dnns[along,])
      if (length(dnn)) {
        if (!use.first.dimnames)
          dnn <- rev(dnn)
        names(dimnames.new)[along] <- dnn[1]
      }
    }
  }
  ## if no names at all were given for the along dimension, use none
  if (!use.along.names)
    dimnames.new[along] <- list(NULL)
  
  ## Construct the output array from the pieces.
  ## Could experiment here with more efficient ways of constructing the
  ## result than using unlist(), e.g.
  ##    out <- numeric(prod(c( arg.dim[-along,1], sum(arg.dim[along,]))))
  ## Don't use names in unlist because this can quickly exhaust memory when
  ## abindX is called with "do.call" (which creates horrendous names in S-PLUS).
  out <- array(unlist(arg.list, use.names=FALSE),
               dim=c( arg.dim[-along,1], sum(arg.dim[along,])),
               dimnames=dimnames.new[perm])
  ## permute the output array to put the join dimension back in the right place
  if (any(order(perm)!=seq(along=perm)))
    out <- aperm(out, order(perm))
  
  ## if new.names is list of character vectors, use whichever are non-null
  ## for dimension names, checking that they are the right length
  if (!is.null(new.names) && is.list(new.names)) {
    for (dd in seq(len=N)) {
      if (!is.null(new.names[[dd]])) {
        if (length(new.names[[dd]])==dim(out)[dd])
          dimnames(out)[[dd]] <- new.names[[dd]]
        else if (length(new.names[[dd]]))
          warning(paste("Component ", dd,
                        " of new.names ignored: has length ",
                        length(new.names[[dd]]), ", should be ",
                        dim(out)[dd], sep=""))
      }
      if (use.dnns && !is.null(names(new.names)) && names(new.names)[dd]!='')
        names(dimnames(out))[dd] <- names(new.names)[dd]
    }
  }
  if (use.dnns && !is.null(names(dimnames(out))) && any(i <- is.na(names(dimnames(out)))))
    names(dimnames(out))[i] <- ''
  out
}


