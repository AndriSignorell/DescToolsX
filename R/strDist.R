
#' Compute Distances Between Strings
#' 
#' \code{strDist} computes distances between strings following to Levenshtein
#' or Hamming method.
#' 
#' The function computes the Hamming and the Levenshtein (edit) distance of two
#' given strings (sequences). The Hamming distance between two vectors is the
#' number mismatches between corresponding entries.
#' 
#' In case of the Hamming distance the two strings must have the same length.
#' 
#' In case of the Levenshtein (edit) distance a scoring and a trace-back matrix
#' are computed and are saved as attributes \code{"ScoringMatrix"} and
#' \code{"TraceBackMatrix"}.  The numbers in the trace-back matrix reflect
#' insertion of a gap in string \code{y} (1), match/missmatch (2), and
#' insertion of a gap in string \code{x} (3).
#' 
#' The edit distance is useful, but normalizing the distance to fall within the
#' interval \verb{[0,1]} is preferred because it is somewhat diffcult to judge whether
#' an LD of for example 4 suggests a high or low degree of similarity.  The
#' method \code{"normlevenshtein"} for normalizing the LD is sensitive to this
#' scenario. In this implementation, the Levenshtein distance is transformed to
#' fall in this interval as follows: \deqn{lnd = 1 - \frac{ld}{max(length(x),
#' length(y))}}{lnd = 1 - ld / max(length(x), length(y))}
#' 
#' where \code{ld} is the edit distance and \code{max(length(x), length(y))}
#' denotes that we divide by the length of the larger of the two character
#' strings. This normalization, referred to as the Levenshtein normalized
#' distance (lnd), yields a statistic where 1 indicates perfect agreement
#' between the two strings, and a 0 denotes imperfect agreement. The closer a
#' value is to 1, the more certain we can be that the character strings are the
#' same; the closer to 0, the less certain.
#' 
#' @param x character vector, first string.
#' @param y character vector, second string.
#' @param method character, name of the distance method. This must be
#' \code{"levenshtein"}, \code{"normlevenshtein"} or \code{"hamming"}. Default
#' is \code{"levenshtein"}, the classical Levenshtein distance.
#' @param mismatch numeric, distance value for a mismatch between symbols.
#' @param gap numeric, distance value for inserting a gap.
#' @param ignore.case if \code{FALSE} (default), the distance measure will be
#' case sensitive and if \code{TRUE}, case is ignored.
#' @return \code{strDist} returns an object of class \code{"dist"}; cf.
#' \code{\link[stats]{dist}}.
#' @note For distances between strings and for string alignments see also
#' Bioconductor package \pkg{Biostrings}
#' @author Matthias Kohl <Matthias.Kohl@@stamats.de>
#' @seealso \code{\link[utils]{adist}}, \code{\link[stats]{dist}}
#' @references R. Merkl and S. Waack (2009) \emph{Bioinformatik Interaktiv}.
#' Wiley.
#' 
#' Harold C. Doran (2010) \emph{MiscPsycho. An R Package for Miscellaneous
#' Psychometric Analyses}
#' @keywords math character utilities
#' @examples
#' 
#' x <- "GACGGATTATG"
#' y <- "GATCGGAATAG"
#' ## Levenshtein distance
#' d <- strDist(x, y)
#' d
#' attr(d, "ScoringMatrix")
#' attr(d, "TraceBackMatrix")
#' 
#' ## Hamming distance
#' strDist(x, y, method="hamming")
#' 



#' @export
strDist <- function (x, y, method = "levenshtein", mismatch = 1, gap = 1, ignore.case = FALSE){
  
  # source MKmisc, Author: Matthias Kohl
  
  if(ignore.case){
    x <- tolower(x)
    y <- tolower(y)
  }
  
  if (!is.na(pmatch(method, "levenshtein")))
    method <- "levenshtein"
  
  METHODS <- c("levenshtein", "normlevenshtein", "hamming")
  method <- pmatch(method, METHODS)
  
  if (is.na(method))
    stop("invalid distance method")
  
  if (method == -1)
    stop("ambiguous distance method")
  
  stopifnot(is.character(x), is.character(y))
  
  if (length(x) == 1 & nchar(x[1]) > 1)
    x1 <- strsplit(x, split = "")[[1]]
  else
    x1 <- x
  
  if (length(y) == 1 & nchar(y[1]) > 1)
    y1 <- strsplit(y, split = "")[[1]]
  else
    y1 <- y
  
  if (method %in% c(1,2)){ ## Levenshtein
    m <- length(x1)
    n <- length(y1)
    D <- matrix(NA, nrow = m+1, ncol = n+1)
    M <- matrix("", nrow = m+1, ncol = n+1)
    D[,1] <- seq_len(m+1)*gap-1
    D[1,] <- seq_len(n+1)*gap-1
    D[1,1] <- 0
    M[,1] <- "d"
    M[1,] <- "i"
    M[1,1] <- "start"
    text <- c("d", "m", "i")
    for(i in c(2:(m+1))){
      for(j in c(2:(n+1))){
        m1 <- D[i-1,j] + gap
        m2 <- D[i-1,j-1] + (x1[i-1] != y1[j-1])*mismatch
        m3 <- D[i,j-1] + gap
        D[i,j] <- min(m1, m2, m3)
        wmin <- text[which(c(m1, m2, m3) == D[i,j])]
        if("m" %in% wmin & x1[i-1] != y1[j-1])
          wmin[wmin == "m"] <- "mm"
        M[i,j] <- paste(wmin, collapse = "/")
      }
    }
    rownames(M) <- rownames(D) <- c("gap", x1)
    colnames(M) <- colnames(D) <- c("gap", y1)
    d <- D[m+1, n+1]
    
    if(method == 2){  ## normalized levenshtein
      d <- 1-d / (max(m, n))
    }
  }
  
  
  if(method == 3){ ## Hamming
    if(length(x1) != length(y1))
      stop("Hamming distance is only defined for equal length strings")
    d <- sum(x1 != y1)
    D <- NULL
    M <- NULL
  }
  attr(d, "Size") <- 2
  attr(d, "Diag") <- FALSE
  if(length(x) > 1) x <- paste0("", x, collapse = "")
  if(length(y) > 1) y <- paste0("", y, collapse = "")
  attr(d, "Labels") <- c(x,y)
  attr(d, "Upper") <- FALSE
  attr(d, "method") <- METHODS[method]
  attr(d, "call") <- match.call()
  attr(d, "ScoringMatrix") <- D
  attr(d, "TraceBackMatrix") <- M
  class(d) <- c("stringDist", "dist")
  
  return(d)
}
