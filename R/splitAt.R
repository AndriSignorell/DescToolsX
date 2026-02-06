

splitAt <- function(x, pos) {
  # splits a vector at given positions
  
  # source: https://stackoverflow.com/questions/16357962/r-split-numeric-vector-at-position
  # author: Joshua Ulrich
  # unname(split(x, findInterval(x, pos)))
  
  # better from flodel
  pos <- c(1L, pos, length(x) + 1L)
  Map(function(x, i, j) x[i:j], list(x), head(pos, -1L), tail(pos, -1L) - 1L)
  
}

