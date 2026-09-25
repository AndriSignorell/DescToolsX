# Labels a square count matrix with identical row and column levels, so
# that normalizeToConfusion() does not warn about missing dimnames (the
# warning is intended for users; tests should not trigger it by accident).
withLevels <- function(m, lvl = LETTERS[seq_len(nrow(m))]) {
  dimnames(m) <- list(lvl, lvl)
  m
}
