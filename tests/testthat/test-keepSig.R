

test_that("keepSig's diag argument does something", {
  
  m <- cor(mtcars)
  
  kept <- keepSig(m, data = mtcars, diag = TRUE)
  expect_equal(diag(kept), diag(m))
  
  # diag = FALSE was documented to blank the diagonal and did nothing:
  # the internally computed p-values are NA there, and a logical index
  # containing NA skips those cells
  dropped <- keepSig(m, data = mtcars, diag = FALSE)
  expect_true(all(is.na(diag(dropped))))
  
  # off-diagonal behaviour is unchanged between the two
  offd <- upper.tri(m)
  expect_equal(kept[offd], dropped[offd])
})


test_that("keepSig blanks exactly the non-significant cells", {
  
  m <- cor(swiss)
  p <- outer(seq_len(ncol(m)), seq_len(ncol(m)),
             Vectorize(function(i, j)
               if (i == j) NA_real_
               else cor.test(swiss[[i]], swiss[[j]])$p.value))
  dimnames(p) <- dimnames(m)
  
  out <- keepSig(m, p = p)
  
  expect_true(all(is.na(out[!is.na(p) & p > 0.05])))
  expect_false(any(is.na(out[!is.na(p) & p <= 0.05])))
  
  expect_error(keepSig(m), "either 'p'")
  expect_error(keepSig(m, data = swiss, sig.level = 0), "sig.level")
})


