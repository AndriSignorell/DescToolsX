
.m <- as.table(cbind(c(1768,946,115), c(807,1387,438), c(189,746,288), c(47,53,16)))

test_that("uncertCoef returns a single numeric (symmetric, no CI)", {
  res <- uncertCoef(.m)
  expect_length(res, 1); expect_true(is.numeric(res))
})

test_that("uncertCoef result is in [0, 1]", {
  res <- uncertCoef(.m)
  expect_gte(res, 0); expect_lte(res, 1)
})

test_that("uncertCoef direction = 'row' and 'column' give different values", {
  r <- uncertCoef(.m, direction = "row")
  c <- uncertCoef(.m, direction = "column")
  expect_false(isTRUE(all.equal(r, c)))
})

test_that("uncertCoef is 0 for an independent table (all cells equal)", {
  tab <- as.table(matrix(rep(25,4), nrow=2))
  expect_equal(uncertCoef(tab), 0, tolerance=1e-6)
})

test_that("uncertCoef symmetric is the harmonic mean of row and column", {
  # sym = 2*MI/(H(X)+H(Y)), row = MI/H(X), col = MI/H(Y)
  # => sym = 2*row*col / (row+col)  (harmonic mean)
  r   <- uncertCoef(.m, direction = "row")
  col <- uncertCoef(.m, direction = "column")
  sym <- uncertCoef(.m, direction = "symmetric")
  expect_equal(sym, 2 * r * col / (r + col), tolerance = 1e-10)
  expect_true(sym >= 0 && sym <= 1)
})

test_that("uncertCoef conf.level returns named vector est/lci/uci", {
  res <- uncertCoef(.m, conf.level=0.95)
  expect_length(res, 3)
  expect_named(res, c("est","lci","uci"))
})

test_that("uncertCoef CI: lci < est < uci", {
  res <- uncertCoef(.m, conf.level=0.95)
  expect_lt(res["lci"], res["est"])
  expect_gt(res["uci"], res["est"])
})

test_that("uncertCoef accepts two vectors", {
  x <- factor(c("A","A","B","B"))
  y <- factor(c("X","Y","X","Y"))
  res <- uncertCoef(x, y)
  expect_gte(res, 0); expect_lte(res, 1)
})


gk <- as.table(cbind(c(1768, 946, 115), c(807, 1387, 438),
                     c(189, 746, 288), c(47, 53, 16)))
dimnames(gk) <- list(paste("A", 1:3), paste("B", 1:4))


# independent re-implementation of the point estimate
.ucRef <- function(x, direction){
  n <- sum(x)
  rs <- rowSums(x); cs <- colSums(x)
  hx <- -sum(rs * log(rs/n))/n
  hy <- -sum(cs * log(cs/n))/n
  hxy <- -sum(x * log(x/n))/n
  switch(direction,
         symmetric = 2*(hx + hy - hxy)/(hx + hy),
         row       =   (hx + hy - hxy)/hx,
         column    =   (hx + hy - hxy)/hy)
}


test_that("uncertCoef() reproduces the entropy definition", {
  
  for(d in c("symmetric", "row", "column"))
    expect_equal(uncertCoef(gk, direction = d), .ucRef(gk, d), tolerance = 1e-8)
  
})


test_that("the symmetric coefficient is the harmonic mean of the two directional ones", {
  
  ur <- uncertCoef(gk, direction = "row")
  uc <- uncertCoef(gk, direction = "column")
  expect_equal(uncertCoef(gk, direction = "symmetric"),
               2 / (1/ur + 1/uc), tolerance = 1e-8)
  
})


test_that("all coefficients lie in [0, 1]", {
  
  for(d in c("symmetric", "row", "column")){
    u <- uncertCoef(gk, direction = d)
    expect_gte(u, 0)
    expect_lte(u, 1)
  }
  
  # perfect association: knowing the row determines the column
  perfect <- as.table(rbind(c(20, 0), c(0, 30)))
  expect_equal(uncertCoef(perfect, direction = "row"), 1, tolerance = 1e-3)
  
})


test_that("est lies inside the confidence interval", {
  
  for(d in c("symmetric", "row", "column")){
    ci <- uncertCoef(gk, conf.level = 0.95, direction = d)
    expect_named(ci, c("est", "lci", "uci"))
    expect_lte(ci[["lci"]], ci[["est"]])
    expect_lte(ci[["est"]], ci[["uci"]])
  }
  
})


test_that("the interval is truncated to [0, 1], not to [-1, 1]", {
  
  # regression: the lower bound was clipped at -1 although the parameter
  # cannot be negative
  weak <- as.table(rbind(c(50, 50), c(50, 51)))
  ci <- uncertCoef(weak, conf.level = 0.95)
  expect_gte(ci[["lci"]], 0)
  expect_lte(ci[["uci"]], 1)
  
})


test_that("sides is implemented", {
  
  # regression: 'sides' was part of the signature but never used
  two   <- uncertCoef(gk, conf.level = 0.95)
  left  <- uncertCoef(gk, conf.level = 0.95, sides = "left")
  right <- uncertCoef(gk, conf.level = 0.95, sides = "right")
  
  expect_equal(left[["uci"]], 1)
  expect_equal(right[["lci"]], 0)
  
  # one-sided bounds are tighter than the two-sided ones
  expect_gt(left[["lci"]], two[["lci"]])
  expect_lt(right[["uci"]], two[["uci"]])
  
  expect_equal(left[["est"]], two[["est"]])
  
})


test_that("uncertCoef() validates its input", {
  
  expect_error(uncertCoef(c(10, 20, 30)), "two-dimensional")
  expect_error(uncertCoef(matrix(c(10, 20), nrow = 1)), "at least two rows")
  expect_error(uncertCoef(matrix(c(1, 2, -3, 4), nrow = 2)), "non-negative")
  expect_error(uncertCoef(gk, conf.level = 1.2), "conf.level")
  expect_error(uncertCoef(gk, conf.level = 0.4, sides = "left"), "greater than 0.5")
  
})


test_that("the vector interface tabulates", {
  
  x <- c("a", "a", "b", "b", "a", "b", "a")
  y <- c("u", "v", "u", "v", "u", "u", "v")
  expect_equal(uncertCoef(x, y), uncertCoef(table(x, y)))
  
})

