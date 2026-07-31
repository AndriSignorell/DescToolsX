
test_that("there is exactly one .nctCI and it returns named limits", {

  ns <- asNamespace("DescToolsX")
  expect_true(exists(".nctCI", envir = ns, inherits = FALSE))

  # both callers index the result by name
  lim <- DescToolsX:::.nctCI(2.5, df = 20, conf.level = 0.95)
  expect_named(lim, c("lci", "uci"))
  expect_lt(lim[["lci"]], lim[["uci"]])

  one <- DescToolsX:::.nctCI(2.5, df = 20, conf.level = 0.95, sides = "left")
  expect_identical(unname(one[["uci"]]), Inf)
  expect_true(is.finite(one[["lci"]]))
})


test_that("cohenD and coefVarCI still work through the shared .nctCI", {

  x <- c(5.1, 4.8, 6.2, 5.5, 5.9, 6.4, 4.9, 5.7)
  y <- c(4.2, 4.6, 4.1, 5.0, 4.4, 4.8, 4.3, 4.7)

  d <- cohenD(x, y, conf.level = 0.95)
  expect_named(d, c("est", "lci", "uci"))
  expect_lt(d[["lci"]], d[["est"]])

  cv <- coefVarCI(c(x, y), method = "nct")
  expect_named(cv, c("est", "lci", "uci"))
})


test_that("gini stays inside [0, 1] however the weights are expressed", {

  # frequency weights and the equivalent replicated vector must agree
  weighted <- gini(c(10, 0), weights = c(2, 3))
  replicated <- gini(c(0, 0, 0, 10, 10))

  expect_equal(weighted, replicated)
  expect_lte(weighted, 1)
  expect_gte(weighted, 0)

  # unweighted correction is unchanged: n/(n-1)
  x <- c(10, 20, 30, 40)
  expect_equal(gini(x, unbiased = TRUE),
               gini(x, unbiased = FALSE) * length(x) / (length(x) - 1))
})


test_that("gini honours sides", {

  set.seed(1)
  x <- rlnorm(60)

  two   <- gini(x, conf.level = 0.95, R = 299)
  left  <- gini(x, conf.level = 0.95, R = 299, sides = "left")
  right <- gini(x, conf.level = 0.95, R = 299, sides = "right")

  expect_equal(unname(left[["uci"]]), 1)
  expect_equal(unname(right[["lci"]]), 0)
  expect_true(is.finite(left[["lci"]]))
  expect_true(is.finite(right[["uci"]]))

  expect_gte(unname(two[["lci"]]), 0)
  expect_lte(unname(two[["uci"]]), 1)
})


test_that("gsd survives a zero when na.rm = TRUE", {

  expect_equal(gsd(c(1, 2, 4), na.rm = TRUE), gsd(c(1, 2, 4)))
  expect_false(is.na(gsd(c(1, 2, 0, 4), na.rm = TRUE)))
  expect_equal(gsd(c(1, 2, 0, 4), na.rm = TRUE), gsd(c(1, 2, 4)))

  expect_true(is.na(gsd(c(1, 2, 0, 4))))   # na.rm = FALSE
})


test_that("hmean closes the open side at 0, not at NA", {

  set.seed(2)
  x <- rlnorm(40)

  left  <- hmean(x, conf.level = 0.95, sides = "left")
  right <- hmean(x, conf.level = 0.95, sides = "right")

  expect_identical(unname(left[["uci"]]), Inf)
  expect_equal(unname(right[["lci"]]), 0)
  expect_false(is.na(right[["lci"]]))

  expect_named(left, c("est", "lci", "uci"))
})


test_that("freq actually sorts by level name", {

  x <- factor(c("b", "b", "a", "c", "c", "c"),
              levels = c("c", "b", "a"))   # levels NOT alphabetical

  byLevel <- freq(x, ord = "level")
  byName  <- freq(x, ord = "name")

  expect_equal(as.character(byLevel$level), c("c", "b", "a"))
  expect_equal(as.character(byName$level),  c("a", "b", "c"))
})


test_that("large/small do not read past the end when NAs dominate", {

  # The point of this test is the out-of-bounds read: k was capped at the
  # length BEFORE the NAs were stripped, so top_i_cpp() ran past the end
  # of a two-element vector. Assert the contents, not the ordering - both
  # functions return ascending, which is a separate contract and not what
  # is under test here.
  x <- c(1, 2, NA, NA, NA)

  expect_length(large(x, k = 5), 2L)
  expect_length(small(x, k = 5), 2L)
  expect_setequal(large(x, k = 5), c(1, 2))
  expect_setequal(small(x, k = 5), c(1, 2))

  expect_equal(max(large(x, k = 5)), 2)
  expect_equal(min(small(x, k = 5)), 1)
})


test_that("gkTau is clamped and tagged as a nominal measure", {

  tab <- as.table(rbind(c(26, 26, 23, 18, 9), c(6, 7, 9, 14, 23)))

  res <- gkTau(tab, direction = "row", conf.level = 0.95)
  expect_gte(unname(res[["lci"]]), 0)
  expect_lte(unname(res[["uci"]]), 1)

  # reduces to phi^2 for a 2x2 table
  t2 <- as.table(cbind(c(11, 2), c(4, 6)))
  expect_equal(unname(gkTau(t2, direction = "row")), unname(phi(t2)^2),
               tolerance = 1e-8)
  expect_equal(unname(gkTau(t2, direction = "column")), unname(phi(t2)^2),
               tolerance = 1e-8)
})


test_that("gkGamma forwards its dots and matches ordAssocs", {

  tab <- as.table(rbind(c(26, 26, 23, 18, 9), c(6, 7, 9, 14, 23)))

  expect_equal(gkGamma(tab), unname(ordAssocs(tab, which = "gamma")$gamma))

  g <- gkGamma(tab, conf.level = 0.95)
  expect_named(g, c("est", "lci", "uci"))
})


test_that("freq2D copes with a single occupied bin row", {

  x <- c(1, 1, 1, 1)
  y <- c(1, 2, 3, 4)

  # trimming empty margins used to drop the matrix to a vector
  expect_silent(z <- freq2D(x, y, n = 5))
  expect_true(is.matrix(z))
})


test_that("herfindahl rejects a degenerate parameter", {

  x <- c(541, 1463, 2445, 3438)

  expect_error(herfindahl(x, parameter = 0), "positive")
  expect_true(is.na(herfindahl(c(0, 0, 0))))
  expect_equal(herfindahl(x), sum((x / sum(x))^2))
})


test_that("findCorrX removes the higher-scoring variable of a pair", {

  cmat <- matrix(c(1,   0.95, 0.10,
                   0.95, 1,   0.12,
                   0.10, 0.12, 1), nrow = 3,
                 dimnames = list(paste0("V", 1:3), paste0("V", 1:3)))

  idx <- findCorrX(cmat, cutoff = 0.8)
  expect_length(idx, 1L)
  expect_true(idx %in% c(1L, 2L))

  # differing row and column names must not be read as asymmetry
  cm2 <- cmat
  rownames(cm2) <- paste0("r", 1:3)
  expect_silent(findCorrX(cm2, cutoff = 0.8))

  expect_error(findCorrX(unname(cmat), cutoff = 0.8, output = "names"),
               "output = 'index'")
})
