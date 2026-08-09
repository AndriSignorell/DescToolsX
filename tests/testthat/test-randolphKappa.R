
x5 <- matrix(c(
  1, 1, 1,
  2, 2, 2,
  1, 2, 1,
  3, 3, 3,
  2, 2, 1
), ncol = 3, byrow = TRUE)


test_that("randolphKappa() uses the pairwise observed agreement", {

  # per subject: sum n_ij (n_ij - 1) / (m (m-1))
  #   (1,1,1) -> 6/6 = 1        (2,2,2) -> 1
  #   (1,2,1) -> 2/6 = 1/3      (3,3,3) -> 1
  #   (2,2,1) -> 2/6 = 1/3
  # Po = 11/15, k = 3, Pe = 1/3  =>  kappa = (11/15 - 1/3) / (2/3) = 0.6
  expect_equal(randolphKappa(x5), 0.6)

  # the modal-category share max_j n_ij / m used before gives 13/15 and
  # therefore 0.8 -- guard against a relapse
  expect_false(isTRUE(all.equal(randolphKappa(x5), 0.8)))
})


test_that("randolphKappa() agrees with percAgreement() on Po", {

  po <- unname(percAgreement(x5, input = "ratings"))
  k <- 3
  expect_equal(randolphKappa(x5), (po - 1 / k) / (1 - 1 / k))
})


test_that("randolphKappa() can become negative", {

  # two raters, two categories, complete disagreement: Po = 0, Pe = 1/2
  x <- matrix(c(1, 2,
                2, 1,
                1, 2), ncol = 2, byrow = TRUE)
  expect_equal(randolphKappa(x), -1)

  # complete agreement
  y <- matrix(c(1, 1, 2, 2), ncol = 2, byrow = TRUE)
  expect_equal(randolphKappa(y), 1)
})


test_that("randolphKappa() honours the number of available categories", {

  # only three of five categories were used
  expect_equal(randolphKappa(x5, categories = 5),
               (11 / 15 - 1 / 5) / (1 - 1 / 5))
  expect_equal(randolphKappa(x5, categories = c("a", "b", "c", "d", "e")),
               (11 / 15 - 1 / 5) / (1 - 1 / 5))

  # more categories -> less chance agreement -> larger kappa
  expect_gt(randolphKappa(x5, categories = 5), randolphKappa(x5))

  expect_error(randolphKappa(x5, categories = 2), "fewer entries")
})


test_that("randolphKappa() reads data frames cell-wise, not column-wise", {

  df <- as.data.frame(x5)
  expect_equal(randolphKappa(df), randolphKappa(x5))

  # as.vector() on a data frame yields the list of COLUMNS, so counting
  # unique() elements there counts distinct raters, not distinct categories
  # three distinct rater columns but four distinct categories: reading the
  # columns gives k = 3, reading the cells gives k = 4
  d2 <- as.data.frame(matrix(c(1, 2, 3,
                               2, 3, 4,
                               1, 1, 2), ncol = 3, byrow = TRUE))
  expect_equal(randolphKappa(d2), randolphKappa(as.matrix(d2)))
  expect_equal(randolphKappa(d2), ((1 / 9) - 1 / 4) / (1 - 1 / 4))
})


test_that("randolphKappa() handles missing ratings per subject", {

  x <- matrix(c(1, 1, 1,
                2, 2, NA,
                1, 2, 1), ncol = 3, byrow = TRUE)
  # subject 2 has two ratings -> its agreement is 1, not 1/3
  expect_equal(randolphKappa(x), ((1 + 1 + 1 / 3) / 3 - 1 / 2) / (1 - 1 / 2))

  x1 <- matrix(c(1, 1,
                 2, 2,
                 NA, NA), ncol = 2, byrow = TRUE)
  expect_warning(randolphKappa(x1), "fewer than two ratings")

  expect_error(randolphKappa(matrix(c(NA, NA, NA, NA), ncol = 2)),
               "at least two categories")
})


test_that("randolphKappa() refuses instead of ignoring conf.level", {
  expect_error(randolphKappa(x5, conf.level = 0.95), "not implemented")
  expect_silent(randolphKappa(x5, conf.level = NA))
})


test_that("randolphKappa() validates its input", {
  expect_error(randolphKappa(c(1, 2, 3)), "matrix or data frame")
  expect_error(randolphKappa(matrix(1:3, ncol = 1)), "at least two raters")
})
