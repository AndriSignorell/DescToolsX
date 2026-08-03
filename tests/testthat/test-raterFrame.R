
d.long <- data.frame(
  expand.grid(subj = as.character(1:5), rater = LETTERS[1:3]),
  rating = c(1, 4, 5, 7, 2, 2, 5, 6, 7, 1, 1, 4, 6, 6, 2))


test_that("raterFrame() reshapes to subjects x raters", {

  m <- raterFrame(rating ~ subj | rater, data = d.long)
  expect_s3_class(m, "raterFrame")
  expect_s3_class(m, "data.frame")
  expect_equal(dim(m), c(5L, 4L))
  expect_equal(colnames(m), c("subj", "A", "B", "C"))
  expect_equal(m$A, c(1, 4, 5, 7, 2))

  expect_equal(attr(m, "subject"), "subj")
  expect_true(!is.null(attr(m, "data.name")))
})


test_that("raterFrame() keeps its attributes when the subject column is dropped", {

  m <- raterFrame(rating ~ subj | rater, data = d.long, dropSubj = TRUE)
  expect_equal(colnames(m), c("A", "B", "C"))
  # `[.data.frame` keeps only names/row.names/class, so attributes set before
  # the dropSubj subsetting used to disappear
  expect_true(!is.null(attr(m, "data.name")))
  expect_true(is.na(attr(m, "subject")))
})


test_that("raterFrame() applies na.action per subject", {

  m <- raterFrame(rating ~ subj | rater, data = d.long[-c(3, 6), ])
  expect_true(anyNA(m))

  mo <- raterFrame(rating ~ subj | rater, data = d.long[-c(3, 6), ],
                   na.action = na.omit)
  expect_false(anyNA(mo))
  expect_equal(nrow(mo), 3L)
  expect_setequal(attr(attr(mo, "na.action"), "values"), c("3", "1"))
})


test_that("raterFrame() survives an na.action with nothing to omit", {

  # na.omit on a complete frame leaves no "na.action" attribute; attr<-() on
  # NULL is an error, so this used to fail with
  # "attempt to set an attribute on NULL"
  expect_silent(m <- raterFrame(rating ~ subj | rater, data = d.long,
                                na.action = na.omit))
  expect_equal(nrow(m), 5L)
  expect_null(attr(m, "na.action"))

  expect_silent(raterFrame(rating ~ subj | rater, data = d.long,
                           na.action = na.pass))
})


test_that("raterFrame() copes with a single rater", {

  one <- d.long[d.long$rater == "A", ]
  m <- raterFrame(rating ~ subj | rater, data = one)
  expect_equal(dim(m), c(5L, 2L))
  expect_equal(colnames(m), c("subj", "A"))

  m2 <- raterFrame(rating ~ subj | rater, data = one, dropSubj = TRUE)
  expect_equal(dim(m2), c(5L, 1L))
})


test_that("raterFrame() refuses duplicated subject/rater combinations", {

  dup <- rbind(d.long, d.long[1L, ])
  expect_error(raterFrame(rating ~ subj | rater, data = dup),
               "duplicated subject/rater")
})


test_that("raterFrame() feeds the agreement functions", {

  m <- raterFrame(rating ~ subj | rater, data = d.long, dropSubj = TRUE)
  expect_length(percAgreement(m, input = "ratings"), 3L)
  expect_true(is.finite(randolphKappa(m)))
})
