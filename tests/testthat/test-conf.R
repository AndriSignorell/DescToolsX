# helper: simple 2-class setup
.pred2 <- factor(c("A","A","B","B","A","B","A","B"))
.ref2  <- factor(c("A","B","A","B","A","B","B","A"))

# binary calls pass pos explicitly; the default and its message are tested once
test_that("conf announces the positive class it picked", {
  expect_message(res <- conf(.pred2, .ref2), "'pos' not specified")
  expect_s3_class(res, "Conf")
})

test_that("conf returns an object of class 'Conf'", {
  expect_s3_class(conf(.pred2, .ref2, pos = "A"), "Conf")
})

test_that("conf result contains required components", {
  res <- conf(.pred2, .ref2, pos = "A")
  expect_true(all(c("table","acc","n","kappa","byclass") %in% names(res)))
})

test_that("conf accuracy is in [0, 1]", {
  res <- conf(.pred2, .ref2, pos = "A")
  expect_gte(res$acc, 0)
  expect_lte(res$acc, 1)
})

test_that("conf accuracy CI: lci <= acc <= uci", {
  res <- conf(.pred2, .ref2, pos = "A")
  expect_lte(res$acc.lci, res$acc)
  expect_gte(res$acc.uci, res$acc)
})

test_that("conf n equals total number of observations", {
  res <- conf(.pred2, .ref2, pos = "A")
  expect_equal(res$n, length(.pred2))
})

test_that("conf kappa is in [-1, 1]", {
  res <- conf(.pred2, .ref2, pos = "A")
  expect_gte(res$kappa, -1)
  expect_lte(res$kappa,  1)
})

test_that("conf perfect prediction gives accuracy 1", {
  x <- factor(c("A","A","B","B"))
  res <- conf(x, x, pos = "A")
  expect_equal(res$acc, 1)
})

test_that("conf byclass contains expected row metrics", {
  res <- conf(.pred2, .ref2, pos = "A")
  expected_rows <- c("sens","spec","ppv","npv")
  expect_true(all(expected_rows %in% rownames(res$byclass)))
})

test_that("conf.table accepts a table directly", {
  tab <- table(Pred = .pred2, Ref = .ref2)
  res <- conf(tab, pos = "A")
  expect_s3_class(res, "Conf")
})

test_that("conf multiclass returns byclass with one column per class", {
  pred <- factor(c("A","B","C","A","B","C","A","B","C"))
  ref  <- factor(c("A","A","C","B","B","C","A","C","B"))
  res  <- conf(pred, ref)
  expect_equal(ncol(res$byclass), 3L)
})

test_that("conf na.rm = TRUE handles missing values", {
  pred <- factor(c("A","B",NA,"A","B"))
  ref  <- factor(c("A","B","A","B","A"))
  expect_s3_class(conf(pred, ref, na.rm = TRUE, pos = "A"), "Conf")
})

test_that("sensX() extracts sensitivity from conf object", {
  res <- sensX(.pred2, .ref2, pos = "A")
  expect_gte(res, 0)
  expect_lte(res, 1)
})

test_that("specX() extracts specificity from conf object", {
  res <- specX(.pred2, .ref2, pos = "A")
  expect_gte(res, 0)
  expect_lte(res, 1)
})



test_that("conf() survives class labels that are substrings of each other", {
  
  # "A" is a substring of "AB": grep(pos, ..., fixed = TRUE) matched both
  # and collapsed the 2x2 table to 1x1 without any error
  tab <- as.table(matrix(c(30, 5, 8, 27), nrow = 2,
                         dimnames = list(c("A", "AB"), c("A", "AB"))))
  
  res <- conf(tab, pos = "A")
  
  expect_equal(dim(res$table), c(2L, 2L))
  expect_equal(res$n, 70)
  expect_equal(res$diag, 57)
  expect_equal(unname(res$acc), 57 / 70)
})


test_that("conf() rejects an unknown positive class", {
  tab <- as.table(matrix(c(30, 5, 8, 27), nrow = 2,
                         dimnames = list(c("no", "yes"), c("no", "yes"))))
  expect_error(conf(tab, pos = "maybe"), "class labels")
})


test_that("multiclass one-vs-all collapsing is label-exact", {
  
  lbl <- c("1", "10", "11")
  tab <- as.table(matrix(c(20, 3, 2,
                           4, 18, 5,
                           1, 6, 21), nrow = 3, byrow = TRUE,
                         dimnames = list(lbl, lbl)))
  
  res <- conf(tab)
  
  # sensitivity of class "1" is 20 / (20 + 4 + 1)
  expect_equal(unname(res$byclass["sens", "1"]), 20 / 25)
  expect_equal(sum(res$byclass["prev", ]), 1)
})

test_that("conf metrics agree with an asymmetric hand-calculated table", {
  tab <- matrix(c(30, 5, 10, 55), 2, byrow = TRUE,
                dimnames = list(c("yes", "no"), c("yes", "no")))
  res <- conf(tab, pos = "yes", conf.level = 0.9)
  expected <- c(sens = 30/40, spec = 55/60, ppv = 30/35,
                npv = 55/65, prev = 0.4, detrate = 0.3, detprev = 0.35,
                bacc = (30/40 + 55/60)/2, fval = 60/75,
                mcc = (30*55 - 5*10)/sqrt(35*40*60*65))
  expect_equal(res$byclass[, "yes"], expected)
  expect_equal(res$n, 100)
  expect_equal(res$diag, 85)
  expect_equal(res$acc, 0.85)
  expect_equal(res$nir, 0.6)
  expect_equal(unname(res$kappa), (0.85 - 0.53)/(1 - 0.53))
  expect_equal(res$acc.pval,
               stats::binom.test(85, 100, p = 0.6, alternative = "greater")$p.value)
  expect_equal(unname(sensX(tab, pos = "yes")), 30/40)
  expect_equal(unname(specX(tab, pos = "yes")), 55/60)
})

test_that("conf rejects malformed inputs and confidence levels", {
  tab <- matrix(c(30, 5, 10, 55), 2,
                dimnames = list(c("yes", "no"), c("yes", "no")))
  expect_error(conf(matrix(1:6, 2)), "square numeric matrix")
  expect_error(conf(matrix("a", 2, 2)), "square numeric matrix")
  bad <- tab
  colnames(bad) <- rev(colnames(bad))
  expect_error(conf(bad, pos = "yes"), "must be identical")
  for (level in list(NA_real_, 0, 1, Inf, "0.95", c(0.8, 0.9), numeric())) {
    expect_error(conf(tab, pos = "yes", conf.level = level), "single number")
  }
  expect_error(conf(c("yes", "no")), "ref.*must be provided")
  expect_error(conf(c("yes", "no"), "yes"), "same length")
})

test_that("conf removes pairs with missing predictions or references", {
  pred <- c("yes", "no", NA, "yes", "no", "yes", "no", "yes")
  ref <- c("yes", "yes", "no", NA, "no", "no", "yes", "yes")
  keep <- complete.cases(pred, ref)
  expected <- conf(pred[keep], ref[keep], pos = "yes", na.rm = FALSE)
  expect_identical(conf(pred, ref, pos = "yes"), expected)
})

test_that("conf reports undefined precision and MCC for an empty prediction margin", {
  tab <- matrix(c(0, 0, 10, 20), 2, byrow = TRUE,
                dimnames = list(c("yes", "no"), c("yes", "no")))
  res <- conf(tab, pos = "yes")
  expect_true(is.na(res$byclass["mcc", "yes"]))
  expect_true(is.na(res$byclass["ppv", "yes"]))
  expect_equal(unname(res$byclass["sens", "yes"]), 0)
  expect_equal(unname(res$byclass["spec", "yes"]), 1)
})

test_that("Conf printing supports binary, multiclass and legacy objects", {
  binary <- conf(.pred2, .ref2, pos = "A", conf.level = 0.9)
  out <- capture.output(ret <- withVisible(print(binary)))
  expect_false(ret$visible)
  expect_s3_class(ret$value, "Conf")
  expect_match(paste(out, collapse = "\n"), "90% CI", fixed = TRUE)
  expect_match(paste(out, collapse = "\n"), "'Positive' Class : A", fixed = TRUE)
  binary$conf.level <- NULL
  expect_output(print(binary), "95% CI")
  tab <- matrix(c(20, 3, 2, 4, 18, 5, 1, 6, 21), 3,
                dimnames = list(LETTERS[1:3], LETTERS[1:3]))
  multi <- conf(tab, pos = "ignored for multiclass")
  expect_null(multi$pos)
  out <- paste(capture.output(print(multi)), collapse = "\n")
  expect_match(out, "Overall Statistics")
  expect_match(out, "Statistics by Class")
})

test_that("Conf plotting draws on a file device", {
  path <- tempfile(fileext = ".pdf")
  grDevices::pdf(path)
  on.exit({ grDevices::dev.off(); unlink(path) }, add = TRUE)
  expect_error(plot(conf(.pred2, .ref2, pos = "A"), main = "Classification"), NA)
})

test_that("conf.glm agrees with explicit cutoff predictions", {
  dat <- data.frame(z = 1:12, y = c(0, 0, 1, 0, 1, 0, 1, 1, 0, 1, 0, 1))
  fit <- stats::glm(y ~ z, data = dat, family = stats::binomial())
  for (cutoff in c(0.4, 0.5, 0.6)) {
    pred <- ifelse(stats::predict(fit, type = "response") > cutoff, "1", "0")
    expect_identical(conf(fit, cutoff = cutoff), conf(pred, dat$y, pos = "1"))
  }
  dat$y <- factor(dat$y, labels = c("no", "yes"))
  fit <- stats::glm(y ~ z, data = dat, family = stats::binomial())
  pred <- ifelse(stats::predict(fit, type = "response") > 0.5, "yes", "no")
  expect_identical(conf(fit, pos = "no"), conf(pred, dat$y, pos = "no"))
  fit$model <- NULL
  expect_error(conf(fit), "model frame")
  fit <- stats::glm(Sepal.Length ~ Sepal.Width, data = iris)
  expect_error(conf(fit), "binary response")
})

test_that("conf.rpart agrees with fitted classification predictions", {
  skip_if_not_installed("rpart")
  fit <- rpart::rpart(Species ~ ., data = iris, method = "class", y = TRUE)
  expect_identical(conf(fit), conf(stats::predict(fit, type = "class"), iris$Species))
})

test_that("conf.multinom requires a model frame and uses fitted classes", {
  skip_if_not_installed("nnet")
  fit <- nnet::multinom(Species ~ ., data = iris, model = TRUE,
                        trace = FALSE, maxit = 300)
  expect_identical(conf(fit), conf(stats::predict(fit, type = "class"), iris$Species))
  fit$model <- NULL
  expect_error(conf(fit), "model frame")
})

test_that("conf.randomForest uses out-of-bag predictions", {
  skip_if_not_installed("randomForest")
  set.seed(6401)
  fit <- randomForest::randomForest(Species ~ ., data = iris, ntree = 50)
  expect_identical(conf(fit), conf(fit$predicted, fit$y))
})
