# ---- shared setup ----
outcome    <- c(1.4, 2.1, 3.0, 2.1, 3.2, 4.7, 3.5, 4.5, 5.4)
treatment1 <- factor(c(1, 1, 1, 2, 2, 2, 3, 3, 3))
anova1     <- aov(outcome ~ treatment1)

treatment2 <- factor(c(1, 2, 3, 1, 2, 3, 1, 2, 3))
anova2     <- aov(outcome ~ treatment1 + treatment2)

test_that("etaSq.lm returns a matrix with 2 columns for one-way ANOVA", {
  E <- etaSq(anova1)
  expect_true(is.matrix(E))
  expect_equal(ncol(E), 2L)
})

test_that("etaSq.lm column names are eta.sq and eta.sq.part", {
  E <- etaSq(anova1)
  expect_equal(colnames(E), c("eta.sq", "eta.sq.part"))
})

test_that("etaSq eta.sq values are in [0, 1]", {
  E <- etaSq(anova1)
  expect_true(all(E[, "eta.sq"] >= 0 & E[, "eta.sq"] <= 1))
})

test_that("etaSq partial eta.sq values are in [0, 1]", {
  E <- etaSq(anova1)
  expect_true(all(E[, "eta.sq.part"] >= 0 & E[, "eta.sq.part"] <= 1))
})

test_that("etaSq eta.sq sums to <= 1 across all terms (one-way)", {
  E <- etaSq(anova1)
  expect_lte(sum(E[, "eta.sq"]), 1 + 1e-9)
})

test_that("etaSq type = 1 returns the same structure as type = 2", {
  E1 <- etaSq(anova1, type = 1)
  E2 <- etaSq(anova1, type = 2)
  expect_equal(dim(E1), dim(E2))
})

test_that("etaSq type = 3 works for one-way ANOVA", {
  E3 <- etaSq(anova1, type = 3)
  expect_true(is.matrix(E3))
})

test_that("etaSq anova = TRUE adds SS, df, MS, F, p columns", {
  E <- etaSq(anova1, anova = TRUE)
  expect_true(all(c("SS","df","MS","F","p") %in% colnames(E)))
})

test_that("etaSq works for two-way ANOVA", {
  E <- etaSq(anova2)
  expect_equal(nrow(E), 2L)   # two terms: treatment1 and treatment2
})

test_that("etaSq row names match the ANOVA term labels", {
  E <- etaSq(anova2)
  expect_true(all(rownames(E) %in% c("treatment1","treatment2")))
})

test_that("etaSq stops with invalid type", {
  expect_error(etaSq(anova1, type = 99))
})

test_that("etaSq stops with non-logical anova argument", {
  expect_error(etaSq(anova1, anova = "yes"))
})


# Review 25.09.2026 ------------------------------------------------------------

# balanced 3 x 2 design with two replicates per cell
dBal <- data.frame(a = gl(3, 4), b = gl(2, 2, 12),
                   y = c(4.1, 5.0, 6.2, 5.8, 3.9, 4.4, 7.1, 6.5, 5.2, 5.9, 8.0, 7.4))

test_that("in a balanced design all three types agree (sum-to-zero contrasts)", {
  fit <- lm(y ~ a * b, data = dBal,
            contrasts = list(a = "contr.sum", b = "contr.sum"))
  E1 <- etaSq(fit, type = 1)
  expect_no_warning(E3 <- etaSq(fit, type = 3))
  expect_equal(etaSq(fit, type = 2), E1)
  expect_equal(E3, E1)
  expect_identical(rownames(E1), c("a", "b", "a:b"))
})

test_that("type 3 warns about treatment contrasts with several factors", {
  fit <- lm(y ~ a * b, data = dBal)
  expect_warning(etaSq(fit, type = 3), "sum-to-zero contrasts")
})

test_that("type 2 SS of a main effect ignores its interaction", {
  d <- dBal[-c(1, 6, 11), ]                      # unbalanced
  fit <- lm(y ~ a * b, data = d)
  ssTot <- sum((d$y - mean(d$y))^2)
  ssA <- anova(lm(y ~ b, d), lm(y ~ a + b, d))$`Sum of Sq`[2]
  ssAB <- anova(lm(y ~ a + b, d), fit)$`Sum of Sq`[2]
  E <- etaSq(fit, type = 2)
  expect_equal(unname(E["a", "eta.sq"]), ssA / ssTot)
  expect_equal(unname(E["a:b", "eta.sq"]), ssAB / ssTot)
  ssRes <- sum(residuals(fit)^2)
  expect_equal(unname(E["a", "eta.sq.part"]), ssA / (ssA + ssRes))
})

test_that("anova = TRUE reproduces the type-1 ANOVA table", {
  fit <- lm(y ~ a + b, data = dBal)
  E <- etaSq(fit, type = 1, anova = TRUE)
  ref <- anova(fit)
  expect_equal(unname(E[, "SS"]), ref[, "Sum Sq"])
  expect_equal(unname(E[1:2, "F"]), ref[1:2, "F value"])
  expect_equal(unname(E[1:2, "p"]), ref[1:2, "Pr(>F)"])
  expect_identical(rownames(E)[3], "Residuals")
  expect_true(is.na(E["Residuals", "eta.sq.part"]))
})

test_that("etaSq.lm needs the model frame and a valid type", {
  expect_error(etaSq(lm(y ~ a, dBal, model = FALSE)), "model frame")
  expect_error(etaSq(lm(y ~ a, dBal), type = "2"), "type must be")
  expect_error(etaSq(lm(y ~ a, dBal), type = 1:2), "type must be")
})


# ---- aovlist -----------------------------------------------------------------

npkFit <- aov(yield ~ N * P * K + Error(block), data = npk)

test_that("etaSq.aovlist works with its default type and matches summary()", {
  E  <- etaSq(npkFit)                        # type = 1 is the method default
  s  <- summary(npkFit)
  sw <- s[["Error: Within"]][[1]]
  sb <- s[["Error: block"]][[1]]
  rw <- trimws(rownames(sw))
  rb <- trimws(rownames(sb))
  ssN  <- sw[rw == "N", "Sum Sq"]
  sseW <- sw[rw == "Residuals", "Sum Sq"]
  sseB <- sb[rb == "Residuals", "Sum Sq"]

  expect_identical(colnames(E), c("eta.sq", "eta.sq.part", "eta.sq.gen"))
  expect_true("N:P:K" %in% rownames(E))       # the term tested in the block stratum
  expect_equal(unname(E["N", "eta.sq"]),
               ssN / sum((npk$yield - mean(npk$yield))^2))
  expect_equal(unname(E["N", "eta.sq.part"]), ssN / (ssN + sseW))
  expect_equal(unname(E["N", "eta.sq.gen"]), ssN / (ssN + sseW + sseB))
})

test_that("etaSq.aovlist anova = TRUE carries the stratum statistics", {
  E  <- etaSq(npkFit, anova = TRUE)
  sw <- summary(npkFit)[["Error: Within"]][[1]]
  expect_true(is.data.frame(E))
  expect_identical(colnames(E), c("eta.sq", "eta.sq.part", "eta.sq.gen", "SS",
                                  "df", "MS", "SSE", "dfE", "F", "p"))
  expect_equal(E["N", "F"], sw[trimws(rownames(sw)) == "N", "F value"])
})

test_that("strata without tested terms are skipped", {
  E <- etaSq(aov(yield ~ N + Error(block), data = npk))
  expect_identical(rownames(E), "N")
})

test_that("a stratum whose terms have no residual df is an error, not a gap", {
  d <- data.frame(block = gl(6, 2), g = gl(6, 2),
                  y = c(3.1, 2.9, 4.2, 4.0, 5.3, 4.8, 3.6, 3.9, 5.0, 5.5, 4.4, 4.1))
  expect_error(etaSq(aov(y ~ g + Error(block), data = d)),
               "no residual degrees of freedom")
})

test_that("etaSq.aovlist validates type and anova", {
  expect_error(etaSq(npkFit, type = 2), "type must be equal to 1")
  expect_error(etaSq(npkFit, type = "1"), "type must be")
  expect_error(etaSq(npkFit, anova = NA_character_), "single logical")
})


test_that("etaSq.lm refuses glm and mlm fits", {
  gfit <- glm(vs ~ mpg, data = mtcars, family = binomial)
  expect_error(etaSq(gfit), "linear models")
  mfit <- lm(cbind(mpg, qsec) ~ factor(cyl), data = mtcars)
  expect_error(etaSq(mfit), "linear models")
})

test_that("frequency weights give the result of the replicated data", {
  wts  <- rep(1:3, 4)
  dRep <- dBal[rep(seq_len(nrow(dBal)), wts), ]
  dUnb <- dBal[-c(1, 6), ]
  wUnb <- wts[-c(1, 6)]
  dUnbRep <- dUnb[rep(seq_len(nrow(dUnb)), wUnb), ]
  cs <- list(a = "contr.sum", b = "contr.sum")

  for (type in 1:3) {
    fitW <- lm(y ~ a * b, data = dUnb, weights = wUnb, contrasts = cs)
    fitR <- lm(y ~ a * b, data = dUnbRep, contrasts = cs)
    expect_equal(etaSq(fitW, type = type), etaSq(fitR, type = type),
                 info = type)
  }
  # the type-2 refits are weighted too: main effect of a in y ~ a * b
  fitW <- lm(y ~ a * b, data = dBal, weights = wts)
  fitR <- lm(y ~ a * b, data = dRep)
  expect_equal(etaSq(fitW, type = 2), etaSq(fitR, type = 2))
})
