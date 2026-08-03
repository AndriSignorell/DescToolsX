
# =========================================================
# entropy()
# =========================================================

# ---------------------------------------------------------
# Fair coin
# ---------------------------------------------------------

entropy(c(1,1))

# should be:
# 1 bit


# ---------------------------------------------------------
# Degenerate distribution
# ---------------------------------------------------------

entropy(c(10,0,0,0))

# should be:
# 0


# ---------------------------------------------------------
# Uniform distribution
# ---------------------------------------------------------

entropy(rep(1, 4))

# should be:
# 2 bits


# ---------------------------------------------------------
# Different log bases
# ---------------------------------------------------------

entropy(rep(1, 4), base = exp(1))

# should be:
# log(4)


entropy(rep(1, 4), base = 10)


# ---------------------------------------------------------
# Normalized entropy
# ---------------------------------------------------------

entropy(rep(1, 4), normalize = TRUE)

# should be:
# 1


entropy(c(10,0,0,0), normalize = TRUE)

# should be:
# 0


# ---------------------------------------------------------
# Table input
# ---------------------------------------------------------

tab <- matrix(
  c(10,20,
    30,40),
  nrow = 2
)

entropy(tab)


# ---------------------------------------------------------
# Vector input
# ---------------------------------------------------------

x <- c("A","A","B","B","C")

entropy(x)


# ---------------------------------------------------------
# x + y input
# ---------------------------------------------------------

y <- c("X","X","X","Y","Y")

entropy(x, y)


# ---------------------------------------------------------
# Zero handling
# ---------------------------------------------------------

entropy(c(1,0,0,0))

# should not produce NaN


# ---------------------------------------------------------
# Entropy bounds
# ---------------------------------------------------------

p <- sample(1:10, 5)

H <- entropy(p)

stopifnot(
  H >= 0,
  H <= log(length(p), base = 2)
)


# =========================================================
# mutInf()
# =========================================================

# ---------------------------------------------------------
# Independent variables
# ---------------------------------------------------------

tab_indep <- matrix(
  c(25,25,
    25,25),
  nrow = 2
)

mutInf(tab_indep)

# should be:
# approximately 0


# ---------------------------------------------------------
# Perfect association
# ---------------------------------------------------------

tab_perf <- matrix(
  c(50,0,
    0,50),
  nrow = 2
)

mutInf(tab_perf)

# should be:
# 1 bit


# ---------------------------------------------------------
# Symmetry
# ---------------------------------------------------------

stopifnot(
  all.equal(
    mutInf(tab),
    mutInf(t(tab))
  )
)


# ---------------------------------------------------------
# Normalized MI
# ---------------------------------------------------------

mutInf(tab_perf, normalize = TRUE)

# should be:
# 1


mutInf(tab_indep, normalize = TRUE)

# should be:
# 0


# ---------------------------------------------------------
# x + y input
# ---------------------------------------------------------

x <- c("A","A","A","B","B","B")
y <- c("X","X","Y","Y","Y","X")

mutInf(x, y)


# ---------------------------------------------------------
# Nonnegativity
# ---------------------------------------------------------

stopifnot(
  mutInf(tab) >= 0
)


# ---------------------------------------------------------
# MI bounded by marginal entropy
# ---------------------------------------------------------

hx <- entropy(rowSums(tab))
hy <- entropy(colSums(tab))
mi <- mutInf(tab)

stopifnot(
  mi <= min(hx, hy)
)


# ---------------------------------------------------------
# Large random table
# ---------------------------------------------------------

set.seed(1)

tab_rand <- matrix(
  sample(1:100, 25, TRUE),
  nrow = 5
)

mutInf(tab_rand)

mutInf(tab_rand, normalize = TRUE)


# ---------------------------------------------------------
# Names / scalar output
# ---------------------------------------------------------

is.numeric(mutInf(tab))
length(mutInf(tab)) == 1
is.numeric(entropy(tab))
length(entropy(tab)) == 1


test_that("entropy tabulates a categorical vector", {
  
  x <- c("A", "A", "B", "B", "C")
  
  # as.numeric() on a character vector used to give NA with a warning
  expect_false(is.na(entropy(x)))
  expect_equal(entropy(x), entropy(c(2, 2, 1)))
  
  # a fair coin is exactly one bit, a fair die log2(6)
  expect_equal(entropy(c(50, 50)), 1)
  expect_equal(entropy(rep(1, 6)), log2(6))
  expect_equal(entropy(rep(1, 6), base = exp(1)), log(6))
  
  # maximum entropy normalizes to 1, a single category to 0
  expect_equal(entropy(rep(1, 6), normalize = TRUE), 1)
  expect_equal(entropy(c(5, 0, 0), normalize = TRUE), 0)
})

