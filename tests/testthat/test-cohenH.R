
# =========================================================
# Tests
# =========================================================

tab <- matrix(
  c(26, 26,
    6, 7),
  nrow = 2,
  byrow = TRUE
)

# ---------------------------------------------------------
# Point estimate only
# ---------------------------------------------------------

cohenH(tab, conf.level = NA)

# manual check
2 * asin(sqrt(26/52)) -
  2 * asin(sqrt(6/13))


# ---------------------------------------------------------
# CI output
# ---------------------------------------------------------

cohenH(tab)

cohenH(tab, conf.level = 0.99)


# ---------------------------------------------------------
# Using vectors
# ---------------------------------------------------------

x <- c(rep("A", 52),
       rep("B", 13))

y <- c(rep(c("yes", "no"),
           c(26,26)),
       rep(c("yes", "no"),
           c(6,7)))

cohenH(x, y)


# ---------------------------------------------------------
# Symmetry check
# ---------------------------------------------------------

tab2 <- tab[c(2,1), ]

cohenH(tab2, conf.level = NA)

# should equal:
-cohenH(tab, conf.level = NA)


# ---------------------------------------------------------
# Zero effect
# ---------------------------------------------------------

tab0 <- matrix(
  c(50, 50,
    50, 50),
  nrow = 2,
  byrow = TRUE
)

cohenH(tab0)

# estimate should be 0


# ---------------------------------------------------------
# Extreme positive effect
# ---------------------------------------------------------

tab1 <- matrix(
  c(100, 0,
    0, 100),
  nrow = 2,
  byrow = TRUE
)

cohenH(tab1, conf.level = NA)

# should be approximately:
# pi


# ---------------------------------------------------------
# Extreme negative effect
# ---------------------------------------------------------

tab2 <- matrix(
  c(0, 100,
    100, 0),
  nrow = 2,
  byrow = TRUE
)

cohenH(tab2, conf.level = NA)

# should be approximately:
# -pi


# ---------------------------------------------------------
# Interpretation thresholds
# ---------------------------------------------------------

# small effect
tab_small <- matrix(
  c(55, 45,
    45, 55),
  nrow = 2,
  byrow = TRUE
)

cohenH(tab_small, conf.level = NA)

# should be around 0.2


# medium effect
tab_medium <- matrix(
  c(70, 30,
    30, 70),
  nrow = 2,
  byrow = TRUE
)

cohenH(tab_medium, conf.level = NA)

# should be around 0.8


# ---------------------------------------------------------
# Invalid input
# ---------------------------------------------------------

try(
  cohenH(matrix(1:9, 3, 3))
)

try(
  cohenH(1:10)
)


# ---------------------------------------------------------
# CI contains estimate
# ---------------------------------------------------------

res <- cohenH(tab, conf.level = 0.95)
stopifnot(
  res["lci"] <= res["est"], 
  res["uci"] >= res["est"]
)


# ---------------------------------------------------------
# Names
# ---------------------------------------------------------

names(cohenH(tab))

# should be:
# est lci uci