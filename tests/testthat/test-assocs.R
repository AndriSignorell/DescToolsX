
# ============================
# Setup
# ============================

set.seed(1)

x <- c(1,2,2,3,3,3,4,5)
y <- c(1,3,2,1,5,3,4,5)

tab <- table(x, y)

tab2 <- as.table(rbind(
  c(26,26,23,18, 9),
  c( 6, 7, 9,14,23)
))


# ============================
# 1. Basic functionality
# ============================

somersDelta(x, y)
kendallTauA(x, y)
kendallTauB(x, y)
stuartTauC(x, y)
gkGamma(x, y)


# ============================
# 2. Table interface
# ============================

somersDelta(tab)
kendallTauA(tab)
kendallTauB(tab)
stuartTauC(tab)
gkGamma(tab)


# ============================
# 3. Symmetry checks
# ============================

stopifnot(all.equal(kendallTauA(x,y), kendallTauA(y,x)))
stopifnot(all.equal(kendallTauB(x,y), kendallTauB(y,x)))
stopifnot(all.equal(stuartTauC(x,y), stuartTauC(y,x)))
stopifnot(all.equal(gkGamma(x,y), gkGamma(y,x)))

# Somers should NOT be symmetric
stopifnot(!isTRUE(all.equal(somersDelta(x,y), somersDelta(y,x))))


# ============================
# 4. Direction check (Somers)
# ============================

stopifnot(
  all.equal(
    somersDelta(x,y),
    somersDelta(y,x, direction="column")
  )
)

stopifnot(
  all.equal(
    somersDelta(tab, direction="row"),
    somersDelta(t(tab), direction="column")
  )
)


# ============================
# 5. CI sanity checks
# ============================

res <- kendallTauB(x, y, conf.level=0.95)

stopifnot(
  is.numeric(res),
  length(res) == 3,
  res[2] <= res[1],
  res[3] >= res[1],
  res[2] >= -1,
  res[3] <= 1
)


# ============================
# 6. SAS validation (very important)
# ============================

res_r <- somersDelta(tab2, direction="row", conf.level=0.95)
res_c <- somersDelta(tab2, direction="column", conf.level=0.95)

# Expected (SAS):
# D(Y|X) ≈ 0.2569
# D(X|Y) ≈ 0.4427

stopifnot(abs(res_r["est"] - 0.2569) < 1e-3)
stopifnot(abs(res_c["est"] - 0.4427) < 1e-3)


# ============================
# 7. Consistency with cor()
# ============================

stopifnot(
  abs(kendallTauB(x,y) - cor(x,y, method="kendall")) < 1e-10
)


# ============================
# 8. Edge cases
# ============================

# constant vectors
x0 <- rep(1,10)
y0 <- rep(1,10)

kendallTauB(x0, y0)   # should be NA or NaN
gkGamma(x0, y0)

# small n
kendallTauB(1:2, 2:1)



expect_equal(unname(ordAssocs(tab2, conf.level = .95)$tauB),
             unname(ordAssocs(t(tab2), conf.level = .95)$tauB))

