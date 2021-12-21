
test_that("growth functions compute length and time correctly", {
  time <- 100
  linf <- 125
  k <- 0.15
  t0 <- -0.5
  len <- linf * (1 - exp(-k * (time - t0)))
  expect_equal(vbgf(time, linf, k, t0), len)
  expect_equal(inv_vb(len, linf, k, t0), time)
})
