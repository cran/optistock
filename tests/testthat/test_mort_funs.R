time <- 100
m_init <- 1
m_inf <- 0.1
alpha <- 0.01
t_scale <- 100

em_test <- exp_mort(time, m_init, m_inf, alpha, t_scale)
dm_test <- decreasing_mort(time, m_init, m_inf, alpha = 0.999)
cm_test <- constant_mort(time, 0.2)
im_test <- inv_mort(time, m_init, m_inf)
gm_test <- gaussian_mort(time, m_init, m_max = 1.5, t_scale, alpha = 10)
hgm_test <- half_gaussian_mort(
  time, m_init, m_max = 1.5, m_inf, t_scale, alpha = 10
)
lm_test <- linear_mort(time, -0.001, m_init)
pm_test <- parabolic_mort(time, m_min = m_init, 0.000001, 800, 0.0000001)

test_that("mortality function returns an accurate value at the given time", {
  expect_equal(em_test, 0.55)
  expect_equal(dm_test, 0.9143129, tolerance = 0.000001)
  expect_equal(cm_test, 0.2)
  expect_equal(im_test, 0.11)
  expect_equal(gm_test, 1.5)
  expect_equal(hgm_test, 1.5)
  expect_equal(lm_test, 0.9)
  expect_equal(pm_test, 1.49001)
})
