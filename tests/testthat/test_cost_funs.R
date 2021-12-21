
tol <- 0.000001

test_that("cost functions return appropriate ammounts for given parameters", {
  time_vec <- c(200, 400, 600)
  recruits <- 100
  init_cost <- 0.05
  time_slope <- 0.05
  time_exp <- 0.98
  rec_slope <- 0.05
  rec_exp <- 1.01
  test_costs <- cost_fun(
    time_vec, recruits, init_cost,
    time_slope, time_exp,
    rec_slope, rec_exp
  )
  test_total_cost <- total_cost(
    time_vec, recruits, init_cost,
    time_slope, time_exp,
    rec_slope, rec_exp
  )
  test_cpf <- cost_per_fish(
    time_at_stocking = time_vec, time_at_rec = 1000,
    n_recruits_desired = recruits,
    cost_fun = total_cost,
    cost_fun_args = list(
      init_cost = init_cost,
      time_slope = time_slope,
      time_exp = time_exp,
      rec_slope = rec_slope,
      rec_exp = rec_exp
    ),
    mort_fun = decreasing_mort,
    mort_fun_args = list(
      m_init = 0.3,
      m_inf = 0.01,
      alpha = 0.99
    )
  )
  expect_equal(
    test_costs,
    c(47.14225, 92.93783, 138.25643),
    tolerance = tol
  )
  expect_equal(
    test_total_cost,
    c(4766.79260872246, 18785.2177568575, 41910.735628117),
    tolerance = tol
  )
  expect_equal(
    test_cpf,
    c(7614090.18, 135465.45, 25495.43),
    tolerance = tol
  )
})
