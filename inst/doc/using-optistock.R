## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(rmarkdown.html_vignette.check_title = FALSE)

## ----setup, echo = FALSE------------------------------------------------------
library(optistock)

## ---- cpf_curve, fig.height = 4, fig.width = 7, echo = FALSE------------------
curve(
  cost_per_fish(
    time_at_stocking = x,
    time_at_rec = 1000,
    n_recruits_desired = 200,
    cost_fun = linear_total_cost,
    cost_fun_args = list(int = 20, beta = 0.01),
    mort_fun = exp_mort,
    mort_fun_args = list(
      m_init = 0.001, m_inf = 0.0001, alpha = 0.05, t_scale = 200
    )
  ),
  0, 1000,
  xlab = "Time (days)", ylab = "Cost-per-fish ($)"
)

## ---- cpf_curve_code, fig.height = 4, fig.width = 7, eval = FALSE-------------
#  curve(
#    cost_per_fish(
#      time_at_stocking = x,
#      time_at_rec = 1000,
#      n_recruits_desired = 200,
#      cost_fun = linear_total_cost,
#      cost_fun_args = list(int = 20, beta = 0.01),
#      mort_fun = exp_mort,
#      mort_fun_args = list(
#        m_init = 0.001, m_inf = 0.0001, alpha = 0.05, t_scale = 200
#      )
#    ),
#    0, 1000,
#    xlab = "Time (days)", ylab = "Cost-per-fish ($)"
#  )

## ---- fig.height = 4, fig.width = 7-------------------------------------------
time <- 1:1000
linf <- 50
k <- 0.4 / 365
t0 <- -0.5 * 365
len_at_age <- vbgf(time, linf, k, t0)
plot(len_at_age ~ time, type = "l", xlab = "Age (days)", ylab = "Length")

## ---- fig.height = 4, fig.width = 7-------------------------------------------
age_at_len <- inv_vb(len_at_age, linf, k, t0)
plot(age_at_len ~ len_at_age, type = "l", xlab = "Length", ylab = "Age (days)")

## ---- mortality_curves, fig.height = 4, fig.width = 7-------------------------
curve(
  exp_mort(x, m_init = 0.5, m_inf = 0.05, alpha = 0.05, t_scale = 200), 
  0, 1000,
  xlab = "Time (days)", ylab = "M"
)
curve(
  decreasing_mort(x, m_init = 0.5, m_inf = 0.05, alpha = 0.99), 
  0, 1000,
  xlab = "Time (days)", ylab = "M"
)

## ---- linear_total_cost, fig.height = 4, fig.width = 7------------------------
recruits <- 1
int <- 1.2
beta <- 0.05
curve(
  linear_total_cost(x, recruits, int, beta), 
  0, 1000, 
  xlab = "Time (days)", ylab = "Cost ($)"
)

## ---- total_daily_cost, fig.height = 4, fig.width = 7-------------------------
init_cost <- 0.01
time_slope <- 0.01
time_exp <- 1.05
rec_slope <- 1
rec_exp <- 1
curve(
  total_daily_cost(x, recruits, init_cost, time_slope, time_exp, rec_slope, rec_exp),
  0, 1000,
  xlab = "Time (days)", ylab = "Cost ($)"
)

