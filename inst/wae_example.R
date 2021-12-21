library(optistock)
library(tidyverse)

data("growth_parameters", package = "optistock")
data("cost_parameters", package = "optistock")

wae_growth_params <-
  growth_parameters %>%
  filter(spp == "walleye", source == "WDNR") %>%
  select(linf, k, t0) %>%
  as.list()
wae_cost_params <-
  cost_parameters %>%
  filter(spp == "walleye",
         (cost_fun == "total_cost" & source == "AFS") |
           (cost_fun == "linear_total_cost" & source == "WDNR"))

linf <- wae_growth_params$linf
k <- wae_growth_params$k
t0 <- wae_growth_params$t0
rec_length <- 38.1

rec_day <- inv_vb(rec_length, linf, k / 365, t0 * 365)
rec_n <- 200

# set up mortality curve parameters
exp_args <- list(m_init = 0.01, m_inf = 0.001, alpha = 0.05)
dec_args <- list(m_init = 0.02, m_inf = 0.001)
bottleneck_args <- list(
  m_inf = 0.001,
  t_scale = rec_day / 4,
  alpha = 50
)

const_mort_data <- tibble(
  mort_fun = "constant_mort",
  mort_fun_type = paste("const", c("hi", "med", "lo"), sep = "_"),
  mort_params = list(list(m = 0.001), list(m = 0.002), list(m = 0.0005))
)

dec_mort_data <- tibble(
  mort_fun = "decreasing_mort",
  mort_fun_type = paste("dec", c("hi", "med", "lo"), sep = "_"),
  mort_params = list(
    c(dec_args, list(alpha = 0.994)),
    c(dec_args, list(alpha = 0.99)),
    c(dec_args, list(alpha = 0.98))
  )
)

exp_mort_data <- tibble(
  mort_fun = "exp_mort",
  mort_fun_type = paste("exp", c("hi", "med", "lo"), sep = "_"),
  mort_params = list(
    c(exp_args, list(t_scale = rec_day * 0.3)),
    c(exp_args, list(t_scale = rec_day * 0.2)),
    c(exp_args, list(t_scale = rec_day * 0.1))
  )
)

bottleneck_mort_data <- tibble(
  mort_fun = "half_gaussian_mort",
  mort_fun_type = paste("bottleneck", c("hi", "med", "lo"), sep = "_"),
  mort_params = list(
    c(list(m_init = 0.005, m_max = 0.02), bottleneck_args),
    c(list(m_init = 0.0025, m_max = 0.01), bottleneck_args),
    c(list(m_init = 0.00125, m_max = 0.005), bottleneck_args)
  )
)

mort_fun_data <- rbind(
  const_mort_data,
  dec_mort_data,
  exp_mort_data,
  bottleneck_mort_data
)

# calculate cost-per-fish for various scenarios
cost_fun_data <- wae_cost_params

cpf_data <-
  expand_grid(mort_fun_data, cost_fun_data) %>%
  mutate(days = list(1:rec_day)) %>%
  mutate(mort = purrr::pmap(
    list(mort_fun, mort_params, days),
    function(x,y,z) {
      return(do.call(x, c(list(z), y)))
    })) %>%
  mutate(cost = purrr::pmap(
    list(cost_fun, days, cost_fun_params),
    function(x, y, z) {
      return(do.call(x, c(list(time = y, recruits = 1), z)))
    })) %>%
  mutate(cpf = purrr::pmap(
    list(days, rec_day, rec_n,
         cost_fun = cost_fun,
         cost_fun_args = cost_fun_params,
         mort_fun = mort_fun, mort_fun_args = mort_params),
    cost_per_fish
  )) %>%
  select(mort_fun, mort_fun_type, cost_fun_type,
         source, days, mort, cost, cpf) %>%
  unnest(cols = c(days, mort, cost, cpf)) %>%
  mutate(max_days = 1500) %>%
  filter(cpf > 0) %>%
  pivot_longer(mort:cpf) %>%
  mutate(name = factor(
    name,
    levels = c("mort", "cost", "cpf"),
    labels = c("M", "Cost", "CPF"))
  ) %>%
  mutate(
    # tricks ggplot into not overlapping colors and linetypes until CPF panel
    source = ifelse(name == "M", "AFS", source),
    cost_fun_type = ifelse(name == "M", "exp", cost_fun_type)
  )

# plot CPF scenario curves
cpf_plot <-
  ggplot(data = cpf_data,
       aes(x = days, y = value,
           color = mort_fun_type, linetype = source)) +
  geom_line(size = 1.2, alpha = 0.8) +
  facet_wrap(mort_fun ~ name, nrow = 4, scales = "free") +
  theme_classic() +
  xlab("Days") + ylab ("") +
  scale_linetype_discrete(
    name = "Cost Data Source",
    labels = c("AFS", "WDNR")
  ) +
  guides(color = "none")
