#' Compute the instantaneous cost of raising hatchery fish
#'
#' This is a multivariable function of both time and number of recruits raised.
#' Cost-per-time and cost-per-recruit can be calculated as a quadratic
#' where the slope and exponent can be specified.
#'
#' The cost-per-fish based on time and number of recruits uses the function:
#'
#' \ifelse{html}{
#'  \out{<i>C = s<sub>T</sub>t<sup>&alpha;</sup>
#'       * s<sub>2</sub>R<sup>&beta;</sup> + b</i>}}{
#'  \deqn{C = s_1 T^{\alpha} \cdot s_2 R^{\beta} + b}
#' }
#'
#' if type = "multiplicative". Otherwise it uses:
#'
#' \ifelse{html}{
#'  \out{<i>C = s<sub>T</sub>t<sup>&alpha;</sup>
#'       + s<sub>2</sub>R<sup>&beta;</sup> + b</i>}}{
#'  \deqn{C = s_1 T^{\alpha} + s_2 R^{\beta} + b}
#' }
#'
#' if type = "additive"
#'
#' where C = the cost to rear R number of recruits at time T,
#' the s values are the slopes, \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}} and
#' \ifelse{html}{\out{&beta;}}{\eqn{\beta}}, are the exponents on time (T) and
#' recruits (R), respectively, and b is the intercept.
#' The instantaneous cost is really what is of interest, and the number of
#' recruits essentially adjusts the intercept on that dimension of the equation.
#'
#' Increasing the exponent will dramatically increase the cost of raising
#' hatchery fish as time goes on. Increasing the exponent dramatically
#' increases the cost of raising a greater number of fish. Integrating this
#' equation across time will compute the total cost to raise the number of
#' \code{recruits} to time T. Use the \code{\link{total_cost}} function to
#' do this automatically.
#'
#' @param time The time at which fish are raised in hatchery
#' @param recruits The number of recruits raised
#' @param init_cost Baseline initial cost to raise a single fish
#' @param time_slope The slope term on the amount of time (see details)
#' @param time_exp The exponent on the amount of time
#' @param rec_slope The slope term on the number of recruits
#' @param rec_exp The exponent on the number of recruits
#' @param type Either multiply the number of recruits times the cost-at-time or
#' add to it (see Details).
#'
#' @return A numeric value representing the cost of rearing the number of
#' \code{recruits} at a given time and given the number of recruits raised
#' @export
#'
#' @examples
#' # compute the instantaneous cost of raising 1000 fish on day 100
#' cost_fun(time = 100, recruits = 1000, init_cost = 0.05,
#'          time_slope = 0, time_exp = 1,
#'          rec_slope = 0.01, rec_exp = 1)
#' # plot a curve of instantaneous cost against time
#' curve(cost_fun(x, 1000, 0.05, 0.01, 1.2, 0.05, 1), 0, 1000,
#'       xlab = "Time", ylab = "$")
#' \dontrun{
#' # 3d plot of costs by time and recruit
#' emdbook::curve3d(cost_fun(x, y, 0.05, 0.01, 1.2, 0.05, 1),
#'                  from = c(0, 0),
#'                  to = c(1000, 1000),
#'                  xlab = "Time", ylab = "Recruits",
#'                  zlab = "$", sys3d = "wireframe")
#' }
cost_fun <- function(time, recruits, init_cost,
                     time_slope = 0, time_exp = 1,
                     rec_slope = 1, rec_exp = 1,
                     type = "multiplicative") {
  if (type == "multiplicative") {
    cost <-
      (time_slope * (time)^time_exp) *
      (rec_slope * (recruits)^rec_exp) +
      init_cost
  } else if (type == "additive") {
    cost <-
      (time_slope * (time)^time_exp) +
      (rec_slope * (recruits)^rec_exp) +
      init_cost
  } else {
    stop("type must be either 'additive' or 'multiplicative'")
  }
  return(cost)
}

#' Compute the total cost of raising a certain number of fish until a give time
#'
#' This function takes the definite integral from time t = 0 until the
#' given \code{time} of the \code{\link{cost_fun}}.
#' This integral is then the total cost of raising x number of fish until
#' \code{time} given the other cost function parameters.
#'
#' @inheritParams cost_fun
#'
#' @return The total cost across time to raise the number of \code{recruits}.
#' This is simply the integral from time t = 0 until \code{time} of the
#' \code{\link{cost_fun}} function.
#' @export
#'
#' @examples
#' # total cost of raising 1000 fish for 100 days at given parameters
#' total_cost(time = 100, recruits = 100,
#'            init_cost = 0.05,
#'            time_slope = 0.01, time_exp = 1.2,
#'            rec_slope = 0.05, rec_exp = 1)
total_cost <- function(time, recruits, init_cost,
                       time_slope, time_exp,
                       rec_slope, rec_exp, type = "multiplicative") {
  out <- lapply(1:length(time), function(x) {
    if (length(recruits) > 1) {
      if (length(time) != length(recruits)) {
        stop("Recruits must either be length 1 or the same length as time")
      }
      return(integrate(cost_fun, lower = 1e-10, upper = time[x],
                       recruits = recruits[x], init_cost = init_cost,
                       time_slope = time_slope, time_exp = time_exp,
                       rec_slope = rec_slope, rec_exp = rec_exp,
                       type = type)$value)

    } else {
      return(integrate(cost_fun, lower = 1e-10, upper = time[x],
                       recruits = recruits, init_cost = init_cost,
                       time_slope = time_slope, time_exp = time_exp,
                       rec_slope = rec_slope, rec_exp = rec_exp,
                       type = type)$value)
    }
  })
  return(unlist(out))
}

#' Compute total cost as a linear function of time
#'
#' This function returns the total cost of raising n \code{recruits} to
#' \code{time}. The curve across \code{time} can only be linear
#' with parameters \code{int} and \code{beta}, but can be non-linear with
#' respect to \code{recruits}
#'
#' @param time The amount of time that fish are raised in hatchery
#' @param int Intercept for the linear total cost curve
#' @param beta Slope for the linear total cost curve
#' @param recruits The number of recruits raised
#' @param rec_exp The exponent on the number of recruits
#'
#' @return A vector the same length as \code{time} with the total cost to raise
#' n \code{recruits} to \code{time}
#' @export
#'
#' @examples
#' curve(linear_total_cost(x, 0.5, 0.001, 100), 0, 1000)
linear_total_cost <- function(time, recruits, int, beta, rec_exp=1) {
  out <- (int + (beta * time)) * (recruits^rec_exp)
  return(out)
}


#' Compute the per-cost fish based on stocking time, time to recruitment,
#' growth, and mortality
#'
#' @param time_at_stocking The time at which fish are stocked (i.e. synonymous
#' with the amount of time that fish are raised in a hatchery)
#' @param time_at_rec The time at which a fish enters the fishery (i.e. the
#' amount of time it takes a fish to grow to a desired length). Use
#' \code{\link{inv_vb}} to calculate this.
#' @param n_recruits_desired The number of recruits desired at
#' \code{time_at_rec}
#' @param mort_fun The mortality function, see ?mort_funs
#' @param mort_fun_args List. Named arguments to be passed to \code{mort_fun}
#' @param cost_fun The cost function. Defaults to \code{\link{total_cost}}
#' @param cost_fun_args Arguments for \code{cost_fun}
#'
#' @return The per-fish cost fish that lives until \code{time_at_rec} based on
#' \code{time_at_stocking}, the cost function and mortality functions.
#' @export
#'
#' @examples
#' cost_args <-
#'   list(
#'     init_cost = 0.05,
#'     time_slope = 0.01, time_exp = 1.2,
#'     rec_slope = 0.01, rec_exp = 1)
#' mort_args <- list(m_init = (1 / 365), m_inf = (0.2 / 365), alpha = 0.0001)
#' # the cost-per-fish to stock across a range of times given cost and mortality
#' # assumes fish recruit into the fishery at day 1000
#' curve(cost_per_fish(x, 1000, 1000,
#'                     cost_fun_args = cost_args,
#'                     mort_fun_args = mort_args),
#'       xlab = "Days", ylab = "$ per fish stocked",
#'       10, 1200)
cost_per_fish <- function(time_at_stocking, time_at_rec, n_recruits_desired,
                          cost_fun = total_cost, cost_fun_args,
                          mort_fun = exp_mort, mort_fun_args) {
  cost_per_recruit <- lapply(time_at_stocking, function(x) {
    n_fish_to_stock <- n_to_stock(time_at_stocking = x,
                                  time_at_rec = time_at_rec,
                                  n_recruits_desired = n_recruits_desired,
                                  mort_fun = mort_fun,
                                  mort_fun_args = mort_fun_args)
    cost_fun_args <- c(time = x, recruits = n_fish_to_stock, cost_fun_args)
    total <- do.call(cost_fun, cost_fun_args)
    out <- total / n_recruits_desired
    return(out)
  })
  return(unlist(cost_per_recruit))
}
