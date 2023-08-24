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
#' \code{recruits} to time T. Use the \code{\link{total_daily_cost}} function to
#' do this automatically.
#'
#' @param time The time at which fish are raised in hatchery
#' @param recruits The number of recruits raised
#' @param daily_cost Baseline daily cost to raise a single fish
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
#' daily_cost_fun(time = 100, recruits = 1000, daily_cost = 0.05,
#'          time_slope = 0, time_exp = 1,
#'          rec_slope = 0.01, rec_exp = 1)
#' # plot a curve of instantaneous cost against time
#' curve(daily_cost_fun(x, 1000, 0.05, 0.01, 1.2, 0.05, 1), 0, 1000,
#'       xlab = "Time", ylab = "$")
#' \dontrun{
#' # 3d plot of costs by time and recruit
#' emdbook::curve3d(daily_cost_fun(x, y, 0.05, 0.01, 1.2, 0.05, 1),
#'                  from = c(0, 0),
#'                  to = c(1000, 1000),
#'                  xlab = "Time", ylab = "Recruits",
#'                  zlab = "$", sys3d = "wireframe")
#' }
daily_cost_fun <- function(time, recruits, daily_cost,
                     time_slope = 0, time_exp = 1,
                     rec_slope = 1, rec_exp = 1,
                     type = "multiplicative") {
  if (type == "multiplicative") {
    cost <-
      (time_slope * (time)^time_exp) *
      (rec_slope * (recruits)^rec_exp) +
      daily_cost
  } else if (type == "additive") {
    cost <-
      (time_slope * (time)^time_exp) +
      (rec_slope * (recruits)^rec_exp) +
      daily_cost
  } else {
    stop("type must be either 'additive' or 'multiplicative'")
  }
  return(cost)
}

#' Compute the total daily cost of raising hatchery fish
#'
#' This function takes the definite integral from time t = 0 until the
#' given \code{time} of the \code{\link{daily_cost_fun}}.
#' This integral is then the total cost of raising x number of fish until
#' \code{time} given the other cost function parameters.
#'
#' @inheritParams daily_cost_fun
#' @param init_cost An intercept on the total cost function
#'
#' @return The total cost across time to raise the number of \code{recruits}.
#' This is simply the integral from time t = 0 until \code{time} of the
#' \code{\link{daily_cost_fun}} function.
#' @export
#'
#' @examples
#' # total cost of raising 1000 fish for 100 days at given parameters
#' total_daily_cost(time = 100, recruits = 100,
#'            daily_cost = 0.05,
#'            time_slope = 0.01, time_exp = 1.2,
#'            rec_slope = 0.05, rec_exp = 1)
total_daily_cost <- function(time, recruits,
                       daily_cost, init_cost = 0,
                       time_slope = 0, time_exp = 1,
                       rec_slope = 1, rec_exp = 1, type = "multiplicative") {
  out <- lapply(1:length(time), function(x) {
    if (length(recruits) > 1) {
      if (length(time) != length(recruits)) {
        stop("Recruits must either be length 1 or the same length as time")
      }
      cost_integral <- integrate(
        daily_cost_fun, lower = 1e-10, upper = time[x],
        recruits = recruits[x], daily_cost = daily_cost,
        time_slope = time_slope, time_exp = time_exp,
        rec_slope = rec_slope, rec_exp = rec_exp,
        type = type
      )
      out <- cost_integral$value + init_cost
      return(out)

    } else {
      cost_integral <- integrate(
        daily_cost_fun, lower = 1e-10, upper = time[x],
        recruits = recruits, daily_cost = daily_cost,
        time_slope = time_slope, time_exp = time_exp,
        rec_slope = rec_slope, rec_exp = rec_exp,
        type = type
      )
      out <- cost_integral$value + init_cost
      return(out)
    }
  })
  return(unlist(out))
}

#' Compute direct total cost to raise hatchery fish
#'
#' This function computes the total cost to raise fish in a hatchery until
#' \code{time}. This function differs from \code{\link{total_daily_cost}} by
#' directly computing the total cost rather than integrating a daily cost
#' estimate.
#'
#' The \code{total_cost} function computes a cost curve according to the
#' following equation:
#'
#' \ifelse{html}{
#'  \out{<i>C = s<sub>T</sub>t<sup>&alpha;</sup>
#'       * s<sub>2</sub>R<sup>&beta;</sup> + b</i>}}{
#'  \deqn{C = \alpha * T ^ \gamma + \beta + R^\tau}
#' }
#'
#' where \ifelse{html}{\out{&alpha;}}{\eqn{\alpha}} corresponds to the
#' \code{time_slope} argument,
#' \ifelse{html}{\out{&gamma;}}{\eqn{\gamma}} is the \code{time_exp} parameter,
#' \ifelse{html}{\out{&beta;}}{\eqn{\beta}} is the intercept (or
#' \code{init_cost}),
#' R is the number of recruits, and
#' \ifelse{html}{\out{&tau;}}{\eqn{\tau}} is the recruitment exponent
#' corresponding to \code{rec_exp}
#'
#'
#'
#' @param time The amount of time that fish are raised in hatchery
#' @param time_slope Controls how quickly the slope increases over time
#' @param time_exp Controls the non-linearity of the curve over time
#' @param init_cost The initial cost (i.e. intercept of the curve)
#' @param recruits The number of recruits
#' @param rec_exp Controls the non-linearity of the curve across recruit number
#'
#' @return A vector of values representing cost for the given time, recruit
#' number, and associated variables
#' @export
#'
#' @examples
#' curve(total_cost(x, time_slope = 0.05, time_exp = 1.2), 0, 100)
#' curve(total_cost(x, time_slope = 0.05, time_exp = 0.5), 0, 100)
total_cost <- function(time, time_slope = 1, time_exp = 1, init_cost = 0,
                       recruits = 1, rec_exp = 1) {
  out <- ((time_slope * time^time_exp) + init_cost) * (recruits^rec_exp)
  return(out)
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
#' cost_args <- list(
#'     init_cost = 0.05,
#'     time_slope = 0.01, time_exp = 1.2,
#'     rec_exp = 1
#' )
#' mort_args <- list(m_init = (1 / 365))
#' # the cost-per-fish to stock across a range of times given cost and mortality
#' # assumes fish recruit into the fishery at day 1000
#' curve(cost_per_fish(
#'     x, 1000, 1000,
#'     cost_fun_args = cost_args,
#'     mort_fun_args = mort_args),
#'   xlab = "Days", ylab = "$ per fish stocked",
#'   10, 1200
#' )
cost_per_fish <- function(time_at_stocking, time_at_rec, n_recruits_desired,
                          cost_fun = total_cost, cost_fun_args,
                          mort_fun = constant_mort, mort_fun_args) {
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
