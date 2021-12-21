#' Functions to produce mortality curves
#'
#' This family of functions produce different shapes of mortality curves across
#' time
#'
#' These functions produced different shapes of mortality curves that are
#' commonly found in fisheries. Some of the more common are
#' \code{constant_mort} (which returns constant mortality across time),
#' \code{exp_mort} (S-shaped decreasing curve), and \code{decreasing_mort}
#' (non-linear decreasing curve). Others are less common and represent specific
#' scenarios such as \code{gaussian_mort} (implemented to represent a
#' bottleneck).
#'
#' @param time The time to calculate mortality at
#' @param m_init Initial rate of mortality at time 0 (or time t for
#' \code{constant_mort})
#' @param m_inf Final rate of mortality as time approaches infinity
#' @param alpha The rate at which mortality decreases across time
#' @param t_scale A horizontal scaling parameter
#'
#' @return A vector of numeric values for mortality rate at \code{time}
#' @export
#'
#' @name mort_funs
#'
#' @examples
#' # an example in years
#' curve(exp_mort(x, 0.2, 0.1, 0.25), 0, 20)
#' # an example in days
#' curve(exp_mort(x, (1 / 365), (0.2 / 365), 0.005), 0, 1000)
exp_mort <- function(time, m_init, m_inf, alpha, t_scale = NULL) {
  if (is.null(t_scale)) {
    t_scale <- -(log((1 / (m_init - m_inf)) - 1) / alpha)
  }
  out <- ((m_init - m_inf) / (1 + exp(alpha * (time - t_scale)))) + m_inf
  return(out)
}

#' @rdname mort_funs
#' @export
decreasing_mort <- function(time, m_init, m_inf, alpha) {
  if (alpha > 1 || alpha < 0) {
    stop("alpha parameter must be between 0 and 1")
  }
  out <- (m_init - m_inf) * (alpha^time) + m_inf
  return(out)
}

#' @rdname mort_funs
#' @export
constant_mort <- function(time, m_init) {
  out <- vapply(time, function(x) {return(m_init)}, numeric(1))
  return(out)
}

#' @rdname mort_funs
#' @export
inv_mort <- function(time, m_init, m_inf) {
  return(((m_init / time)) + m_inf)
}

#' @rdname mort_funs
#' @param m_max The maximum mortality that is achieved at \code{time = t_scale}
#' @export
gaussian_mort <- function(time, m_init, m_max, t_scale, alpha) {
  out <-
    ((m_max - m_init) * exp(-(((time - t_scale)^2)) / (2 * (alpha^2)))) + m_init
  return(out)
}

#' @rdname mort_funs
#' @export
half_gaussian_mort <- function(time, m_init, m_max, m_inf, t_scale, alpha) {
  out <- lapply(time, function(x) {
    if (x <= t_scale) {
      out <-
        ((m_max - m_init) * exp(-(((x - t_scale)^2)) / (2 * (alpha^2)))) +
        m_init
    } else {
      out <-
        ((m_max - m_inf) * exp(-(((x - t_scale)^2)) / (2 * (alpha^2)))) +
        m_inf
    }
  })
  return(unlist(out))
}

#' @rdname mort_funs
#' @export
linear_mort <- function(time, alpha, m_init) {
  return((time * alpha) + m_init)
}

#' @rdname mort_funs
#' @param m_min The lowest mortality that the curve should reach
#' @param beta Slope on the quadratic term for \code{parabolic_mort}
#' @export
parabolic_mort <- function(time, m_min, alpha, t_scale, beta) {
  out <- (alpha * ((time - t_scale)^2)) + (beta * time) + m_min
  return(out)
}


#' Calculate the number of recruits left after given time based on mortality
#'
#' This function will use the provided mortality function and parameters along
#' with the length of time from stocking until the time in question to determine
#' how many fish will be left at that time (i.e. how many fish die between
#' \code{time_at_stocking} and \code{time_at_rec}).
#'
#' This function calculates how many fish are left at a certain time based on
#' the initial number of fish stocked and the integral of the mortality
#' function. The number of fish left are computed using the following equation:
#'
#' \ifelse{html}{
#'   \out{N<sub>t</sub> = N<sub>0</sub> exp(&int; f(t))}
#' }{
#'   \deqn{N_t = N_0 \exp{\int_{T=0}^{t}f(t)}}
#' }
#'
#' where \ifelse{html}{\out{N<sub>0</sub>}}{\deqn{N_0}} is the initial number of fish
#' stocked and f(t) is the mortality function. The amount of time is provided
#' to the function as the time at which fish are recruited into the fishery
#' minus the time at which they are stocked. The time at which fish are
#' recruited into the fishery can be calculated using the inverse von
#' Bertalanffy growth function (see \code{\link{inv_vb}}).
#'
#'
#' @param time_at_stocking The day that fish are stocked (i.e. synonymous with
#' the amount of time that fish are raised in a hatchery)
#' @param time_at_rec The time at which a fish enters the fishery (i.e. the
#' amount of time it takes a fish to grow to a desired length). Use
#' \code{\link{inv_vb}} to calculate this.
#' @param fish_init The initial number of fish stocked
#' @param mort_fun The mortality function, see ?mort_funs
#' @param mort_fun_args List. Named arguments to be passed to \code{mort_fun}
#'
#' @return The number of fish that will be left given the mortality function,
#' its parameters, and the time (\code{time_at_rec} - \code{time_at_stocking})
#' @export
#'
#' @examples
#' mort_args <- list(
#'   m_init = (1 / 365),
#'   m_inf = (0.2/365),
#'   alpha = 0.005
#' )
#' recruits_at_time(100, 1000, 1000,
#'                  mort_fun = exp_mort,
#'                  mort_fun_args = mort_args)
recruits_at_time <- function(time_at_stocking, time_at_rec, fish_init,
                             mort_fun = exp_mort, mort_fun_args) {
  out <- lapply(time_at_stocking, function(x) {
    if (x > time_at_rec) {
      return(fish_init)
    } else {
      mort_fun_args <- c(f = mort_fun,
                         lower = x,
                         upper = time_at_rec,
                         mort_fun_args)
      total_m <- do.call("integrate", mort_fun_args)
      return(fish_init * exp(-total_m$value))
    }
  })
  return(unlist(out))
}

#' Calculate the number of fish to stock based on desired recruit number
#' and given mortality curve
#'
#' This function is essentially the inverse of \code{\link{recruits_at_time}}.
#' Given the number of fish desired at a certain time and the mortality
#' function and parameters this function will calculate how many fish should be
#' stocked into a system.
#'
#' @param time_at_stocking The time that fish are stocked (i.e. synonymous with
#' the amount of time that fish are raised in a hatchery)
#' @param time_at_rec The time at which a fish enters the fishery (i.e. the
#' amount of time it takes a fish to grow to a desired length). Use
#' \code{\link{inv_vb}} to calculate this.
#' @param n_recruits_desired The number of recruits desired at
#' \code{time_at_rec}
#' @param mort_fun The mortality function, see ?mort_funs
#' @param mort_fun_args List. Named arguments to be passed to \code{mort_fun}
#'
#' @return The number of fish to be stocked at \code{time_at_stocking} to
#' get the desired number of fish at \code{time_at_rec} based on the mortality
#' function and associated parameters
#' @export
#'
#' @examples
#' # how many fish to stock on day 100 if you want 10000 fish on day 1000
#' n_to_stock(10000, 100, 1000,
#'            mort_fun = exp_mort,
#'            mort_fun_args = list(m_init = (1 / 365),
#'                                 m_inf = (0.2/365),
#'                                 alpha = 0.005))
n_to_stock <- function(time_at_stocking, time_at_rec, n_recruits_desired,
                       mort_fun = exp_mort, mort_fun_args) {
  out <- lapply(time_at_stocking, function(x) {
    total_mort <- do.call("integrate",
                          c(mort_fun, lower = x,
                            upper = time_at_rec, mort_fun_args))
    return(n_recruits_desired / exp(-total_mort$value))
  })
  return(unlist(out))
}
