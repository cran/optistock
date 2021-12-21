#' Basic von Bertalanffy growth function (VBGF)
#'
#' @param time Time at which to calculate size
#' @param linf The \ifelse{html}{\out{L<sub>&infin;</sub>}}{\eqn{L_\infty}}
#' parameter of the VBGF
#' @param k The k parameter of the VBGF
#' @param t0 The \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}}
#'
#' @return A numeric vector of lengths given the age (or amount of time) and
#' parameters
#' @export
#'
#' @examples
#' curve(vbgf(x, 30, 0.25, -0.2), 0, 10)
#' curve(vbgf(x, 30, (0.25 / 365), -0.2), 0, 10 * 365)
vbgf <- function(time, linf, k, t0) {
  out <- linf * (1 - exp(-k * (time - t0)))
  return(out)
}

#' The inverse von Bertalanffy function (iVBGF)
#'
#' This function calculates the inverse of the VBGF, or, time it takes to grow
#' to a particular length
#'
#' @param len Numeric. A length at which to determine how long it takes to grow
#' @param linf The \ifelse{html}{\out{L<sub>&infin;</sub>}}{\eqn{L_\infty}}
#' parameter of the VBGF
#' @param k The k parameter of the VBGF
#' @param t0 The \ifelse{html}{\out{t<sub>0</sub>}}{\eqn{t_0}}
#'
#' @return A numeric vector of how long it takes to grow to length \code{len}
#' @export
#'
#' @examples
#' time <- 365
#' len_at_age <- vbgf(time, 30, (0.2 / 365), -0.2)
#' inv_vb(len_at_age, 30, (0.2/365), -0.2)
inv_vb <- function(len, linf, k, t0) {
  return((log(((len / linf) - 1) / (-1)) / -k) + t0)
}
