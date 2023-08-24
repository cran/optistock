#' Growth parameters for species used in examples
#'
#' This data.frame contains the growth parameters used in
#' \code{\link{spp_examples}}. The growth parameters correspond with the von
#' Bertalanfy growth curve (VBGF -- see \code{\link{vbgf}})
#'
#' @format A data.frame with 7 fields and 6 records:
#' \describe{
#'   \item{spp}{Species common name}
#'   \item{latin}{Scientific name for the species}
#'   \item{linf}{The L_infinity parameters for the VBGF}
#'   \item{k}{The k parameter for the VBGF}
#'   \item{t0}{The t_0 parameter for the VBGF}
#'   \item{n}{Number of samples used. For WDNR data this is the number of paired
#'     length-at-age data points (WDNR, 2021).
#'     For FishBase it is the number of submitted entries.
#'   }
#'   \item{source}{Where data was retrieved from. WDNR is the Wisconsin Dep't
#'     of Natural Resources Fisheries Management Database.
#'     FishBase (Froese and Pauly, 2010) is FishBase.
#'   }
#' }
"growth_parameters"

#' Cost parameters for species used in examples
#'
#' This data.frame contains the cost parameters used in
#' \code{\link{spp_examples}}. These parameters correspond with the
#' \code{\link{total_cost}} and \code{\link{linear_total_cost}} functions.
#'
#' @format A \code{\link[tibble]{tibble}} of  variables and 16 records
#' \describe{
#'   \item{spp}{Species common name}
#'   \item{source}{The source of the data. WDNR is from Wisconsin Dep't of
#'     Natural Resources Hatchery cost data. AFS is the
#'     American Fisheries Society Special Publication 35 on Fishkill Replacement
#'     costs
#'   }
#'   \item{cost_fun_type}{Either "exp" for exponential -- corresponds to the
#'     \code{\link{total_cost}} cost function, or "linear" -- corresponds to the
#'     \code{\link{linear_total_cost}} cost function.
#'   }
#'   \item{cost_fun}{The cost function -- either
#'     \code{\link{total_cost}} or \code{\link{linear_total_cost}}
#'   }
#'   \item{cost_fun_params}{A list-col of the parameters necessary for
#'     the respective cost function found in \code{cost_fun}
#'   }
#' }
"cost_parameters"
