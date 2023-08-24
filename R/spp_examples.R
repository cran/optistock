#' Species examples from optistocking manuscript
#'
#' This family of functions opens R scripts to run the scenarios that are used
#' in the optistocking paper. Simply call the function to open the file that
#' contains R code for the species' of interest.
#'
#' @return NULL. Opens an R script with an example
#' @export
#'
#' @name spp_examples
#'
#' @examples
#' \dontrun{
#' walleye_example()
#' }
walleye_example <- function() {
  utils::file.edit(system.file("wae_example.R", package = "optistock"))
}

#' @rdname spp_examples
#' @export
musky_example <- function() {
  utils::file.edit(system.file("mue_example.R", package = "optistock"))
}

#' @rdname spp_examples
#' @export
rainbow_trout_example <- function() {
  utils::file.edit(system.file("rbtr_example.R", package = "optistock"))
}

#' @rdname spp_examples
#' @export
chinook_example <- function() {
  utils::file.edit(system.file("chinook_example.R", package = "optistock"))
}
