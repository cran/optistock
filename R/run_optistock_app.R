#' Run Shiny app to create sandbox optistock CPF curves
#'
#' This function will open a Shiny app where you can play around with parameters
#' to see how the resulting CPF curve will change.
#'
#' @return NULL. Opens and runs the Shiny application that comes with the
#' \code{optistock} package
#'
#' @export
optistock_app <- function() {
    appDir <- system.file("optistock_app", package = "optistock")
    if (appDir == "") {
      stop("Could not find example directory. Try re-installing `optistock`.",
           call. = FALSE)
    }
    if (!requireNamespace("shiny", quietly = TRUE)) {
      stop("The shiny package is required to run this app. \n",
           "Install it with install.packages(\"shiny\")")
    }

    shiny::runApp(appDir, display.mode = "normal")
}
