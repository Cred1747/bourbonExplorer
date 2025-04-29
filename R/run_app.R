# bourbonExplorer/R/run_app.R

#' Launch the Bourbon Explorer Shiny App
#'
#' This function starts the interactive Shiny app included in the package.
#'
#' @export
run_bourbon_app <- function() {
  appDir <- system.file("shiny", package = "bourbonExplorer")

  # Error handling if directory not found
  if (appDir == "") {
    stop("Shiny app directory not found. Re-install the package.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
