#' @export
runExample <- function() {
  appDir <- system.file("shinyApp", package = "randomizeIt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  #
  # library(shiny)
  # library(shinydashboard)
  #
  # library(dplyr)
  # library(dipsaus)
  # library(mosaic)

  shiny::runApp(appDir, display.mode = "normal")
}
