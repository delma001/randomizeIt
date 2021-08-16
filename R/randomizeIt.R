#' @export
rondomizeIt <- function() {
  appDir <- system.file("shiny-examples", "randomizeIt", package = "randomizeIt")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
