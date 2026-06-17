#' Run HRDashboard shiny app
#'
#' @export
run_app <- function() {
  options(shiny.maxRequestSize = 1e3 * 1024^2)

  app_dir <- system.file("app", package = "HRDashboard")
  if (identical(app_dir, "")) {
    stop(
      "Could not find the app directory. Try re-installing `HRDashboard`.",
      call. = FALSE
    )
  }
  setwd(app_dir)

  shiny::shinyAppDir(
    appDir = ".",
    options = list(
      host = "0.0.0.0",
      port = 3000,
      devmode = TRUE
    )
  )
}
