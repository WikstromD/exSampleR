#' Launches exsampler
#'
#' This function launches the exsampler interactive Shiny app.
#'
#' @return Runs the Shiny app.
#' @export
#'
#' @import shiny
run_app <- function() {
  shinyApp(ui = app_ui(), server = app_server)
}
