#’ UI for exSampleR App
#’
#’ This defines the user interface for the exSampleR interactive Shiny application.
#’
#’ @return A Shiny UI object
#’ @export
#’ @import shiny
app_ui <- function() {
  fluidPage(
    tags$head(
      tags$style(HTML("
        html, body { height: 100%; margin: 0; padding: 0; }
        #app-container { height: 100vh; display: flex; flex-direction: row; }
        #sidebar-pane {
          width: 300px; min-width: 260px; overflow-y: auto;
          padding: 10px; border-right: 1px solid #ddd;
          background-color: #f9f9f9;
        }
        #main-content { flex-grow: 1; display: flex; flex-direction: column; overflow: hidden; }
        #plot-container { flex-grow: 1; overflow-y: auto; height: 100%; cursor: crosshair; }
        input[type='number']::-webkit-inner-spin-button,
        input[type='number']::-webkit-outer-spin-button { -webkit-appearance: none; margin: 0; }
        input[type='number'] { -moz-appearance: textfield; }
      "))
    ),
    div(
      id = "app-container",
      # Sidebar
      div(
        id = "sidebar-pane",
        wellPanel(
          fluidRow(
            column(12,
                   selectInput("dataset_name",  "Choose a Dataset:", choices = NULL)
            ),
            column(12,
                   selectInput("variable_name", "Choose a Numeric Variable:", choices = NULL)
            ),
            column(12,
                   selectInput("plot_type", "Plot Type:",
                               choices = list(
                                 "Histogram"         = "histogram",
                                 "QQ Plot"           = "qq_normal",
                                 "Detrended QQ Plot" = "qq_detrended",
                                 "PP Plot"           = "pp_plot"
                               ),
                               selected = "histogram"
                   )
            ),
            column(12,
                   uiOutput("band_ui")
            ),
            column(12,
                   checkboxInput("match_increment", "Match simulated data granularity", value = TRUE)
            ),
            column(12, hr()),
            column(12,
                   actionButton("generate_plots", "Generate Plots", style = "margin-top: 10px;")
            ),
            column(12,
                   uiOutput("plot_definition")
            )
          )
        )
      ),
      # Main content
      div(
        id = "main-content",
        div(
          id = "plot-container",
          plotOutput(
            "plot_grid",
            click  = "plot_click",
            height = "calc(100vh - 90px)"
          )
        ),
        div(
          style = "padding: 10px;",
          uiOutput("guess_result")
        )
      )
    )
  )
}
