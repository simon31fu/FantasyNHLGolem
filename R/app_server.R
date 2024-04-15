#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  r <- reactiveValues()

  # Your application server logic
  mod_module1_server("module1_1", r = r)
  mod_module2_server("module2_1", r = r)
  mod_module3_server("module3_1", r = r)
  mod_module4_server("module4_1", r = r)
}

