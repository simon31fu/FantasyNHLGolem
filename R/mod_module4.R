#' module4 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList verbatimTextOutput
mod_module4_ui <- function(id){
  ns <- NS(id)
  tagList(
 shiny::verbatimTextOutput(ns("out"))
  )
}

#' module4 Server Functions
#'
#' @importFrom shinipsum random_print
#' @importFrom shiny renderPrint
#' @noRd
mod_module4_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$out <- shiny::renderPrint({
      shinipsum::random_print(r$type)
    })
  })
}

## To be copied in the UI
# mod_module4_ui("module4_1")

## To be copied in the server
# mod_module4_server("module4_1")
