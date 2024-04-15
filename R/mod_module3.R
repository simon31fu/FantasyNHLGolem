#' module3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom DT DTOutput
mod_module3_ui <- function(id){
  ns <- NS(id)
  tagList(
  DT::DTOutput(ns("dt"))
  )
}

#' module3 Server Functions
#'
#' @importFrom shinipsum random_DT
#' @importFrom DT renderDT
#' @noRd
mod_module3_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$dt <- DT::renderDT({
      shinipsum::random_DT(r$num, 5)
    })
  })
}

## To be copied in the UI
# mod_module3_ui("module3_1")

## To be copied in the server
# mod_module3_server("module3_1")
