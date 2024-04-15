#' module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList plotOutput
mod_module1_ui <- function(id){
  ns <- NS(id)
  tagList(
  plotOutput(ns("plot"))
  )
}

#' module1 Server Functions
#'
#' @importFrom shinipsum random_ggplot
#' @importFrom shiny renderPlot
#' @noRd
mod_module1_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 output$plot <- renderPlot({
   shinipsum::random_ggplot(r$plot1)
 })
  })
}

## To be copied in the UI
# mod_module1_ui("module1_1")

## To be copied in the server
# mod_module1_server("module1_1")
