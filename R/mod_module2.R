#' module2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList textOutput
mod_module2_ui <- function(id){
  ns <- NS(id)
  tagList(
  shiny::textOutput(ns("text"))
  )
}

#' module2 Server Functions
#'
#' @importFrom shinipsum random_text
#' @importFrom shiny renderText
#' @noRd
mod_module2_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

 output$text <- shiny::renderText({
   shinipsum::random_text(nwords = r$num)
 })


  })
}

## To be copied in the UI
# mod_module2_ui("module2_1")

## To be copied in the server
# mod_module2_server("module2_1")
