#' module7 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_module7_ui <- function(id){
  ns <- NS(id)
  tagList(
 shiny::numericInput("num",
                     "Select Number of Words:",
                     value = 300)
  )
}

#' module7 Server Functions
#'
#' @noRd
mod_module7_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    shiny::observeEvent(input$num, {

    r$number <- as.numeric(input$num)

    })

  })
}

## To be copied in the UI
# mod_module7_ui("module7_1")

## To be copied in the server
# mod_module7_server("module7_1")
