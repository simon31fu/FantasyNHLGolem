#' module8 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_module8_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' module8 Server Functions
#'
#' @noRd 
mod_module8_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_module8_ui("module8_1")
    
## To be copied in the server
# mod_module8_server("module8_1")
