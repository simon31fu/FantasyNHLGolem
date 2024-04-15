#' module6 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_module6_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' module6 Server Functions
#'
#' @noRd 
mod_module6_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_module6_ui("module6_1")
    
## To be copied in the server
# mod_module6_server("module6_1")
