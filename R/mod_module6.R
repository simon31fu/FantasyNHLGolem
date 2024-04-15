#' module6 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList radioButtons
mod_module6_ui <- function(id){
  ns <- NS(id)
  opts <- c("1", "2", "3", "4")
  tagList(
shiny::radioButtons(ns("options"),
                    "Select Option",
                    choices = opts,
                    selected = "1",
                    inline = FALSE)
  )
}

#' module6 Server Functions
#'
#' @importFrom shiny observeEvent
#' @noRd
mod_module6_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    shiny::observeEvent(input$options, {

    r$options <- input$options

    if (input$options == "1") {r$plot1 <- "raster"}
    if (input$options == "1") {r$num <- 100}
    if (input$options == "1") {r$type <- "character"}
    if (input$options == "2") {r$plot1 <- "ribbon"}
    if (input$options == "2") {r$num <- 200}
    if (input$options == "2") {r$type <- "numeric"}
    if (input$options == "3") {r$plot1 <- "tile"}
    if (input$options == "3") {r$num <- 300}
    if (input$options == "3") {r$type <- "integer"}
    if (input$options == "4") {r$plot1 <- "violin"}
    if (input$options == "4") {r$num <- 400}
    if (input$options == "4") {r$type <- "model"}

    })

  })
}

## To be copied in the UI
# mod_module6_ui("module6_1")

## To be copied in the server
# mod_module6_server("module6_1")
