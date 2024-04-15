#' module5 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList plotOutput
mod_module5_ui <- function(id){
  ns <- NS(id)
  tagList(
    one = plotOutput(ns("one")),
    two = plotOutput(ns("two")),
    three = plotOutput(ns("three"))
  )
}

#' module5 Server Functions
#'
#' @importFrom shinipsum random_ggplot
#' @importFrom shiny renderPlot
#' @noRd
mod_module5_server <- function(id, r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$one <- renderPlot({
      random_ggplot(r$plot1)
    })

    output$two <- renderPlot({
      random_ggplot(r$plot1)
    })

    output$three <- renderPlot({
      random_ggplot(r$plot1)
    })
  })
}

## To be copied in the UI
# mod_module5_ui("module5_1")

## To be copied in the server
# mod_module5_server("module5_1")
