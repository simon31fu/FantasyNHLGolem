#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme font_google
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = bslib::bs_theme(version = 5,
                              bg = "white",
                              fg = "black",
                              primary = "green",
                              heading_font = bslib::font_google("Fira Sans"),
                              base_font = bslib::font_google("Fira Sans"),
                              code_font = bslib::font_google("Fredoka One"),
                              "progress-bar-bg" = "lime"),
      shiny::titlePanel("Random App"),
      mod_module6_ui("module6_1"),
      # mod_module7_ui("module7_1"),
      shiny::tabsetPanel(
      shiny::tabPanel("Random 1", mod_module1_ui("module1_1")),
      shiny::tabPanel("Random 2", mod_module2_ui("module2_1")),
      shiny::tabPanel("Random 3", mod_module3_ui("module3_1")),
      shiny::tabPanel("Random 4", mod_module4_ui("module4_1")),
      shiny::tabPanel("Random 5", mod_module5_ui("module5_1"))
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Golem.proto"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
