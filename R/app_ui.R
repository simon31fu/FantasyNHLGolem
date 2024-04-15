#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme font_google
#' @noRd
shinyApp(ui=ui,server = server)
load('./data/2019-12-11_app-data.RData', envir=.GlobalEnv)
app_ui <- function() {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(
      title = "NHL Ice Hockey"
      #title = span("NHL Ice Hockey",
      #                 style = "color: black; font-size: 20px; font-family: Open Sans")
    ),
    shinydashboard::dashboardSidebar(
      # Here goes more customization
      collapsed = TRUE,
      shinyjs::useShinyjs(),
      shinydashboard::sidebarMenu(
        shinyjs::useShinyjs(),
        id = "tabs",
        shinydashboard::menuItem('Shot Map',
                                 startExpanded = TRUE,
                                 shinydashboard::menuSubItem('Team', tabName = 'team_shots'),
                                 shinydashboard::menuSubItem('Arena', tabName = 'arena_shots'),
                                 shinydashboard::menuSubItem('Player', tabName = 'player_shots')
        ),
        shinydashboard::menuItem('Season Statistics', tabName = 'season'),
        shinydashboard::menuItem('Player Performance', tabName = 'player'),
        shinydashboard::menuItem('Team Performance', tabName = 'team')
      )
    ),
    shinydashboard::dashboardBody(
      tags$head(
        tags$style(HTML(".main-sidebar { font-size: 15px; }")) #change the font size to 20
      ),
      shinydashboard::tabItems(
        shinydashboard::tabItem('team_shots', uiOutput('shotByTeam')),
        shinydashboard::tabItem('arena_shots', uiOutput('shotByArena')),
        shinydashboard::tabItem('player_shots', uiOutput('shotByPlayer')),
        shinydashboard::tabItem('season', uiOutput('statisticBySeason')),
        shinydashboard::tabItem('player', uiOutput('performanceByPlayer')),
        shinydashboard::tabItem('team', uiOutput('performanceByTeam'))
      )
    ),
    skin = 'black'
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
