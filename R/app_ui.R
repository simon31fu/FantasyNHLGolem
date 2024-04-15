#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyjs
#' @importFrom bslib bs_theme font_google
#' @noRd
app_ui <- function(request) {
  # Define the header
  header <- dashboardHeader(
    title = span("NHL Bet Assistor", style = "color: black; font-size: 20px; font-family: Open Sans")
  )

  # Define the sidebar
  sidebar <- dashboardSidebar(
    collapsed = TRUE,
    useShinyjs(), # Initialize shinyjs only once here
    sidebarMenu(
      id = "tabs",
      menuItem('Shot Map',
               startExpanded = TRUE,
               menuSubItem('Team', tabName = 'team_shots'),
               menuSubItem('Arena', tabName = 'arena_shots'),
               menuSubItem('Player', tabName = 'player_shots')
      ),
      menuItem('Season Statistics', tabName = 'season'),
      menuItem('Player Performance', tabName = 'player'),
      menuItem('Team Performance', tabName = 'team')
    )
  )

  # Define the body
  body <- dashboardBody(
    tags$head(
      tags$style(HTML(".main-sidebar { font-size: 15px; }")) # Styling can be adjusted as necessary
    ),
    tabItems(
      tabItem('team_shots',
              mod_team_shots_ui("shotByTeam", team_choices = sort(unique(vF_teams_DT$long.name)),
                                allYears = c('2018','2017','2016','2015','2014'))
              ),
      tabItem('arena_shots', uiOutput('shotByArena')),
      tabItem('player_shots', uiOutput('shotByPlayer')),
      tabItem('season', uiOutput('statisticBySeason')),
      tabItem('player', uiOutput('performanceByPlayer')),
      tabItem('team', uiOutput('performanceByTeam'))
    )
  )

  # Compose and return the dashboard page
  ui <- dashboardPage(header, sidebar, body, skin = 'black')
  return(ui)
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
