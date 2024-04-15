mod_team_shots_ui <- function(id,team_choices,allYears){
  ns <- NS(id)
  tagList(

    fluidPage(theme = shinythemes::shinytheme("spacelab"),
              fluidRow(
                align='center',
                #collapsible box for main inputs
                box(solidHeader = F, status = 'primary', collapsible = T, width = '100%',
                    title = "Filters",
                    #input selections are inside div so we can place left and right inputs side by side
                    div(style="display: inline-block;vertical-align:top; width: 30% ; margin-bottom: 0em;",
                        selectInput(ns('leftTeam'), 'Team 1', choices = team_choices, selected = 'Boston Bruins', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 15% ; margin-bottom: 0em;",
                        radioButtons(ns('leftHome'), '', choices = c('Home','Away'), selected = 'Home',
                                     inline = FALSE, width = NULL, choiceNames = NULL,
                                     choiceValues = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 30%; margin-bottom: 0em;",
                        selectInput(ns('rightTeam'), 'Team 2', choices = team_choices, selected = 'New York Rangers', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 15% ; margin-bottom: 0em;",
                        radioButtons(ns('rightHome'), '', choices = c('Home','Away'), selected = 'Home',
                                     inline = FALSE, width = NULL, choiceNames = NULL,
                                     choiceValues = NULL)),
                    div(style="display: inline-block;vertical-align:top; width: 20%; margin-top: 0em;",
                        selectInput(ns('year'), 'Season', choices = allYears, selected = '2018', multiple = FALSE,
                                    selectize = TRUE, width = NULL, size = NULL))
                )
              ),
              fluidRow(
                align = "center",
                box(solidHeader = F, status = 'primary', title = "Rink Plot", width = '100%', textOutput(ns("teamText")),
                    plotly::plotlyOutput(ns("icemap_team")))
              )
    )

  )
}

mod_team_shots_server <- function(id,main_session,image_file){
  shiny::moduleServer(id, function(input, output, session){
    ns <- session$ns

    ## year
    selected_year <- shiny::reactive({
      return(input$year)
    })

    ## team
    left_team_id <- shiny::reactive({
      return(vF_teams_DT[long.name == input$leftTeam]$team.id)
    })
    right_team_id <- shiny::reactive({
      return(vF_teams_DT[long.name == input$rightTeam]$team.id)
    })

    ## home
    leftHome <- shiny::reactive({
      return(input$leftHome)
    })
    rightHome <- shiny::reactive({
      return(input$rightHome)
    })

    df_left <- shiny::reactive({
      year <- selected_year()
      upper_lim <- as.numeric(year) +1
      upper_lim <- paste0(as.character(upper_lim),'000000')
      lower_lim <- as.numeric(year) -1
      lower_lim <- paste0( as.character(lower_lim),'999999')
      if (leftHome() == 'Home') {
        games_left <- vF_game_info[home.teamID == left_team_id()]
      }else if (leftHome() == "Away") {
        games_left <- vF_game_info[away.teamID == left_team_id()]
      }
      return(vF_game_plays[team.id.for == left_team_id() & (as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                           & game.id %in% games_left$game.id])
    })


    df_right <- shiny::reactive({
      year <- selected_year()
      upper_lim <- as.numeric(year) +1
      upper_lim <- paste0(as.character(upper_lim),'000000')
      lower_lim <- as.numeric(year) -1
      lower_lim <- paste0( as.character(lower_lim),'999999')
      if (rightHome() == 'Home') {
        games_right <- vF_game_info[home.teamID == right_team_id()]
      }else if (rightHome() == "Away") {
        games_right <- vF_game_info[away.teamID == right_team_id()]
      }
      return(vF_game_plays[team.id.for == right_team_id() & (as.numeric(game.id) > as.numeric(lower_lim) & as.numeric(game.id) < as.numeric(upper_lim))
                           & game.id %in% games_right$game.id])
    })

    left_home <- shiny::reactive({
      if (input$leftHome == "Home") {
        return("at home")
      }else{
        return("away")
      }
    })

    right_home <- shiny::reactive({
      if (input$rightHome == "Home") {
        return("at home")
      }else{
        return("away")
      }
    })

    output$icemap_team <- plotly::renderPlotly({
      # browser()
      df_left <- df_left()
      df_right <- df_right()
      df_left_shots <- df_left[result.eventTypeId == 'SHOT']
      df_left_goals <- df_left[result.eventTypeId == 'GOAL']
      df_right_shots <- df_right[result.eventTypeId == 'SHOT']
      df_right_goals <- df_right[result.eventTypeId == 'GOAL']
      df <- rbind(df_left, df_right)
      #set the rink image and plot
      txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")

      # browser()
      df %>%
        plot_ly()  %>%
        add_markers(
          data = df_left_shots,
          hoverinfo='skip',
          x = ~l.x, y=~l.y, marker = list(size = 20, color = 'blue', opacity = max(50/nrow(df_left_shots),0.01)), name = paste("Shot map of",input$leftTeam,"playing ",left_home())
        ) %>%
        add_markers(
          data = df_right_shots,
          hoverinfo='skip',
          x = ~r.x, y=~r.y, marker = list(size = 20, color = 'red', opacity = max(50/nrow(df_right_shots),0.01)), name = paste("Shot map of",input$rightTeam,"playing ",right_home())
        ) %>%
        add_markers(
          data = df_left_goals,
          hoverinfo='text',
          hovertext=paste(df_left_goals$result.secondaryType),
          x = ~l.x, y=~l.y, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_left_goals)), name = "Goals"
        ) %>%
        add_markers(
          data = df_right_goals,
          hoverinfo='text',
          hovertext=paste(df_right_goals$result.secondaryType),
          x = ~r.x, y=~r.y, showlegend = FALSE, marker = list(size = 3, color = 'black', opacity = 200/nrow(df_right_goals))
        ) %>%
        layout(
          xaxis = list(range = c(-110,110), title = '', showticklabels=FALSE, zeroline = FALSE, showline = FALSE, fixedrange=TRUE),
          yaxis = list(range = c(-50,50), title = '', showticklabels=FALSE, zeroline = FALSE, showline = FALSE, fixedrange=TRUE),
          legend = list(
            orientation = "h",
            y = -0.1,
            xanchor = "center",
            x = 0.5),
          images= list(
            source= paste('data:image/png;base64', txt, sep=','),
            xref= "x",
            yref= "y",
            x = 0,
            y = 0,
            sizex = 220,
            sizey = 90,
            opacity = 0.8,
            layer = "below",
            xanchor = "center",
            yanchor = "middle",
            sizing = "stretch"
          )
        )
    })

    output$teamText <- renderText({
      paste0("This is the shot map of ", input$leftTeam,
             " playing ", left_home(), " on the left and ",input$rightTeam, " playing ",right_home()," on the right.\n The density of the shot map shows the frequency of shots taken in each area of the rink. Individual dot points show where goals have been scored from.")
    })

  })
}
