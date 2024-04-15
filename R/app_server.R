#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dplyr)
library(stringr)
library(shinyjs)
load("./data.RData")
usethis::use_data(vF_game_info, overwrite = TRUE)
usethis::use_data(vF_game_plays, overwrite = TRUE)
usethis::use_data(vF_game_plays_players, overwrite = TRUE)
usethis::use_data(vF_game_teams_stats, overwrite = TRUE)
usethis::use_data(vF_player_info, overwrite = TRUE)
usethis::use_data(vF_player_season_data, overwrite = TRUE)
usethis::use_data(vF_teams_DT, overwrite = TRUE)
app_server <- function(input, output, session) {
  # Include CSS class to collapse the sidebar initially
  shinyjs::addClass(selector = "body", class = "sidebar-collapse")

  # Define static lists of years and player types
  allYears <- c('2018', '2017', '2016', '2015', '2014')
  player_type_choices <- c('Offence', 'Defence', 'Goalie')

  # Data pre-processing: creating concatenated full names and IDs
  vF_player_info$fullName <- paste(vF_player_info$firstName, vF_player_info$lastName, sep = ' ')
  vF_game_plays$game_and_event_id <- paste(vF_game_plays$game.id, vF_game_plays$about.eventIdx, sep = '')
  vF_game_plays_players$game_and_event_id <- paste(vF_game_plays_players$game.id, vF_game_plays_players$eventIdx, sep = '')
  image_file <- "full-rink.png"  # Path to your rink image, make sure it's accessible

  # Reactive constructs for dynamic dropdowns based on dataset
  team_choices <- reactive({
    choices <- unique(vF_teams_DT$long.name)
    choices[order(choices)]
  })

  arena_choices <- reactive({
    choices <- unique(vF_teams_DT$venue.city)
    choices[order(choices)]
  })

  # Observers for dynamic UI elements related to input tabs
  observeEvent(input$tabs, {
    updateSelectInput(session, 'tabs', selected = input$tabs)
  })

  # Observers to update player performance dropdowns
  observe({
    updateSelectInput(session, inputId = "playerPerf1", choices = player_choices_perf())
  })

  observe({
    updateSelectInput(session, inputId = "playerPerf2", choices = player_choices_perf())
  })
  # Reactive for filtering player choices based on selected year
  player_choices <- reactive({
    player_ids <- vF_player_season_data[vF_player_season_data$season == input$year]$player.id
    player_names <- vF_player_info[vF_player_info$player.id %in% player_ids]$fullName
    player_names[order(player_names)]
  })

  # Reactives for team performance selections
  team_Perf1 <- reactive(input$teamPerf1)
  team_Perf2 <- reactive(input$teamPerf2)
  team_Perf3 <- reactive(input$teamPerf3)
  team_stat_type <- reactive(input$statType)

  # Reactives to determine home or away status
  left_home <- reactive(if (input$leftHome == "Home") "at home" else "away")
  right_home <- reactive(if (input$rightHome == "Home") "at home" else "away")

  # Reactives for getting team and player IDs
  left_team_id <- reactive(vF_teams_DT[vF_teams_DT$long.name == input$leftTeam]$team.id)
  right_team_id <- reactive(vF_teams_DT[vF_teams_DT$long.name == input$rightTeam]$team.id)
  arena_team_id <- reactive(vF_teams_DT[vF_teams_DT$venue.city == input$arena]$team.id)

  # Reactives for getting team and arena details
  arena_team <- reactive(vF_teams_DT[vF_teams_DT$venue.city == input$arena]$long.name)
  arena_name <- reactive(vF_teams_DT[vF_teams_DT$venue.city == input$arena]$venue.name)

  # Reactives for player IDs based on selection
  left_player_id <- reactive(vF_player_info[vF_player_info$fullName == input$leftPlayer]$player.id)
  right_player_id <- reactive(vF_player_info[vF_player_info$fullName == input$rightPlayer]$player.id)

  # Reactives for various selected years
  selected_year <- reactive(input$year)
  selected_year_arena_shots <- reactive(input$yearArenaShots)
  selected_year_player_shots <- reactive(input$yearPlayerShots)
  selected_yearStat <- reactive(input$yearStat)
  selected_yearPerf <- reactive(input$yearTeam)
  selected_yearPlayer <- reactive(input$yearPlayer)

  # Reactive for dynamic player choices based on player category
  player_choices_perf <- reactive({
    req(input$playerCat)  # Ensure player category is selected before proceeding
    playerIds <- switch(input$playerCat,
                        'Offence' = vF_player_info[primaryPosition.code %in% c('R', 'L', 'C')]$player.id,
                        'Defence' = vF_player_info[primaryPosition.code == 'D']$player.id,
                        'Goalie'  = vF_player_info[primaryPosition.code == 'G']$player.id,
                        vF_player_info[primaryPosition.code == 'D']$player.id)  # Default to Defence if none matched
    playerNames <- vF_player_info[vF_player_info$player.id %in% playerIds, c("firstName", "lastName")]
    sort(unique(paste(playerNames$firstName, playerNames$lastName, sep=' ')))
  })

  # Reactives for home/away status
  leftHome <- reactive(input$leftHome)
  rightHome <- reactive(input$rightHome)

  # Reactive expressions to calculate game data based on selected parameters
  df_left <- reactive({
    req(selected_year(), leftHome(), left_team_id())  # Ensure necessary inputs are available
    year_bounds(selected_year())
    games_left <- vF_game_info[get_team_filter(leftHome(), left_team_id())]
    vF_game_plays[team.id.for == left_team_id() & within_year_bounds(game.id) & game.id %in% games_left$game.id]
  })

  df_right <- reactive({
    req(selected_year(), rightHome(), right_team_id())
    year_bounds(selected_year())
    games_right <- vF_game_info[get_team_filter(rightHome(), right_team_id())]
    vF_game_plays[team.id.for == right_team_id() & within_year_bounds(game.id) & game.id %in% games_right$game.id]
  })

  # Ensure the arena-specific reactive data fetches are correctly structured
  df_arena_home <- reactive({
    reactive_game_data(selected_year_arena_shots(), arena_team_id(), true)
  })

  df_arena_away <- reactive({
    reactive_game_data(selected_year_arena_shots(), arena_team_id(), false)
  })

  # Ensure the player-specific reactive data fetches are correctly structured
  df_left_player <- reactive({
    reactive_player_data(selected_year_player_shots(), left_player_id())
  })

  df_right_player <- reactive({
    reactive_player_data(selected_year_player_shots(), right_player_id())
  })

  # Render UI for Team Performance Comparison
  output$performanceByTeam <- renderUI({
    shinythemes::shinytheme("spacelab")
    fluidPage(
      fluidRow(
        align = 'center',
        box(
          solidHeader = FALSE, status = 'primary', width = 12,
          title = 'Compare 3 Teams',
          div(style = "display: inline-block; vertical-align: top; width: 30%;",
              selectInput('teamPerf1', 'Select Team 1', choices = team_choices(), selected = 'New York Rangers')),
          div(style = "display: inline-block; vertical-align: top; width: 30%;",
              selectInput('teamPerf2', 'Select Team 2', choices = team_choices(), selected = 'Florida Panthers')),
          div(style = "display: inline-block; vertical-align: top; width: 30%;",
              selectInput('teamPerf3', 'Select Team 3', choices = team_choices(), selected = 'Boston Bruins')),
          div(style = "display: inline-block; vertical-align: top; width: 10%;",
              selectInput('statType', 'Statistics Level', choices = c('Macro', 'Micro'), selected = 'Macro'))
        )
      ),
      fluidRow(
        align = 'center',
        box(plotOutput("trendPlot1"), width = 12, solidHeader = FALSE, status = 'primary'),
        box(plotOutput("trendPlot2"), width = 12, solidHeader = FALSE, status = 'primary'),
        box(plotOutput("trendPlot3"), width = 12, solidHeader = FALSE, status = 'primary'),
        box(plotOutput("trendPlot4"), width = 12, solidHeader = FALSE, status = 'primary')
      )
    )
  })

  # Render UI for Player Performance Comparison
  output$performanceByPlayer <- renderUI({
    shinythemes::shinytheme("spacelab")
    fluidPage(
      fluidRow(
        align = 'center',
        box(
          solidHeader = FALSE, status = 'primary', width = '100%',
          title = 'Compare 2 Players',
          div(style = "vertical-align: top; width: 15%;",
              selectInput('playerCat', 'Select a Category', choices = player_type_choices, selected = 'Defence')),
          div(style = "display: inline-block; vertical-align: top; width: 30%;",
              selectInput('playerPerf1', 'Select Player 1', choices = player_choices_perf())),
          div(style = "display: inline-block; vertical-align: top; width: 30%;",
              selectInput('playerPerf2', 'Select Player 2', choices = player_choices_perf()))
        )
      ),
      fluidRow(
        align = 'center',
        box(plotOutput("playerTrendPlot1"), width = 6, solidHeader = FALSE, status = 'primary'),
        box(plotOutput("playerTrendPlot2"), width = 6, solidHeader = FALSE, status = 'primary')
      )
    )
  })

  # Render UI for Seasonal Statistics
  output$statisticBySeason <- renderUI({
    shinythemes::shinytheme("spacelab")
    navbarPage(
      "", id = 'someID',
      tabPanel("Summary",
               fluidPage(
                 fluidRow(
                   align = 'center',
                   box(
                     solidHeader = FALSE, width = '100%',
                     title = 'Arena', status = "primary",
                     div(style = "display: inline-block; vertical-align: top; width: 15%;",
                         selectInput('yearStat', 'By Season', choices = c(allYears, 'All'), selected = '2018')),
                     DT::dataTableOutput("table_statistic_arena")
                   )
                 ),
                 fluidRow(
                   box(solidHeader = FALSE, width = '100%', title = 'Team', status = "primary", DT::dataTableOutput("table_statistic_team")),
                   box(solidHeader = FALSE, width = '100%', title = 'Player', status = "primary", DT::dataTableOutput("table_statistic_player"))
                 )
               )
      ),
      tabPanel("Visualization",
               fluidPage(
                 fluidRow(
                   box(solidHeader = FALSE, width = '100%', title = 'Arena (by Cities) Home Vs Away Impact by Season', status = "primary")
                 ),
                 fluidRow(
                   box(plotOutput("arenaMapGoals"), status = "primary", width = 6, solidHeader = FALSE, title = 'Goals'),
                   box(plotOutput("arenaMapWins"), status = "primary", width = 6, solidHeader = FALSE, title = 'Wins')
                 ),
                 fluidRow(
                   box(plotOutput("teamGoals"), status = "primary", width = 6, solidHeader = FALSE, title = 'Goals'),
                   box(plotOutput("teamWins"), status = "primary", width = 6, solidHeader = FALSE, title = 'Wins')
                 )
               )
      )
    )
  })
  # Shot by Team UI
  output$shotByTeam <- renderUI({
    fluidPage(
      theme = shinythemes::shinytheme("spacelab"),
      fluidRow(
        align = 'center',
        box(solidHeader = FALSE, status = 'primary', collapsible = TRUE, width = '100%',
            title = "Filters",
            div(style = "display: inline-block;vertical-align:top; width: 30%;",
                selectInput('leftTeam', 'Team 1', choices = team_choices(), selected = 'Boston Bruins')),
            div(style = "display: inline-block;vertical-align:top; width: 15%;",
                radioButtons('leftHome', '', choices = c('Home', 'Away'), selected = 'Home')),
            div(style = "display: inline-block;vertical-align:top; width: 30%;",
                selectInput('rightTeam', 'Team 2', choices = team_choices(), selected = 'New York Rangers')),
            div(style = "display: inline-block;vertical-align:top; width: 15%;",
                radioButtons('rightHome', '', choices = c('Home', 'Away'), selected = 'Home')),
            div(style = "display: inline-block;vertical-align:top; width: 10%;",
                selectInput('year', 'Season', choices = allYears, selected = '2018'))
        )
      ),
      fluidRow(
        align = "center",
        box(solidHeader = FALSE, status = 'primary', title = "Rink Plot", width = '100%', plotlyOutput("icemap_team"))
      )
    )
  })
  # Shots by Arena UI
  output$shotByArena <- renderUI({
    fluidPage(
      theme = shinythemes::shinytheme("spacelab"),
      fluidRow(
        align = 'center',
        box(
          solidHeader = TRUE,
          status = 'primary',
          collapsible = TRUE,
          width = '100%',
          title = "Arena Filters",
          div(
            style = "display: inline-block; vertical-align: top; width: 22%;",
            selectInput('arena', 'Arena', choices = arena_choices(), multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL)
          ),
          div(
            style = "display: inline-block; vertical-align: top; width: 22%;",
            selectInput('yearArenaShots', 'Season', choices = allYears, selected = '2018', multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL)
          )
        )
      ),
      fluidRow(
        align = "center",
        box(
          title = 'Rink Plot',
          solidHeader = TRUE,
          status = 'primary',
          width = '100%',
          textOutput("arenaText"),
          plotly::plotlyOutput("icemap_Arena")
        )
      )
    )
  })
  # Shots by Player UI
  output$shotByPlayer <- renderUI({
      fluidPage(
        theme = shinythemes::shinytheme("spacelab"),
        fluidRow(
          align='center',
          box(
            solidHeader = TRUE,
            collapsible = TRUE,
            width = '100%',
            status = 'primary',
            title = "Player Filters",
            div(
              style="display: inline-block; vertical-align: top; width: 30%;",
              selectInput('leftPlayer', 'Player 1', choices = player_choices(), selected = "Nikita Kucherov", multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL)
            ),
            div(
              style="display: inline-block; vertical-align: top; width: 30%;",
              selectInput('rightPlayer', 'Player 2', choices = player_choices(), selected = "Alex Ovechkin", multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL)
            ),
            div(
              style="width: 20%;",
              selectInput('yearPlayerShots', 'Season', choices = allYears, multiple = FALSE,
                          selectize = TRUE, width = NULL, size = NULL)
            )
          )
        ),
        fluidRow(
          align = "center",
          box(
            solidHeader = TRUE,
            status = 'primary',
            title = "Rink Plot",
            width = '100%',
            textOutput("playerText"),
            plotly::plotlyOutput("icemap_player")
          )
        )
      )
    })
    # Player Table Statistics
    output$table_statistic_player <- DT::renderDataTable({
      req(selected_yearStat())  # Ensure the year is selected
      playerYear <- selected_yearStat()
      dataPlayerYear <- if (playerYear != 'All') {
        vF_player_season_data[vF_player_season_data$season == playerYear]
      } else {
        vF_player_season_data[vF_player_season_data$season %in% allYears]
      }

      df <- dataPlayerYear %>%
        select(player.id, stat.assists, stat.goals, stat.games, stat.shots) %>%
        drop_na() %>%
        group_by(player.id) %>%
        summarise_each(list(sum)) %>%
        left_join(vF_player_info, by = "player.id") %>%
        mutate(Player = paste(firstName, lastName)) %>%
        select(Player, stat.games, stat.goals, stat.shots, stat.assists) %>%
        arrange(desc(stat.goals))

      DT::datatable(df, options = list(orderClasses = TRUE, pageLength = 5))
    })
    # Team Table Statistics
    output$table_statistic_team <- DT::renderDataTable({
      req(selected_yearStat())  # Ensure reactive value is ready
      statYear <- selected_yearStat()

      dataStatYear <- if (statYear != 'All') {
        vF_game_info[vF_game_info$season == statYear]
      } else {
        vF_game_info
      }

      homeTeam <- dataStatYear[, c('home.teamID', 'home.win', 'home.goals')]
      awayTeam <- dataStatYear[, c('away.teamID', 'away.win', 'away.goals')]

      colnames(homeTeam)[1] <- 'team.id'
      homeTeamGroupBy <- aggregate(. ~ team.id, homeTeam, sum)

      colnames(awayTeam)[1] <- 'team.id'
      awayTeamGroupBy <- aggregate(. ~ team.id, awayTeam, sum)

      teamStat <- merge(homeTeamGroupBy, awayTeamGroupBy, by = 'team.id')
      teamStat <- merge(teamStat, vF_teams_DT[, c('team.id', 'long.name', 'venue.name', 'venue.city')], by = 'team.id')
      teamStat <- teamStat[, c('long.name', 'venue.name', 'venue.city', 'home.win', 'away.win', 'home.goals', 'away.goals')]

      colnames(teamStat) <- c('Name', 'Home Venue', 'City', 'Home Wins', 'Away Wins', 'Home Goals', 'Away Goals')
      teamStat$`Total Wins` <- teamStat$`Home Wins` + teamStat$`Away Wins`
      teamStat <- teamStat[, c('Name', 'Total Wins', 'Home Wins', 'Away Wins', 'Home Goals', 'Away Goals')]
      teamStat <- teamStat[order(teamStat$`Total Wins`, decreasing = TRUE),]

      DT::datatable(teamStat, options = list(orderClasses = TRUE, lengthMenu = c(5, 30, 50), pageLength = 5))
    })
  # Arena Statistics Table
    output$table_statistic_arena <- DT::renderDataTable({
      req(selected_yearStat())
      statYear <- selected_yearStat()

      dataStatYear <- if (statYear != 'All') {
        vF_game_info[vF_game_info$season == statYear]
      } else {
        vF_game_info[vF_game_info$season %in% allYears]
      }

      df <- dataStatYear[, c('name', 'home.win', 'away.win', 'home.goals', 'away.goals')]
      dfGroupByArena <- aggregate(. ~ name, df, sum)

      dfGroupByArena <- merge(dfGroupByArena, vF_teams_DT[, c('venue.name', 'long.name', 'venue.city', 'locationName', 'division.name')], by = 'venue.name')

      colnames(dfGroupByArena) <- c('Name', 'Home Wins', 'Away Wins', 'Home Goals', 'Away Goals', 'Home Team', 'City', 'Location', 'Division')
      dfGroupByArena <- dfGroupByArena[, c('Name', 'Home Wins', 'Away Wins', 'Home Goals', 'Away Goals', 'Home Team', 'Division')]
      dfGroupByArena <- dfGroupByArena[order(dfGroupByArena$`Home Wins`, decreasing = TRUE),]

      DT::datatable(dfGroupByArena, options = list(orderClasses = TRUE, lengthMenu = c(5, 30, 50), pageLength = 5))
    })
    # Trend Plot 1 - Wins and Blocks
    output$trendPlot1 <- renderPlot({
      req(input$teamPerf1, input$teamPerf2, input$teamPerf3, team_stat_type())

      teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
      df <- vF_game_teams_stats[team.id %in% teamIds]
      df$game.id <- stringr::str_sub(df$game.id, end = -7)
      df <- dplyr::group_by(df, game.id, team.id, HoA) %>%
        dplyr::summarise_all(dplyr::funs(sum), na.rm = TRUE) %>%
        dplyr::ungroup()

      if (team_stat_type() == 'Macro') {
        df <- dplyr::select(df, game.id, team.id, HoA, won)
        df <- merge(df, vF_teams_DT[, c('team.id', 'short.name')], by = 'team.id')
        df$team.id <- df$short.name
        ggplot(df, aes(x = as.factor(team.id), y = won, fill = HoA)) +
          geom_bar(stat = "identity", position = "stack", width = 0.6, alpha = 0.5) +
          theme_bw() +
          facet_grid(~game.id) +
          scale_fill_manual(values = c("cyan1", "bisque4")) +
          labs(x = '', y = '', title = 'Wins') +
          theme(plot.title = element_text(hjust = 0.5))
      } else {
        df <- dplyr::select(df, game.id, team.id, HoA, blocked)
        df <- merge(df, vF_teams_DT[, c('team.id', 'short.name')], by = 'team.id')
        df$team.id <- df$short.name
        ggplot(df, aes(x = as.factor(team.id), y = blocked, fill = HoA)) +
          geom_bar(stat = "identity", position = "stack", width = 0.6, alpha = 0.5) +
          theme_bw() +
          facet_grid(~game.id) +
          scale_fill_manual(values = c("cyan1", "bisque4")) +
          labs(x = '', y = '', title = 'Blocked') +
          theme(plot.title = element_text(hjust = 0.5))
      }
    })

  output$trendPlot2 <- renderPlot({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro') {
      df <- df[,c('game.id', "team.id", 'HoA', 'goals')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() +
        geom_bar(data =df, aes(y = goals, x = as.factor(team.id), fill = HoA),  size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') +
        theme_bw() +
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) +
        xlab('') +ylab('') + ggtitle('Goals') + theme(plot.title = element_text(hjust = 0.5))
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'takeaways')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() +
        geom_bar(data =df, aes(y = takeaways, x = as.factor(team.id), fill = HoA),  size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') +
        theme_bw() +
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) +
        xlab('') +ylab('') + ggtitle('Takeaways') + theme(plot.title = element_text(hjust = 0.5))
    }

  })
  output$trendPlot3 <- renderPlot({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro'){
      df <- df[,c('game.id', "team.id", 'HoA', 'shots')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() +
        geom_bar(data =df, aes(y = shots, x = as.factor(team.id), fill = HoA), size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') +
        theme_bw() +
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) +
        xlab('') +ylab('') + ggtitle('Shots') + theme(plot.title = element_text(hjust = 0.5))
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'giveaways')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() +
        geom_bar(data =df, aes(y = giveaways, x = as.factor(team.id), fill = HoA),  size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') +
        theme_bw() +
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) +
        xlab('') +ylab('') + ggtitle('Giveaways') + theme(plot.title = element_text(hjust = 0.5))
    }

  })
  output$trendPlot4 <- renderPlot({
    teamIds <- vF_teams_DT[long.name %in% c(input$teamPerf1, input$teamPerf2, input$teamPerf3)]$team.id
    df <- vF_game_teams_stats[team.id %in% teamIds]
    df$game.id <- str_sub(df$game.id, end=-7)
    df <- as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
    if (team_stat_type() == 'Macro') {
      df <- df[,c('game.id', "team.id", 'HoA', 'pim')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() +
        geom_bar(data =df, aes(y = pim, x = as.factor(team.id), fill = HoA), size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') +
        theme_bw() +
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) +
        xlab('') +ylab('') + ggtitle('Penalty Infraction Minute') + theme(plot.title = element_text(hjust = 0.5))
    } else {
      df <- df[,c('game.id', "team.id", 'HoA', 'hits')]
      df <- merge(df, vF_teams_DT[,c('team.id','short.name')], by='team.id')
      df$team.id <- df$short.name
      ggplot() +
        geom_bar(data =df, aes(y = hits, x = as.factor(team.id), fill = HoA), size=0.25, width=0.6, alpha= 0.5, stat = "identity", position = 'stack') +
        theme_bw() +
        facet_grid(~game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) +
        xlab('') +ylab('') + ggtitle('Hits') + theme(plot.title = element_text(hjust = 0.5))
    }

  })
  output$playerTrendPlot1 <- renderPlot({
    playerId <- vF_player_info[(fullName == input$playerPerf1)]$player.id
    df <- vF_player_season_data[(player.id == playerId) & (season %in% allYears)]
    if (input$playerCat %in% c('Defence','Offence')){
      df <- df[,c('season', 'stat.assists', 'stat.goals', 'stat.games',
                  'stat.points','stat.shots', 'stat.penaltyMinutes','stat.hits','stat.blocked')]
      df <- rename(df, c('stat.assists' = 'Assists', 'stat.goals'='Goals', 'stat.games'='Games',
                         'stat.points' = 'Points','stat.shots'='Shots', 'stat.penaltyMinutes'='Penalty Minutes','stat.hits'='Hits','stat.blocked'='Blocked'))
    } else {
      df <- df[,c('season', 'stat.wins','stat.losses','stat.shutouts')]
      df <- rename(df, c('stat.wins'='Wins','stat.losses'='Losses','stat.shutouts'='Shutouts'))
    }

    df$season = as.factor(df$season)
    df <- unique(df, by='season')
    #dft <- df %>%
    #  rownames_to_column %>%
    #  gather(var, value, -rowname) %>%
    #  spread(rowname, value)
    #colnames(dft) <- dft[1,]
    #dft <- dft[-c(1),]
    #dft[,2:ncol(dft)] <- sapply(dft[,2:ncol(dft)], as.numeric)
    #ggparcoord(dft, columns=2:ncol(dft), groupColumn = 1,
    #           title='', scale = 'globalminmax')
    data_long <- melt(df, id="season")
    ggplot(data=data_long,
           aes(x=season, y=value, colour=variable, group = variable)) +
      scale_y_continuous(breaks= pretty_breaks()) +
      geom_line() + geom_point()
  })
  output$playerTrendPlot2 <- renderPlot({
    playerId <- vF_player_info[(fullName == input$playerPerf2)]$player.id
    df <- vF_player_season_data[(player.id == playerId) & (season %in% allYears)]
    if (input$playerCat %in% c('Defence','Offence')){
      df <- df[,c('season', 'stat.assists', 'stat.goals', 'stat.games',
                  'stat.points','stat.shots', 'stat.penaltyMinutes','stat.hits','stat.blocked')]
      df <- rename(df, c('stat.assists' = 'Assists', 'stat.goals'='Goals', 'stat.games'='Games',
                         'stat.points'='Points','stat.shots'='Shots', 'stat.penaltyMinutes'='Penalty Minutes','stat.hits'='Hits','stat.blocked'='Blocked'))
    } else {
      df <- df[,c('season', 'stat.wins','stat.losses','stat.shutouts')]
      df <- rename(df, c('stat.wins'='Wins','stat.losses'='Losses','stat.shutouts'='Shutouts'))
    }
    df$season = as.factor(df$season)
    df <- unique(df, by='season')
    data_long <- melt(df, id="season")
    ggplot(data=data_long,
           aes(x=season, y=value, colour=variable, group = variable)) +
      scale_y_continuous(breaks= pretty_breaks()) +
      geom_line() + geom_point()
  })
  output$teamGoals <- renderPlot({
    statYear <- selected_yearStat()
    if (statYear != 'All'){
      dataStatYear <- vF_game_info[vF_game_info$season == statYear]
    } else {
      dataStatYear <- vF_game_info
    }
    homeTeam <- dataStatYear[,c('home.teamID', 'home.win', 'home.goals')]
    awayTeam <- dataStatYear[,c('away.teamID', 'away.win', 'away.goals')]
    colnames(homeTeam)[1] <- colnames(awayTeam)[1] <- 'team.id'
    homeTeamGroupBy <- aggregate( .~team.id, homeTeam, sum)
    awayTeamGroupBy <- aggregate( .~ team.id, awayTeam, sum)
    teamStat <- merge(homeTeamGroupBy, awayTeamGroupBy, by = 'team.id')
    teamStat <- merge(teamStat, vF_teams_DT[,c('team.id', 'long.name', 'venue.name', 'venue.city')], by = 'team.id')
    teamStat <- teamStat[,c('long.name', 'venue.name', 'venue.city', 'home.win', 'away.win', 'home.goals', 'away.goals')]
    teamStat<- rename(teamStat, c('home.win' = 'Home Wins', 'away.win' = 'Away Wins', 'home.goals'='Home Goals', 'away.goals'='Away Goals'))
    teamStatGather <- teamStat %>%
      gather(key='GoalType', value = 'Goals', 'Home Goals', 'Away Goals') %>%
      gather(key = 'WinType', value = 'Wins', 'Home Wins', 'Away Wins')
    ggplot(teamStatGather, aes(Goals, fct_reorder2(long.name, GoalType == 'away.goals', Goals, .desc = FALSE), color = GoalType)) +
      geom_point() + ylab('')
  })
  output$teamWins <- renderPlot({
    statYear <- selected_yearStat()
    if (statYear != 'All'){
      dataStatYear <- vF_game_info[vF_game_info$season == statYear]
    } else {
      dataStatYear <- vF_game_info
    }
    homeTeam <- dataStatYear[,c('home.teamID', 'home.win', 'home.goals')]
    awayTeam <- dataStatYear[,c('away.teamID', 'away.win', 'away.goals')]
    colnames(homeTeam)[1] <- colnames(awayTeam)[1] <- 'team.id'
    homeTeamGroupBy <- aggregate( .~team.id, homeTeam, sum)
    awayTeamGroupBy <- aggregate( .~ team.id, awayTeam, sum)
    teamStat <- merge(homeTeamGroupBy, awayTeamGroupBy, by = 'team.id')
    teamStat <- merge(teamStat, vF_teams_DT[,c('team.id', 'long.name', 'venue.name', 'venue.city')], by = 'team.id')
    teamStat <- teamStat[,c('long.name', 'venue.name', 'venue.city', 'home.win', 'away.win', 'home.goals', 'away.goals')]
    teamStat<- rename(teamStat, c('home.win' = 'Home Wins', 'away.win' = 'Away Wins', 'home.goals'='Home Goals', 'away.goals'='Away Goals'))
    teamStatGather <- teamStat %>%
      gather(key='GoalType', value = 'Goals', 'Home Goals', 'Away Goals') %>%
      gather(key = 'WinType', value = 'Wins', 'Home Wins', 'Away Wins')
    ggplot(teamStatGather, aes(Wins, fct_reorder2(long.name, WinType == 'away.win', Wins, .desc = FALSE), color = WinType)) +
      geom_point() + ylab('')
  })
  output$arenaMapGoals <- renderPlot({
    statYear <- selected_yearStat()
    if (statYear != 'All'){
      dataStatYear <- vF_game_info[vF_game_info$season == statYear]
    } else {
      dataStatYear <- vF_game_info[vF_game_info$season %in% allYears]
    }

    df <- dataStatYear[,c('name','home.win','away.win','home.goals','away.goals')]
    dfGroupByArena <- aggregate(. ~ name , df, sum)
    colnames(dfGroupByArena)[1] <- 'venue.name'
    dfGroupByArena <- merge(dfGroupByArena, vF_teams_DT[,c('venue.name', 'venue.city', 'locationName', 'division.name')], by = 'venue.name')
    colnames(dfGroupByArena)[6] <- 'name'
    us_cities <- us.cities
    us_cities$name <- str_sub(us_cities$name, end = -4)
    #dfGroupByArena$locationName <- c('TX', )
    #colnames(dfGroupByArena)[7] <- 'country.etc'
    arenaCities <- merge(us_cities, dfGroupByArena, by = 'name')
    arenaCities <- arenaCities %>% distinct(name, .keep_all = TRUE)
    arenaCities<- rename(arenaCities, c('home.win' = 'Home Wins', 'away.win' = 'Away Wins', 'home.goals'='Home Goals', 'away.goals'='Away Goals'))
    arenaCitiesGather <- arenaCities %>%
      gather(key='GoalType', value = 'Goals', 'Home Goals', 'Away Goals') %>%
      gather(key = 'WinType', value = 'Wins', 'Home Wins', 'Away Wins')
    set_breaks = function(limits) {
      seq(limits[1], limits[2], by = round((limits[2]-limits[1])/4))
    }
    MainStates <- map_data("state")
    ggplot(data=arenaCitiesGather) + geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                                                   color="white", fill="white" ) + geom_point(aes(x=long, y=lat, size = Goals, color = GoalType),
                                                                                              alpha = .5) + geom_text_repel(data = arenaCities, aes(x=long, y=lat, label=name), hjust=0, vjust=0,
                                                                                                                            size=3.5) + scale_size_continuous(breaks = set_breaks)
  })
  output$arenaMapWins <- renderPlot({
    statYear <- selected_yearStat()
    if (statYear != 'All'){
      dataStatYear <- vF_game_info[vF_game_info$season == statYear]
    } else {
      dataStatYear <- vF_game_info[vF_game_info$season %in% allYears]
    }
    df <- dataStatYear[,c('name','home.win','away.win','home.goals','away.goals')]
    dfGroupByArena <- aggregate(. ~ name , df, sum)
    colnames(dfGroupByArena)[1] <- 'venue.name'
    dfGroupByArena <- merge(dfGroupByArena, vF_teams_DT[,c('venue.name', 'venue.city', 'locationName', 'division.name')], by = 'venue.name')
    colnames(dfGroupByArena)[6] <- 'name'
    us_cities <- us.cities
    us_cities$name <- str_sub(us_cities$name, end = -4)
    arenaCities <- merge(us_cities, dfGroupByArena, by = 'name')
    arenaCities <- arenaCities %>% distinct(name, .keep_all = TRUE)
    arenaCities<- rename(arenaCities, c('home.win' = 'Home Wins', 'away.win' = 'Away Wins', 'home.goals'='Home Goals', 'away.goals'='Away Goals'))
    arenaCitiesGather <- arenaCities %>%
      gather(key='GoalType', value = 'Goals', 'Home Goals', 'Away Goals') %>%
      gather(key = 'WinType', value = 'Wins', 'Home Wins', 'Away Wins')
    set_breaks = function(limits) {
      seq(limits[1], limits[2], by = round((limits[2]-limits[1])/4))
    }
    MainStates <- map_data("state")
    ggplot(data=arenaCitiesGather) + geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                                                   color="white", fill="white" ) + geom_point(aes(x=long, y=lat, size = Wins, color=WinType),
                                                                                              alpha = .5) + geom_text_repel(data = arenaCities, aes(x=long, y=lat, label=name), hjust=0, vjust=0,
                                                                                                                            size=3.5) + scale_size_continuous(breaks = set_breaks)
  })
  output$icemap_team <- plotly::renderPlotly({
    df_left <- df_left()
    df_right <- df_right()
    df_left_shots <- df_left[result.eventTypeId == 'SHOT']
    df_left_goals <- df_left[result.eventTypeId == 'GOAL']
    df_right_shots <- df_right[result.eventTypeId == 'SHOT']
    df_right_goals <- df_right[result.eventTypeId == 'GOAL']
    df <- rbind(df_left, df_right)
    #set the rink image and plot
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
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
  output$icemap_Arena <- plotly::renderPlotly({
    df_left <- df_arena_home()
    df_right <- df_arena_away()
    df_left_shots <- df_left[result.eventTypeId == 'SHOT']
    df_left_goals <- df_left[result.eventTypeId == 'GOAL']
    df_right_shots <- df_right[result.eventTypeId == 'SHOT']
    df_right_goals <- df_right[result.eventTypeId == 'GOAL']
    df <- rbind(df_left, df_right)
    #set the rink image and plot
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
    df %>%
      plot_ly()  %>%
      add_markers(
        data = df_left_shots,
        hoverinfo='skip',
        x = ~l.x, y=~l.y, marker = list(size = 20, color = 'blue', opacity = max(50/nrow(df_left_shots),0.01)), name = paste("Shotmap of",arena_team(),"playing at home")
      ) %>%
      add_markers(
        data = df_right_shots,
        hoverinfo='skip',
        x = ~r.x, y=~r.y, marker = list(size = 20, color = 'red', opacity = max(50/nrow(df_right_shots),0.01)), name = paste("Shotmap of teams visiting",input$arena )
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
  output$icemap_player <- plotly::renderPlotly({
    df_left <- df_left_player()
    df_right <- df_right_player()
    df_left_shots <- df_left[result.eventTypeId == 'SHOT']
    df_left_goals <- df_left[result.eventTypeId == 'GOAL']
    df_right_shots <- df_right[result.eventTypeId == 'SHOT']
    df_right_goals <- df_right[result.eventTypeId == 'GOAL']
    df <- rbind(df_left, df_right)
    #set the rink image and plot
    txt <- RCurl::base64Encode(readBin(image_file, "raw", file.info(image_file)[1, "size"]), "txt")
    df %>%
      plot_ly()  %>%
      add_markers(
        data = df_left_shots,
        hoverinfo='skip',
        x = ~l.x, y=~l.y, marker = list(size = 15, color = 'blue', opacity = min(20/nrow(df_left_shots),0.3)), name = paste("Shotmap of",input$leftPlayer)
      ) %>%
      add_markers(
        data = df_right_shots,
        hoverinfo='skip',
        x = ~r.x, y=~r.y, marker = list(size = 15, color = 'red', opacity = min(20/nrow(df_right_shots),0.3)), name = paste("Shotmap of",input$rightPlayer)
      ) %>%
      add_markers(
        data = df_left_goals,
        hoverinfo='text',
        hovertext=paste(df_left_goals$result.secondaryType),
        x = ~l.x, y=~l.y, marker = list(size = 4, color = 'black', opacity = 1), name = "Goals"
      ) %>%
      add_markers(
        data = df_right_goals,
        hoverinfo='text',
        hovertext=paste(df_right_goals$result.secondaryType),
        x = ~r.x, y=~r.y, showlegend = FALSE, marker = list(size = 4, color = 'black', opacity =1)
      ) %>%
      layout(
        xaxis = list(range = c(-110,110),title = '', showticklabels=FALSE, zeroline = FALSE, showline = FALSE, fixedrange=TRUE),
        yaxis = list(range = c(-50,50),title = '', showticklabels=FALSE, zeroline = FALSE, showline = FALSE, fixedrange=TRUE),
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
  output$arenaText <- renderText({
    paste0("This is the shot map of ", arena_team(),
           " playing at home on the left and teams visiting ", input$arena, " on the right.\n The density of the shot map shows the frequency of shots taken in each area of the rink. Individual dot points show where goals have been scored from.")
  })
  output$playerText <- renderText({
    paste0("This is the shot map of ", input$leftPlayer,
           " on the left and ", input$rightPlayer, " on the right.\n The density of the shot map shows the frequency of shots taken in each area of the rink. Individual dot points show where goals have been scored from.")
  })
}
