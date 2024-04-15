#' module1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#' @param r state
#' @param base_input original input
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @noRd
#'
#' @importFrom shiny NS tagList plotOutput
mod_module_trend_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(plotOutput(ns("plot")))
}

#' module1 Server Functions
#' @import ggplot2
#' @importFrom shinipsum random_ggplot
#' @importFrom shiny renderPlot
#' @noRd
mod_module_trend_plot_server <- function(id, r, base_input) {
  moduleServer(id, function(input, output, session) {
    choices <- list(
      Macro = list(
        id1 = list(var = "won",
                   title = "Wins"),
        id2 = list(var = "goals",
                   title = "Goals"),
        id3 = list(var = "shots",
                   title = "Shots"),
        id4 = list(var = "pim",
                   title = "Penalty Infraction Minute")
      ),
      Micro = list(
        id1 = list(var = "blocked",
                   title = "Blocked"),
        id2 = list(var = "takeaways",
                   title = "Takeaways"),
        id3 = list(var = "giveaways",
                   title = "Giveaways"),
        id4 = list(var = "hits",
                   title = "Hits")
      )
    )

    output$plot <- renderPlot({
      title <- choices[[r()]][[id]][["title"]]
      vari <- choices[[r()]][[id]][["var"]]
      teamIds <-
        vF_teams_DT[long.name %in% c(base_input$teamPerf1,
                                     base_input$teamPerf2,
                                     base_input$teamPerf3)]$team.id
      df <- vF_game_teams_stats[team.id %in% teamIds]
      df$game.id <- str_sub(df$game.id, end = -7)
      df <-
        as.data.frame(df %>% group_by(game.id, team.id, HoA) %>% summarise_each(funs(sum)))
      df <- df[, c('game.id', "team.id", 'HoA', vari)]
      df <-
        merge(df, vF_teams_DT[, c('team.id', 'short.name')], by = 'team.id')
      df$team.id <- as.factor(df$short.name)
      ggplot() +
        geom_bar(
          data = df,
          aes_string(y = vari, x = "team.id", fill = "HoA"),
          size = 0.25,
          width = 0.6,
          alpha = 0.5,
          stat = "identity",
          position = 'stack'
        ) +
        theme_bw() +
        facet_grid(~ game.id) +
        scale_fill_manual("legend", values = c("cyan1", "bisque4")) +
        xlab('') + ylab('') + ggtitle(title) + theme(plot.title = element_text(hjust = 0.5))
    })

  })
}
