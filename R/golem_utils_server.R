#' Inverted versions of in, is.null and is.na
#'
#' @noRd
#'
#' @examples
#' 1 %not_in% 1:10
#' not_null(NULL)
`%not_in%` <- Negate(`%in%`)

not_null <- Negate(is.null)

not_na <- Negate(is.na)

#' Removes the null from a vector
#'
#' @noRd
#'
#' @example
#' drop_nulls(list(1, NULL, 2))
drop_nulls <- function(x) {
  x[!sapply(x, is.null)]
}

#' If x is `NULL`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NULL`
#'
#' @noRd
#'
#' @examples
#' NULL %||% 1
"%||%" <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}

#' If x is `NA`, return y, otherwise return x
#'
#' @param x,y Two elements to test, one potentially `NA`
#'
#' @noRd
#'
#' @examples
#' NA %|NA|% 1
"%|NA|%" <- function(x, y) {
  if (is.na(x)) {
    y
  } else {
    x
  }
}

#' Calculate upper and lower year bounds
#'
#' Calculates and returns a list containing the upper and lower bounds of the given year, formatted to include all days in those years.
#' @param year Numeric or character year
#' @return A list with `lower` and `upper` bounds as strings.
#' @noRd
year_bounds <- function(year) {
  list(
    upper = paste0(as.character(as.numeric(year) + 1), '000000'),
    lower = paste0(as.character(as.numeric(year) - 1), '999999')
  )
}

#' Determine if a game ID falls within given year bounds
#'
#' Checks if a numeric game ID is within the numeric bounds specified.
#' @param game_id Numeric, the game ID to check.
#' @param bounds List, with numeric `lower` and `upper` bounds.
#' @return Boolean indicating if the game ID is within bounds.
#' @noRd
within_year_bounds <- function(game_id, bounds) {
  as.numeric(game_id) > as.numeric(bounds$lower) & as.numeric(game_id) < as.numeric(bounds$upper)
}

#' Generate filter expression based on home/away status
#'
#' Returns a logical expression to filter game information based on team home or away status and team ID.
#' @param home_status Character, either 'Home' or 'Away'.
#' @param team_id Numeric, the team ID to filter by.
#' @return An expression to use in data subsetting.
#' @noRd
get_team_filter <- function(home_status, team_id) {
  if (home_status == 'Home') {
    home.teamID == team_id
  } else {
    away.teamID == team_id
  }
}

#' Fetch reactive game data based on year, team, and home/away status
#'
#' Fetches game data reactively based on specified year, team ID, and whether the team is home or away.
#' @param year Numeric or character year to fetch data for.
#' @param team_id Numeric, the team ID.
#' @param is_home Boolean, `TRUE` if fetching home games, `FALSE` if away.
#' @return A subset of `vF_game_plays` data frame based on the filters.
#' @noRd
reactive_game_data <- function(year, team_id, is_home) {
  req(year, team_id)  # Ensure required variables are present
  bounds <- year_bounds(year)
  games <- vF_game_info[get_team_filter(if (is_home) 'Home' else 'Away', team_id)]
  vF_game_plays[team.id.for == team_id & within_year_bounds(game.id, bounds) & game.id %in% games$game.id]
}

#' Fetch reactive data for player based on year and player ID
#'
#' Reactively fetches data for games involving a specific player based on year and player ID.
#' @param year Numeric or character, year to fetch data for.
#' @param player_id Numeric, player ID.
#' @return A subset of `vF_game_plays` data frame based on the filters.
#' @noRd
reactive_player_data <- function(year, player_id) {
  req(year, player_id)  # Ensure required variables are present
  bounds <- year_bounds(year)
  games_player <- vF_game_plays_players[player.id == player_id & (playerType == "Scorer" | playerType == "Shooter")]
  vF_game_plays[within_year_bounds(game.id, bounds) & game_and_event_id %in% games_player$game_and_event_id]
}


#' Typing reactiveValues is too long
#'
#' @inheritParams reactiveValues
#' @inheritParams reactiveValuesToList
#'
#' @noRd
rv <- function(...) shiny::reactiveValues(...)
rvtl <- function(...) shiny::reactiveValuesToList(...)
