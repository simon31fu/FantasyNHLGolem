#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  # Define a custom function to load data
  load_all_data <- function() {
    data_path <- golem::app_sys("app/data")
    load(file.path(data_path, "data.RData"))
  }

  # Define a custom onStart function if not provided
  custom_onStart <- function() {
    load_all_data()  # Load data
    if (!is.null(onStart)) onStart()  # Then run any user-provided onStart function
  }
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

