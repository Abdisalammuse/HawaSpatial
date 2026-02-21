#' Run the Shiny Application
#'
#' @param onStart A function that will be called before the app is actually run.
#' @param options Named options that should be passed to the `runApp` call.
#' @param enableBookmarking Can be one of "url", "server", or "disable".
#' @param uiPattern A regular expression used to match the UI.
#' @param ... arguments to pass to golem_opts.
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
  # 1. Set HawaSpatial specific global options
  # This allows large DHS/MICS file uploads (900MB)
  options(shiny.maxRequestSize = 900 * 1024^2)

  # Disable S2 to prevent topology errors with shapefiles
  sf::sf_use_s2(FALSE)

  # 2. Launch the App
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
