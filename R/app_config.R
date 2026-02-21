#' Access files in the current app
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' @export
app_sys <- function(...) {
  system.file(..., package = "HawaSpatial")
}

#' Read Golem Config
#'
#' @param str The setting to read
#' @param config The configuration profile
#' @export
get_golem_config <- function(
    str,
    config = Sys.getenv("GOLEM_CONFIG_ACTIVE", "default")
) {
  config::get(
    value = str,
    config = config,
    file = app_sys("golem-config.yml")
  )
}
