#' Get or set GENIUS_API_TOKEN value
#'
#' The API wrapper functions in this package all rely on a Genius API
#' key residing in the environment variable \code{GENIUS_API_TOKEN}. The
#' easiest way to accomplish this is to set it in the `\code{.Renviron}` file in your
#' home directory.
#' @param force force setting a new Dark Sky API key for the current environment?
#' @return atomic character vector containing the Genius API token
#' @export

