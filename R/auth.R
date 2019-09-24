#' Get or set Genius access token value
#'
#' The API wrapper functions in this package all rely on a Genius client access token
#' residing in the environment variable \code{GENIUS_API_TOKEN}. The
#' easiest way to accomplish this is to set it in the `\code{.Renviron}` file in your
#' home directory.
#' @param force force setting a new Genius API token for the current environment?
#' @return atomic character vector containing the Genius API token
#' @export
genius_token <- function(force = FALSE) {

  env <- Sys.getenv('GENIUS_API_TOKEN')
  if (!identical(env, "") && !force) return(env)

  if (!interactive()) {
    stop("Please set env var GENIUS_API_TOKEN to your Genius API key",
         call. = FALSE)
  }

  message("Couldn't find env var GENIUS_API_TOKEN See ?genius_token for more details.")
  message("Please enter your Genius Client Access Token and press enter:")
  pat <- readline(": ")

  if (identical(pat, "")) {
    stop("Genius API key entry failed", call. = FALSE)
  }

  message("Updating GENIUS_API_TOKEN env var to PAT")
  Sys.setenv(GENIUS_API_TOKEN = pat)

  pat

}
