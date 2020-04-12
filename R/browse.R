#' Open the Genius homepage in your browser
#'
#' Opens a browser to \code{https://genius.com/}.
#'
#' @return A browser is opened to the Genius website
#' if the session is interactive. The URL is returned as a character string.
#'
#' @export
#'
#' @examples
#' \donttest{
#' browse_genius()
#' }
browse_genius <- function() {
  check_internet()
  if (interactive()) {
    utils::browseURL(
      url = "https://genius.com/",
      browser = getOption("browser")
    )
  }

  invisible("https://genius.com/")
}

#' Open the Genius url of a resource in your browser
#'
#' Opens a browser to the Genius url of a Genius "resource" (i.e.
#' the result of a successful \code{get_album|artist|song()} call).
#'
#' @param x a \code{genius_album}, \code{genius_artist},
#' or \code{genius_song} object
#'
#' @return A browser is opened to the Genius resource's url
#' if the session is interactive. The URL is returned as a character string.
#'
#' @export
#'
#' @examples
#' \donttest{
#' song <- get_song(song_id = 3039923)
#' browse_genius_resource(song)
#' }
browse_genius_resource <- function(x) {

  check_internet()

  stopifnot(
    any(is_genius_album(x), is_genius_artist(x),
        is_genius_song(x))
    )

  if (interactive()) {
    utils::browseURL(
      url = x$content$url,
      browser = getOption("browser")
    )
  }

  invisible(x$content$url)

}
