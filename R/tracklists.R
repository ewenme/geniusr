#' Retrieve meta data for an album
#'
#' The Genius API lets you search for an album's meta data, given an album ID.
#' @param album_id An album ID (\code{album_id} returned in \code{\link{get_song_meta}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' get_album_meta(album_id = 337082)
#' @export

album_id <- 337082
artist_id <- 1421
album_url <- "https://genius.com/albums/Kendrick-lamar/Damn"



# start session
session <- rvest::html(album_url)

# get titles
foo <- rvest::html_nodes(session, ".chart_row-content-title") %>%
  rvest::html_text(trim = TRUE)

# remove everything after (and including) line break
foo <- gsub("\n.*", "", foo)

# get lyric links
bar <- rvest::html_nodes(session, ".chart_row-content")

bar <- stringr::str_match_all(bar, "<a href=\"(.*?)\"")

sapply(bar,`[`,2)
