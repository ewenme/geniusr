#' Retrieve an album's tracklisting
#'
#' Scrape an album's tracklisting, and song meta data, given an album ID.
#' @param album_id An album ID (\code{album_id} returned in \code{\link{get_song_meta}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' scrape_tracklist(album_id = 337082)
#' }
#' @export
scrape_tracklist <- function(album_id, access_token=genius_token()) {

  # get past cmd check
  album_name <- artist_id <- artist_name <- artist_url <- NULL

  # get album meta data
  album_info <- get_album_meta(album_id)

  # start session
  session <- suppressWarnings(rvest::html(album_info$album_url))

  # get track numbers
  song_number <- rvest::html_nodes(session, ".chart_row-number_container-number") %>%
    rvest::html_text(trim = TRUE) %>%
    as.numeric()

  # get track titles
  song_title <- rvest::html_nodes(session, ".chart_row-content-title") %>%
    rvest::html_text(trim = TRUE)

  # remove everything after (and including) line break
  song_title <- gsub("\n.*", "", song_title)

  # get lyric links
  song_lyrics_url <- rvest::html_nodes(session, ".chart_row-content")

  # match url part
  song_lyrics_url <- stringr::str_match_all(song_lyrics_url, "<a href=\"(.*?)\"")

  # choose list element containing url
  song_lyrics_url <- sapply(song_lyrics_url,`[`,2)

  # combine album elements into dataframe
  album_meta <- data.frame(song_number, song_title, song_lyrics_url)

  # add album meta data
  album_meta$album_id <- album_info$album_id
  album_meta$album_name <- album_info$album_name
  album_meta$album_url <- album_info$album_url
  album_meta$album_cover_art_url <- album_info$album_cover_art_url
  album_meta$album_release_date <- album_info$album_release_date
  album_meta$artist_id <- album_info$artist_id
  album_meta$artist_name <- album_info$artist_name
  album_meta$artist_url <- album_info$artist_url

  # remove missing song nos
  album_meta <- subset(album_meta, !is.na(song_number))

  # reorder cols
  album_meta <- subset(album_meta, select=c(song_number, song_title, song_lyrics_url,
                              album_name, album_id, artist_id, artist_name, artist_url))

  # return character variables
  album_meta$song_title <- as.character(album_meta$song_title)
  album_meta$song_lyrics_url <- as.character(album_meta$song_lyrics_url)

  return(tibble::as_tibble(album_meta))

}
