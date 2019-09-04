#' Retrieve an album's tracklisting
#'
#' Get an album's tracklisting, and song meta data, given an album ID.
#'
#' @inheritParams get_album
#'
#' @examples
#' \dontrun{
#' scrape_tracklist(album_id = 337082)
#' }
#' @export
scrape_tracklist <- function(album_id, access_token = genius_token()) {

  .Deprecated("get_album_tracklist")

  check_internet()

  # get album data
  album <- genius_get_album(album_id)

  # start session
  session <- read_html(album$url)

  # get track numbers
  song_number <- as.numeric(html_text(
    html_nodes(session, ".chart_row-number_container-number"),
    trim = TRUE
  ))

  # get track titles
  song_title <- html_text(
    html_nodes(session, ".chart_row-content-title"),
    trim = TRUE
    )

  # remove everything after (and including) line break
  song_title <- gsub("\n.*", "", song_title)

  # get lyric links
  song_lyrics_url <- html_nodes(session, ".chart_row-content") %>%
    html_nodes("a") %>%
    html_attr("href")

  # combine album elements into dataframe
  album_meta <- data.frame(song_number, song_title, song_lyrics_url,
                           stringsAsFactors = FALSE)

  # add album meta data
  album_meta$album_id <- album$id
  album_meta$album_name <- album$name
  album_meta$album_url <- album$url
  album_meta$album_cover_art_url <- album$cover_art_url
  album_meta$album_release_date <- album$release_date
  album_meta$artist_id <- album$artist$id
  album_meta$artist_name <- album$artist$name
  album_meta$artist_url <- album$artist$url

  # remove missing song nos
  album_meta <- album_meta[!is.na(album_meta$song_number), ]

  # reorder cols
  album_meta <- album_meta[, c(
    "song_number", "song_title", "song_lyrics_url", "album_name",
    "album_id", "artist_id", "artist_name", "artist_url"
    )]

  as_tibble(album_meta)
}

#' Retrieve an album's tracklisting
#'
#' Get an album's tracklisting, and song meta data, given an album ID.
#'
#' @inheritParams get_album
#'
#' @examples
#' \dontrun{
#' get_album_tracklist(album_id = 337082)
#' }
#' @export
genius_get_album_tracklist <- scrape_tracklist
