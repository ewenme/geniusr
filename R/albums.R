#' Retrieve metadata for an album
#'
#' The Genius API lets you request data for a specific album, given an album ID.
#' \code{get_album()} returns this data in full.
#'
#' @seealso See [get_album_df()] to return a tidy data frame.
#'
#' @param album_id ID of the album (\code{album_id} within an object returned by
#' \code{\link{get_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a list
#'
#' @examples
#' \dontrun{
#' get_album(album_id = 337082)
#' }
#' @export
get_album <- function(album_id, access_token = genius_token()) {

  check_internet()

  path <- sprintf("api.genius.com/albums/%s", album_id)

  # request track
  req <- genius_get(url = path, access_token)

  stop_for_status(req)

  res <- content(req)

  as_genius_resource(
    res$response$album, path, req
  )
}

#' Retrieve meta data for an album
#'
#' The Genius API lets you return data for a specific album, given an album ID.
#' \code{get_album_meta} returns this data in a tidy, but reduced, format.
#'
#' @seealso [get_album()] to return extended data as a list.
#'
#' @inheritParams get_album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_album_df(album_id = 337082)
#' }
#' @export
get_album_df <- function(album_id, access_token = genius_token()) {

  # pull album meta
  album <- get_album(album_id, access_token)

  artist <- album$artist

  # make list for album_info
  album_info <- list(
    album_id = album$id,
    album_name = album$name,
    album_url = album$url,
    album_cover_art_url = album$cover_art_url,
    album_release_date = album$release_date,
    album_comment_count = album$comment_count,
    song_pageviews = album$song_pageviews,
    artist_id = artist$id,
    artist_name = artist$name,
    artist_url = artist$url
  )

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(album_info, is.null)))
  for(i in ndxNULL){ album_info[[i]] <- NA }

  as_tibble(album_info)

}

get_tracklist <- function(session) {

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
  album_meta <- data.frame(
    song_number, song_title, song_lyrics_url,
    stringsAsFactors = FALSE
  )

  # remove missing song nos
  album_meta <- album_meta[!is.na(album_meta$song_number), ]

  album_meta
}

#' Retrieve an album's tracklisting
#'
#' Get an album's tracklisting, and song meta data, given an album ID.
#'
#' @inheritParams get_album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_album_tracklist_id(album_id = 337082)
#' }
#' @export
get_album_tracklist_id <- function(album_id, access_token = genius_token()) {

  check_internet()

  # get album data
  album <- get_album(album_id, access_token)

  # start session
  session <- read_html(album$url)

  # get tracklist
  album_tracks <- get_tracklist(session)

  # add album meta data
  album_tracks$album_id <- album$id
  album_tracks$album_name <- album$name
  album_tracks$album_url <- album$url
  album_tracks$album_cover_art_url <- album$cover_art_url
  album_tracks$album_release_date <- album$release_date
  album_tracks$artist_id <- album$artist$id
  album_tracks$artist_name <- album$artist$name
  album_tracks$artist_url <- album$artist$url

  # reorder cols
  album_tracks <- album_tracks[, c(
    "song_number", "song_title", "song_lyrics_url", "album_name",
    "album_id", "artist_id", "artist_name", "artist_url"
  )]

  as_tibble(album_tracks)

}

#' Retrieve an album's tracklisting
#'
#' Attempt to get an album's tracklisting, given an artist
#' and album name.
#'
#' @param artist_name Name of artist
#' @param album_name Name of album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_album_tracklist_search(artist_name = "Kendrick Lamar",
#' album_name = "DAMN.")
#' }
#' @export
get_album_tracklist_search <- function(artist_name, album_name) {

  # remove bad artist/album strings
  artist_name <- str_replace_all(artist_name, bad_lyric_strings)
  album_name <- str_replace_all(album_name, bad_lyric_strings)

  # construct search path
  path <- sprintf("https://genius.com/albums/%s/%s",
                  artist_name, album_name)

  # start session
  session <- read_html(path)

  # get tracklist
  album_tracks <- get_tracklist(session)

  as_tibble(album_tracks)

}

