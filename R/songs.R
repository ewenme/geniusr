#' Retrieve metadata for a song
#'
#' The Genius API lets you return data for a specific song, given a song ID.
#' \code{get_song} returns this data in full.
#'
#' @seealso [get_song_df()] to return a tidy data frame.
#'
#' @param song_id ID of the song (\code{song_id} within an object returned by
#' \code{\link{search_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a list
#'
#' @examples
#' \dontrun{
#' get_song(song_id = 3039923)
#' }
#' @export
get_song <- function(song_id, access_token = genius_token()) {

  check_internet()

  path <- sprintf("api.genius.com/songs/%s", song_id)

  # request track
  req <- genius_get(url = path, access_token)

  stop_for_status(req)

  res <- content(req)

  res$response$song
}

#' Retrieve metadata for a song
#'
#' The Genius API lets you search for meta data for a song, given a song ID.
#' \code{get_song_meta} returns this data in a tidy, but reduced, format.
#'
#' @seealso [get_song()] to return data in full as a list.
#'
#' @inheritParams get_song
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_song_df(song_id = 3039923)
#' }
#'
#' @export
get_song_df <- function(song_id, access_token = genius_token()) {

  # pull song meta
  song <- get_song(song_id, access_token)

  # grab album, artist, stat data
  album <- song$album
  artist <- song$primary_artist
  stats <- song$stats

  # make list for song_info
  song_info <- list(
    song_id = song$id,
    song_name = song$title_with_featured,
    song_lyrics_url = song$url,
    song_art_image_url = song$song_art_image_url,
    song_release_date = song$release_date,
    song_pageviews = stats$pageviews,
    song_annotation_count = song$annotation_count,
    artist_id = artist$id,
    artist_name = artist$name,
    artist_url = artist$url,
    album_id = album$id,
    album_name = album$name,
    album_url = album$url
  )

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(song_info, is.null)))
  for(i in ndxNULL){ song_info[[i]] <- NA }

  as_tibble(song_info)
}
