#' Retrieve metadata for a song
#'
#' The Genius API lets you return data for a specific song, given a song ID.
#' \code{get_song} returns this data in a relatively untouched state.
#'
#' @param song_id ID of the song (\code{song_id} within an object returned by
#' \code{\link{search_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
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

  # extract request content
  res <- content(req)

  res$response$song
}

#' Retrieve metadata for a song
#'
#' The Genius API lets you search for meta data for a song, given a song ID.
#' \code{get_song_meta} returns this data in a reduced data frame
#' (see \code{get_song} to return extended metadata).
#'
#' @inheritParams get_song
#'
#' @examples
#' \dontrun{
#' get_song_meta(song_id = 3039923)
#' }
#' @export
get_song_meta <- function(song_id, access_token = genius_token()) {

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
    release_date = song$release_date,
    pageviews = stats$pageviews,
    annotation_count = song$annotation_count,
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

#' Retrieve metadata for a song
#'
#' The Genius API lets you search for meta data for a song, given a song ID.
#' \code{get_song_meta} returns this data in a reduced data frame
#' (see \code{get_song} to return extended metadata).
#'
#' @inheritParams get_song
#'
#' @examples
#' \dontrun{
#' get_song_df(song_id = 3039923)
#' }
#'
#' @export
get_song_df <- get_song_meta

#' Retrieve metadata for all of an artist's songs
#'
#' The Genius API lets you search for song metadata of an artist,
#' given an artist ID. \code{get_artist_songs} returns this data in a stripped-back data frame.
#'
#' @param artist_id An artist ID (\code{artist_id} returned in \code{\link{search_artist}})
#' @param include_features Whether to return results where artist isn't the primary artist (logical, defaults to FALSE)
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @examples
#' \dontrun{
#' get_artist_songs(artist_id = 1421)
#' }
#' @export
get_artist_songs <- function(artist_id, include_features = FALSE,
                             access_token = genius_token()) {

  check_internet()

  pri_artist_id <- artist_id

  # base URL
  base_url <- "api.genius.com/artists/"

  track_lyric_urls <- list()

  i <- 1

  while (i > 0) {
  # search for artist's songs
  req <- GET(url = paste0(base_url, artist_id, "/songs", '?per_page=', 10, '&page=', i),
             add_headers(Authorization=paste0("Bearer ", access_token)))

  stop_for_status(req)

  # extract request content
  res <- content(req)
  res <- res$response

  # extract track info from returned results
  song_info <- map_df(1:length(res$songs), function(x) {
    tmp <- res$songs[[x]]
    art <- res$songs[[x]]$primary_artist
    list(
      song_id = tmp$id,
      song_name = tmp$title_with_featured,
      song_lyrics_url = tmp$url,
      annotation_count = tmp$annotation_count,
      artist_id = art$id,
      artist_name = art$name,
      artist_url = art$url
    )
  })

  track_lyric_urls[[i]] <- song_info

  if (!is.null(res$next_page)) {
    i <- res$next_page
  } else {
    break
  }

  }

  # bind rows of results
  track_lyrics <- do.call("rbind", track_lyric_urls)

  # keep / discard features
  if (include_features == FALSE) {

    track_lyrics <- subset(track_lyrics, `artist_id` == artist_id)

  } else if (include_features == TRUE) NULL

  as_tibble(track_lyrics)
}
