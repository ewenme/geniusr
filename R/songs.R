#' Retrieve metadata for a song
#'
#' The Genius API lets you return data for a specific song, given a song ID.
#' \code{get_song} returns this data in a relatively untouched state.
#'
#' @param song_id ID of the song (\code{song_id} within an object returned by
#' \code{\link{search_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
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
#' @param song_id A song ID (\code{song_id} returned in \code{\link{search_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' get_song_meta(song_id = 3039923)
#' }
#' @export
get_song_meta <- function(song_id, access_token = genius_token()) {

  # pull song meta without request meta
  song_meta <- get_song(song_id, access_token)

  # grab album, artist, stat data
  alb <- song_meta$album
  art <- song_meta$primary_artist
  stat <- song_meta$stats

  # make list for song_info
  song_info <- list(
    song_meta$id, song_meta$title_with_featured,
    song_meta$url, song_meta$song_art_image_url,
    song_meta$release_date,
    stat$pageviews, song_meta$annotation_count,
    art$id, art$name, art$url, alb$id,
    alb$name, alb$url
    )

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(song_info, is.null)))
  for(i in ndxNULL){ song_info[[i]] <- NA }

  # name song_info list
  names(song_info) <- c(
    "song_id", "song_name", "song_lyrics_url", "song_art_image_url",
    "release_date", "pageviews", "annotation_count", "artist_id",
    "artist_name", "artist_url", "album_id", "album_name", "album_url"
    )

  return(as_tibble(song_info))
}

#' Retrieve meta data for all of an artist's appearances on Genius
#'
#' Return meta data for all appearances (features optional) of an artist on Genius.
#' @param artist_id An artist ID (\code{artist_id} returned in \code{\link{search_artist}})
#' @param include_features Whether to return results where artist isn't the primary artist (logical, defaults to FALSE)
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' get_artist_songs(artist_id = 1421)
#' }
#' @export
get_artist_songs <- function(artist_id, include_features = FALSE,
                             access_token = genius_token()) {

  # check for internet
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

  # stop if unexpected request status returned
  stop_for_status(req)

  # extract request content
  res <- content(req)

  # drill down
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

    track_lyrics <- subset(track_lyrics, `artist_id` == pri_artist_id)

  } else if (include_features == TRUE) NULL

  return(as_tibble(track_lyrics))

}
