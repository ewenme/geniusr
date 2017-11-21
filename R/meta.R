#' Retrieve meta data for a song
#'
#' The Genius API lets you search for meta data for a song, given a song ID.
#' @param song_id A song ID (\code{song_id} returned in \code{\link{search_song}}).
#' @examples
#' song_meta(song_id = 2290596)
#' @export
song_meta <- function(song_id) {

  # base URL
  base_url <- "api.genius.com/songs/"

  # search for track
  req <- httr::GET(url = paste0(base_url, song_id),
                   httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response

  # extract track info from returned results
  track_info <- purrr::map_df(1:length(res), function(x) {
    trk <- res[[x]]
    alb <- res[[x]]$album
    art <- res[[x]]$primary_artist
    stat <- res[[x]]$stats
    list(
      song_id = trk$id,
      song_name = trk$title_with_featured,
      song_url = trk$url,
      song_art_image_url = trk$song_art_image_url,
      release_date = trk$release_date,
      pageviews = stat$pageviews,
      annotation_count = trk$annotation_count,
      artist_id = art$id,
      album_id = alb$id
    )
  })

  # isolate unique pairs
  return(unique(track_info))

}


#' Retrieve meta data for an artist
#'
#' The Genius API lets you search for meta data for an artist, given an artist ID.
#' @param artist_id An artist ID (\code{artist_id} returned in \code{\link{search_song}}).
#' @examples
#' artist_meta(artist_id = 129372)
#' @export
artist_meta <- function(artist_id) {

  # base URL
  base_url <- "api.genius.com/artists/"

  # search for artist
  req <- httr::GET(url = paste0(base_url, artist_id),
                   httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response

  # extract artist info from returned results
  artist_info <- purrr::map_df(1:length(res), function(x) {
    tmp <- res[[x]]
    list(
      artist_id = tmp$id,
      artist_name = tmp$name,
      artist_url = tmp$url,
      artist_image_url = tmp$image_url,
      followers_count = tmp$followers_count

    )
  })

  # isolate unique pairs
  return(unique(track_info))

}


#' Retrieve meta data for an album
#'
#' The Genius API lets you search for meta data for an album, given an album ID.
#' @param album_id An album ID (\code{album_id} returned in \code{\link{song_meta}}).
#' @examples
#' album_meta(album_id = 129372)
#' @export
album_meta <- function(album_id) {

  # base URL
  base_url <- "api.genius.com/albums/"

  # search for album
  req <- httr::GET(url = paste0(base_url, album_id),
                   httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response

  # extract album info from returned results
  album_info <- purrr::map_df(1:length(res), function(x) {
    tmp <- res[[x]]
    art <- res[[x]]$artist
    list(
      album_id = tmp$id,
      album_name = tmp$name,
      album_url = tmp$url,
      album_cover_art_url = tmp$cover_art_url,
      album_release_date = tmp$release_date,
      pageviews = tmp$song_pageviews,
      artist_id = art$id
    )
  })

}
