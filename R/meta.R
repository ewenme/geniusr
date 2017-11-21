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
      lyrics_url = trk$path,
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

album_meta <- function(album_id) {

  # base URL
  base_url <- "api.genius.com/albums/"

}


