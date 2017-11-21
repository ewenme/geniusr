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
      song_art_image_url =
      release_date = trk$release_date,
      pageviews = stat$pageviews,
      accepted_annotations = stat$accepted_annotations,
      artist_id = art$id
    )
  })

  # isolate unique pairs
  return(unique(track_info))

}
