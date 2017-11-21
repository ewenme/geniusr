track_meta <- function(track_id) {

  # base URL
  base_url <- "api.genius.com/songs/"

  # search for track
  req <- httr::GET(url = paste0(base_url, track_id),
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
    list(
      track_id = trk$id,
      track_name = trk$title_with_featured,
      lyrics_url = trk$path,
      artist_id = art$id
    )
  })

  # isolate unique pairs
  return(unique(track_info))

}
