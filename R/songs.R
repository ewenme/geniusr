get_artist_songs <- function(artist_id) {

  # base URL
  base_url <- "api.genius.com/artists/"

  track_lyric_urls <- list()

  i <- 1

  while (i > 0) {
  # search for artist
  req <- httr::GET(url = paste0(base_url, artist_id, "/songs", '?per_page=', 50, '&page=', i),
                   httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

    # req <- httr::GET(url = paste0(base_url, artist_id, "/songs", '?per_page=', 50, '&page=', 1),
    #                  httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response

  # extract track info from returned results
  song_info <- purrr::map_df(1:length(res$songs), function(x) {
    tmp <- res$songs[[x]]
    art <- res$songs[[x]]$primary_artist
    stat <- res$songs[[x]]$stats
    list(
      song_id = tmp$id,
      song_name = tmp$title_with_featured,
      song_url = tmp$url,
      annotation_count = tmp$annotation_count,
      # pageviews = stat$pageviews,
      artist_id = art$id,
      artist_name = art$name
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
  foo <- bind_rows(track_lyric_urls)

}
