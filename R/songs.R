artist_songs <- function(artist_id) {

  # base URL
  base_url <- "api.genius.com/artists/"

  track_lyric_urls <- list()

  i <- 1

  while (i > 0) {
  # search for artist
  req <- httr::GET(url = paste0(base_url, artist_id, "/songs", '?per_page=', 50, '&page=', i),
                   httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  # req <- httr::GET(url = paste0(base_url, artist_id, "/songs", '?per_page=', 10, '&page=', 1),
  #                  httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  # extract request content
  req <- httr::content(req)

  # drill down
  req <- req$response

  track_lyric_urls <- c(track_lyric_urls, req$songs)

  if (!is.null(req$next_page)) {
    i <- req$next_page
  } else {
    break
  }

  }

}
