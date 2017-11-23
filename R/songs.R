artist_songs <- function(artist_id) {

  # base URL
  base_url <- "api.genius.com/artists/"

  track_lyric_urls <- list()

  i <- 1

  while (i > 0) {
  # search for artist
  req <- httr::GET(url = paste0(base_url, artist_id, "/songs", '&per_page=', 50, '&page=', i),
                   httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  req <- httr::GET(url = paste0(base_url, artist_id, "/songs", '&per_page=', 10, '&page=', 1),
                   httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response

  track_lyric_urls <- c(track_lyric_urls, res$songs)

  if (!is.null(res$next_page)) {
    i <- res$next_page
  } else {
    break
  }

  }

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response

}


track_lyric_urls <- list()
i <- 1
while (i > 0) {
  tmp <- GET(requestURL, query = list(access_token = token, per_page = 50, page = i)) %>% content %>% .$response
  track_lyric_urls <- c(track_lyric_urls, tmp$songs)
  if (!is.null(tmp$next_page)) {
    i <- tmp$next_page
  } else {
    break
  }
}
