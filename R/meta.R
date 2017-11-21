track_meta <- function(track_id) {

  # base URL
  base_url <- "api.genius.com/songs/"

  # search for track
  req <- httr::GET(url = paste0(base_url, track_id),
                   httr::add_headers(Authorization=paste0("Bearer ", genius_token())))

  # extract request content
  res <- httr::content(req)

}
