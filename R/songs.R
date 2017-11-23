#' Retrieve meta data for an artist's discography
#'
#' Return song meta data for all appearances (features optional) of an artist on Genius.
#' @param artist_id An artist ID (\code{artist_id} returned in \code{\link{search_artist}})
#' @param include_features Return results where artist isn't the primary artist (logical, defaults to FALSE)
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' get_artist_songs(artist_id = 1421)
#' @export
get_artist_songs <- function(artist_id, include_features=FALSE, access_token=genius_token()) {

  pri_artist_id <- artist_id

  # base URL
  base_url <- "api.genius.com/artists/"

  track_lyric_urls <- list()

  i <- 1

  while (i > 0) {
  # search for artist's songs
  req <- httr::GET(url = paste0(base_url, artist_id, "/songs", '?per_page=', 10, '&page=', i),
                   httr::add_headers(Authorization=paste0("Bearer ", access_token)))

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
  track_lyrics <- bind_rows(track_lyric_urls)

  # keep / discard features
  if (include_features == FALSE) {

    track_lyrics <- dplyr::filter(track_lyrics, `artist_id` == pri_artist_id)

  } else NULL

  return(dplyr::as_tibble(track_lyrics))

}
