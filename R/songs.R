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
get_artist_songs <- function(artist_id, include_features=FALSE, access_token=genius_token()) {

  # check for internet
  check_internet()

  pri_artist_id <- artist_id

  # base URL
  base_url <- "api.genius.com/artists/"

  track_lyric_urls <- list()

  i <- 1

  while (i > 0) {
  # search for artist's songs
  req <- httr::GET(url = paste0(base_url, artist_id, "/songs", '?per_page=', 10, '&page=', i),
                   httr::add_headers(Authorization=paste0("Bearer ", access_token)))

  # stop if unexpected request status returned
  httr::stop_for_status(req)

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response

  # extract track info from returned results
  song_info <- purrr::map_df(1:length(res$songs), function(x) {
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

  return(tibble::as_tibble(track_lyrics))

}
