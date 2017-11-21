#' Retrieve artist identifiers for a search term
#'
#' The Genius API lets you search hosted content (all songs). Use \code{search_artist} to
#' return \code{artist_id}, \code{artist_name} and \code{artist_url} for all matches to a search.
#' @param search_term A character string to search for artist matches.
#' @param n_results Number of results to return.
#' @examples
#' search_artist(search_term = "Kanye")
#' @export
search_artist <- function(search_term, n_results=10) {

  # base URL
  base_url <- "api.genius.com/search?q="

  # replace spaces with %20
  search_term <- gsub(" ", "%20", search_term)

  # search for term
  req <- httr::GET(url = paste0(base_url, search_term, '&per_page=',
                          n_results, '&access_token=', genius_token()))

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response$hits

  # extract artist info from returned results
  artist_info <- purrr::map_df(1:length(res), function(x) {
    tmp <- res[[x]]$result$primary_artist
    list(
      artist_id = tmp$id,
      artist_name = tmp$name,
      artist_url = tmp$url
    )
  })

  # isolate unique pairs
  return(unique(artist_info))

}



#' Retrieve song identifiers for a search term
#'
#' The Genius API lets you search hosted content (all songs). Use \code{search_song} to
#' return \code{song_id}, \code{song_name}, \code{lyrics_url} and
#' \code{artist_id} for all matches to a search.
#' @param search_term A character string to search for song matches.
#' @param n_results Number of results to return.
#' @examples
#' search_song(search_term = "Ferrari")
#' @export
search_song <- function(search_term, n_results=10) {

  # base URL
  base_url <- "api.genius.com/search?q="

  # replace spaces with %20
  search_term <- gsub(" ", "%20", search_term)

  # search for term
  req <- httr::GET(url = paste0(base_url, search_term, '&per_page=',
                                n_results, '&access_token=', genius_token()))

  # extract request content
  res <- httr::content(req)

  # drill down
  res <- res$response$hits

  # extract song and artist info from returned results
  song_info <- purrr::map_df(1:length(res), function(x) {
    trk <- res[[x]]$result
    tmp <- res[[x]]$result$primary_artist
    list(
      song_id = trk$id,
      song_name = trk$title_with_featured,
      song_url = trk$url,
      artist_id = tmp$id
    )
  })

  # isolate unique pairs
  return(unique(song_info))

}
