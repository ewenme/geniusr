#' Retrieve artist identifiers for associated search terms
#'
#' The Genius API lets you search hosted content (all songs). Use \code{search_artist} to
#' return \code{artist_id}, \code{artist_name} and \code{artist_url} for all unique artist matches to a search.
#' @param search_term A character string to search for artist matches
#' @param n_results Maximum no. of search results to return (this is the number of hosted content search results, unique artist matches will be smaller)
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' search_artist(search_term = "Lil")
#' }
#' @export
search_artist <- function(search_term, n_results=10, access_token=genius_token()) {

  # base URL
  base_url <- "api.genius.com/search?q="

  # replace spaces with %20
  search_term <- gsub(" ", "%20", search_term)

  # initiate empty list
  artist_results <- list()

  # set max pages to loop through
  n_pages <- n_results / 10

  # initiate page counter
  i <- 1

  # while loop for retrieving results
  while (i > 0 & i <= n_pages) {

  # search for term
  req <- httr::GET(url = paste0(base_url, search_term, '&per_page=', 10, '&page=', i,
                                '&access_token=', access_token))

  # stop if unexpected request status returned
  httr::stop_for_status(req)

  # extract request content
  res <- httr::content(req)

  # check if there are any results
  if (length(res$response$hits) > 0) {
    i <- i + 1
  } else {
    break
  }

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

  # add results to list item
  artist_results[[i]] <- artist_info

  }

  # bind rows of results
  artist_results <- do.call("rbind", artist_results)

  # isolate unique pairs
  return(tibble::as_tibble(unique(artist_results)))

}


#' Retrieve song identifiers for associated search terms
#'
#' The Genius API lets you search hosted content (all songs). Use \code{search_song} to
#' return \code{song_id}, \code{song_name}, \code{lyrics_url} and
#' \code{artist_id} for all unique song matches to a search.
#' @param search_term A character string to search for song matches
#' @param n_results Maximum no. of search results to return
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' search_song(search_term = "Gucci", n_results=50)
#' }
#' @export
search_song <- function(search_term, n_results=10, access_token=genius_token()) {

  # base URL
  base_url <- "api.genius.com/search?q="

  # replace spaces with %20
  search_term <- gsub(" ", "%20", search_term)

  # initiate empty list
  song_results <- list()

  # set max pages to loop through
  n_pages <- n_results / 10

  # initiate page counter
  i <- 1

  # while loop for retrieving results
  while (i > 0 & i <= n_pages) {

  # search for artists related to search term
  req <- httr::GET(url = paste0(base_url, search_term, '&per_page=', 10, '&page=', i,
                                '&access_token=', access_token))

  # stop if unexpected request status returned
  httr::stop_for_status(req)

  # extract request content
  res <- httr::content(req)

  # check if there are any results
  if (length(res$response$hits) > 0) {
    i <- i + 1
  } else {
    break
  }

  # drill down
  res <- res$response$hits

  # extract song and artist info from returned results
  song_info <- purrr::map_df(1:length(res), function(x) {
    trk <- res[[x]]$result
    tmp <- res[[x]]$result$primary_artist
    list(
      song_id = trk$id,
      song_name = trk$title_with_featured,
      song_lyrics_url = trk$url,
      artist_id = tmp$id,
      artist_name = tmp$name
    )
  })

  # add results to list item
  song_results[[i]] <- song_info

  }

  # bind rows of results
  song_results <- do.call("rbind", song_results)

  # isolate unique pairs
  return(tibble::as_tibble(unique(song_results)))

}
