#' Retrieve artist info for a search term
#'
#' The Genius API lets you search hosted content (all songs). Use \code{search_artist} to return \code{artist_id} and \code{artist_name} for all matches to a search.
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
      artist_name = tmp$name
    )
  })

  # isolate unique pairs
  return(unique(artist_info))

}
