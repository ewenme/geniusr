# set custom genius resource class
as_genius_resource <- function(content, path, response) {

  structure(
    list(
      content = content,
      path = path,
      response = response
    ),
    class = "genius_resource"
  )
}

#' Slightly more human-readable output for genius_resource objects
#'
#' @param x a genius_resource object
#' @param ... ignored
#' @export
print.genius_resource <- function(x, ...) {

  cat("<Genius ", x$path, ">\n", sep = "")
  utils::str(x$content, max=1)
  invisible(x)

}

#' Search documents hosted on Genius
#'
#' The Genius API lets you search hosted content (all songs). Use \code{search_genius()}
#' to return hits on for a given search term, in full.
#'
#' @family search
#'
#' @param search_term A character string to search for
#' @param n_results Maximum no. of search results to return
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a \code{genius_resource} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @examples
#' \dontrun{
#' search_genius(search_term = "Lil", n_results = 100)
#' }
#' @export
search_genius <- function(search_term, n_results = 10,
                          access_token = genius_token()) {

  check_internet()

  path <- sprintf("api.genius.com/search?q=%s", search_term)

  results <- list()

  # limit pages returned if results limit set
  if (is.numeric(n_results)) {

    pages_limit <- ceiling(n_results / 10)
  }

  # initiate page counter
  i <- 1

  # while loop for retrieving results
  while (i > 0 & i <= pages_limit) {

    # search for term
    req <- GET(
      url = "https://api.genius.com/search",
      query = list(
        q = search_term, page = i,
        access_token = access_token
        )
      )

    # stop if unexpected request status returned
    stop_for_status(req)

    # extract request content
    res <- content(req)

    results[[i]] <- res$response$hits

    # check if there are any results
    if (length(res$response$hits) > 0) {
      i <- i + 1
    } else {
      break
    }
  }

  # bind result elements
  results <- do.call(c, results)

  results <- sapply(results, "[", "result")

  names(results) <- NULL

  results

  as_genius_resource(
    results, path, req
  )
}

#' Search artists on Genius
#'
#' The Genius API lets you search hosted content (all songs). Use \code{search_artist()}
#' to return \code{artist_id}, \code{artist_name} and \code{artist_url} for all unique
#' artist matches found using a search term.
#'
#' @family search
#'
#' @inheritParams search_genius
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' search_artist(search_term = "Lil", n_results = 20)
#' }
#' @export
search_artist <- function(search_term, n_results = 10,
                          access_token = genius_token()) {

  check_internet()

  results <- search_genius(search_term, n_results, access_token)

  results <- results$content

  # extract artist info from returned results
  artist_info <- lapply(seq_along(results), function(x) {

    artist <- results[[x]]$primary_artist
    tibble(
      artist_id = artist$id,
      artist_name = artist$name,
      artist_url = artist$url
    )
  })

  artist_info <- do.call(rbind, artist_info)

  # isolate unique artists
  as_tibble(unique(artist_info))

}

#' Search songs on Genius
#'
#' The Genius API lets you search hosted content (all songs). Use
#' \code{search_song()} to return \code{song_id}, \code{song_name},
#' \code{lyrics_url} and \code{artist_id} for all unique song matches
#' found using a search term.
#'
#' @family search
#'
#' @inheritParams search_genius
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' search_song(search_term = "Gucci", n_results = 50)
#' }
#' @export
search_song <- function(search_term, n_results = 10,
                        access_token = genius_token()) {

  check_internet()

  results <- search_genius(search_term, n_results, access_token)

  results <- results$content

  # extract song and artist info from returned results
  song_info <- lapply(seq_along(results), function(x) {

    track <- results[[x]]
    artist <- results[[x]]$primary_artist

    tibble(
      song_id = track$id,
      song_name = track$title_with_featured,
      song_lyrics_url = track$url,
      artist_id = artist$id,
      artist_name = artist$name
    )
  })

  song_info <- do.call(rbind, song_info)

  # isolate unique pairs
  as_tibble(unique(song_info))
}
