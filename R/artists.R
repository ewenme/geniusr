#' Retrieve metadata for an artist
#'
#' The Genius API lets you return data for a specific artist, given an artist ID.
#' \code{get_artist} returns this data in a relatively untouched state.
#'
#' @param artist_id ID of the artist (\code{artist_id} within an object returned by
#' \code{\link{search_artist}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @examples
#' \dontrun{
#' get_artist(artist_id = 16775)
#' }
#' @export
get_artist <- function(artist_id, access_token = genius_token()) {

  check_internet()

  path <- sprintf("api.genius.com/artists/%s", artist_id)

  # request track
  req <- genius_get(url = path, access_token)

  stop_for_status(req)

  # extract request content
  res <- content(req)

  res$response$artist
}

#' Retrieve metadata for an artist
#'
#' The Genius API lets you search for meta data for an artist, given an
#' artist ID. \code{get_artist_meta} returns this data in a reduced data frame
#' (see \code{get_artist} to return extended metadata).
#'
#' @inheritParams get_artist
#'
#' @examples
#' \dontrun{
#' get_artist_meta(artist_id = 16751)
#' }
#' @export
get_artist_meta <- function(artist_id, access_token=genius_token()) {

  # pull artist meta
  artist <- get_artist(artist_id, access_token)

  # make list for artist_info
  artist_info <- list(
    artist_id = artist$id,
    artist_name = artist$name,
    artist_url = artist$url,
    artist_image_url = artist$image_url,
    followers_count = artist$followers_count
    )

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(artist_info, is.null)))
  for(i in ndxNULL){ artist_info[[i]] <- NA }

  as_tibble(artist_info)
}
