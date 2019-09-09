#' Retrieve metadata for an album
#'
#' The Genius API lets you request data for a specific album, given an album ID.
#' \code{gen_get_album()} returns this data in full.
#'
#' @seealso [gen_get_album_df()] to return a tidy data frame.
#'
#' @param album_id ID of the album (\code{album_id} within an object returned by
#' \code{\link{gen_get_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a list
#'
#' @examples
#' \dontrun{
#' gen_get_album(album_id = 337082)
#' }
#' @export
gen_get_album <- function(album_id, access_token = genius_token()) {

  check_internet()

  path <- sprintf("api.genius.com/albums/%s", album_id)

  # request track
  req <- genius_get(url = path, access_token)

  stop_for_status(req)

  # extract request content
  res <- content(req)

  res$response$album
}

#' Retrieve metadata for an album
#'
#' The Genius API lets you return data for a specific album, given an album ID.
#' \code{get_album_meta} returns this data in a tidy, but reduced, format.
#'
#' @seealso [gen_get_album()] to return data in full as a list.
#'
#' @inheritParams gen_get_album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_album_meta(album_id = 337082)
#' }
#' @export
get_album_meta <- function(album_id, access_token = genius_token()) {

  .Deprecated("gen_get_album_df")

  # pull album meta
  album <- genius_get_album(album_id, access_token)

  artist <- album$artist

  # make list for album_info
  album_info <- list(
    album_id = album$id,
    album_name = album$name,
    album_url = album$url,
    album_cover_art_url = album$cover_art_url,
    album_release_date = album$release_date,
    album_comment_count = album$comment_count,
    song_pageviews = album$song_pageviews,
    artist_id = artist$id,
    artist_name = artist$name,
    artist_url = artist$url
    )

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(album_info, is.null)))
  for(i in ndxNULL){ album_info[[i]] <- NA }

  as_tibble(album_info)

}

#' Retrieve meta data for an album
#'
#' The Genius API lets you return data for a specific album, given an album ID.
#' \code{get_album_meta} returns this data in a tidy, but reduced, format.
#'
#' @seealso [gen_get_album()] to return extended data as a list.
#'
#' @inheritParams gen_get_album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' gen_get_album_df(album_id = 337082)
#' }
#' @export
gen_get_album_df <- get_album_meta
