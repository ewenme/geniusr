#' Retrieve meta data for an artist
#'
#' The Genius API lets you search for meta data for an artist, given an artist ID.
#' @param artist_id An artist ID (\code{artist_id} returned in \code{\link{search_artist}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' get_artist_meta(artist_id = 16751)
#' }
#' @export
get_artist_meta <- function(artist_id, access_token=genius_token()) {

  # check for internet
  check_internet()

  # base URL
  base_url <- "api.genius.com/artists/"

  # search for artist
  req <- GET(url = paste0(base_url, artist_id),
             add_headers(Authorization=paste0("Bearer ", access_token)))

  # stop if unexpected request status returned
  stop_for_status(req)

  # extract request content
  res <- content(req)

  # drill down
  artist_meta <- res$response$artist

  # make list for artist_info
  artist_info <- list(artist_meta$id,
                      artist_meta$name,
                      artist_meta$url,
                      artist_meta$image_url,
                      artist_meta$followers_count)

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(artist_info, is.null)))
  for(i in ndxNULL){ artist_info[[i]] <- NA }

  # name artist_info list
  names(artist_info) <- c('artist_id', 'artist_name', 'artist_url', 'artist_image_url', 'followers_count')

  return(as_tibble(artist_info))

}


#' Retrieve meta data for an album
#'
#' The Genius API lets you search for an album's meta data, given an album ID.
#' @param album_id An album ID (\code{album_id} returned in \code{\link{get_song_meta}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @examples
#' \dontrun{
#' get_album_meta(album_id = 337082)
#' }
#' @export
get_album_meta <- function(album_id, access_token=genius_token()) {

  # check for internet
  check_internet()

  # base URL
  base_url <- "api.genius.com/albums/"

  # search for album
  req <- GET(url = paste0(base_url, album_id),
             add_headers(Authorization=paste0("Bearer ", access_token)))

  # stop if unexpected request status returned
  stop_for_status(req)

  # extract request content
  res <- content(req)

  # drill down
  album_meta <- res$response$album

  # grab album, artist, stat data
  art <- album_meta$artist

  # make list for album_info
  album_info <- list(album_meta$id,
                     album_meta$name,
                     album_meta$url,
                     album_meta$cover_art_url,
                     album_meta$release_date,
                     album_meta$song_pageviews,
                     art$id,
                     art$name,
                     art$url)

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(album_info, is.null)))
  for(i in ndxNULL){ album_info[[i]] <- NA }

  # name album_info list
  names(album_info) <- c('album_id', 'album_name', 'album_url', 'album_cover_art_url', 'album_release_date',
                        'pageviews','artist_id','artist_name', 'artist_url')

  return(as_tibble(album_info))

}
