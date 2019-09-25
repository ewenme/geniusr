#' Retrieve metadata for an artist
#'
#' The Genius API lets you return data for a specific artist, given an artist ID.
#' \code{get_artist} returns this data in full.
#'
#' @family artist
#' @seealso See \code{\link{get_artist_df}} to return a tidy data frame.
#'
#' @param artist_id ID of the artist (\code{artist_id} within an object returned by
#' \code{\link{search_artist}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a \code{genius_resource} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
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

  res <- content(req)

  as_genius_resource(
    res$response$artist, path, req
  )

}

#' Retrieve metadata for an artist
#'
#' The Genius API lets you search for meta data for an artist, given an
#' artist ID. \code{get_artist_df} returns this data in a tidy, but
#' reduced, format.
#'
#' @family artist
#' @seealso See \code{\link{get_artist}} to return data in full as a list.
#'
#' @inheritParams get_artist
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_artist_df(artist_id = 16751)
#' }
#' @export
get_artist_df <- function(artist_id, access_token = genius_token()) {

  # pull artist meta
  artist <- get_artist(artist_id, access_token)

  # make list for artist_info
  artist_info <- list(
    artist_id = artist$content$id,
    artist_name = artist$content$name,
    artist_url = artist$content$url,
    artist_image_url = artist$content$image_url,
    artist_followers_count = artist$content$followers_count,
    artist_twitter_name = artist$content$twitter_name
  )

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(artist_info, is.null)))
  for(i in ndxNULL){ artist_info[[i]] <- NA }

  as_tibble(artist_info)
}

#' Retrieve metadata for all of an artist's songs
#'
#' The Genius API lets you search for song metadata of an artist,
#' given an artist ID. \code{get_artist_songs} returns this data
#' in full.
#'
#' @family artist
#' @seealso See \code{\link{get_artist_songs_df}} to return a tidy data frame.
#'
#' @inheritParams get_artist
#' @param sort method to order results; by "title" (default) or by
#' "popularity"
#' @param include_features Whether to return results where artist
#' isn't the primary artist (logical, defaults to FALSE)
#'
#' @return a \code{genius_resource} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @examples
#' \dontrun{
#' get_artist_songs(artist_id = 1421)
#' get_artist_songs(artist_id = 1421, sort = "popularity")
#' get_artist_songs(artist_id = 1421, include_features = TRUE)
#' }
#' @export
get_artist_songs <- function(artist_id,
                             sort = c("title", "popularity"),
                             include_features = FALSE,
                             access_token = genius_token()) {

  check_internet()

  sort_results <- match.arg(sort)

  # sort results alpha or by pop
  if (sort_results == "title") {
    path <- sprintf("api.genius.com/artists/%s/songs?sort=title", artist_id)
  } else if (sort_results == "popularity") {
    path <- sprintf("api.genius.com/artists/%s/songs?sort=popularity", artist_id)
  }

  songs <- list()

  # start page count
  i <- 1

  # request available pages of artist songs
  while (i > 0) {

    req <- GET(
      url = paste0(path, '&page=', i),
      add_headers(Authorization = paste0("Bearer ", access_token))
      )

    stop_for_status(req)

    res <- content(req)
    res <- res$response

    songs[[i]] <- res

    if (!is.null(res$next_page)) {
      i <- res$next_page
    } else {
      break
    }
  }

  # remove next page element
  songs <- sapply(songs, "[", 1)

  # combine list of lists
  songs <- flatten(songs)

  # remove feature records
  if (include_features) return(songs)
  else {

    # get primary artist IDs
    primary_artist_ids <- sapply(
      seq_along(songs), function(x) songs[[x]]$primary_artist$id
      )

    # get position of artist = primary artist
    is_primary_artist <- primary_artist_ids == artist_id

    songs <- songs[is_primary_artist]
  }

  as_genius_resource(
    songs, path, req
  )
}

#' Retrieve metadata for all of an artist's songs
#'
#' The Genius API lets you search for song metadata of an artist,
#' given an artist ID. \code{get_artist_songs_df} returns this data
#' in a tidy, but reduced, format.
#'
#' @family artist
#' @seealso See \code{\link{get_artist_songs}} to return data in full as a
#' list.
#'
#' @inheritParams get_artist
#' @inheritParams get_artist_songs
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_artist_songs_df(artist_id = 1421)
#' }
#' @export
get_artist_songs_df <- function(artist_id,
                                sort = c("title", "popularity"),
                                include_features = FALSE,
                                access_token = genius_token()) {

  # pull artist discography
  songs <- get_artist_songs(artist_id, sort,
                            include_features, access_token)

  songs <- songs$content

  # extract track info from returned results
  song_info <- lapply(seq_along(songs), function(x) {
    tmp <- songs[[x]]
    art <- songs[[x]]$primary_artist
    tibble(
      song_id = tmp$id,
      song_name = tmp$title_with_featured,
      song_lyrics_url = tmp$url,
      annotation_count = tmp$annotation_count,
      artist_id = art$id,
      artist_name = art$name,
      artist_url = art$url
    )
  })

  song_info <- do.call(rbind, song_info)

  as_tibble(song_info)
}
