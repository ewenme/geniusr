#' Retrieve metadata for an artist
#'
#' The Genius API lets you return data for a specific artist, given an artist ID.
#' \code{gen_get_artist} returns this data in full.
#'
#' @seealso [gen_get_artist_df()] to return a tidy data frame.
#'
#' @param artist_id ID of the artist (\code{artist_id} within an object returned by
#' \code{\link{search_artist}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a list
#'
#' @examples
#' \dontrun{
#' gen_get_artist(artist_id = 16775)
#' }
#' @export
gen_get_artist <- function(artist_id, access_token = genius_token()) {

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
#' artist ID. \code{get_artist_meta} returns this data in a tidy, but
#' reduced, format.
#'
#' @seealso [gen_get_artist()] to return data in full as a list.
#'
#' @inheritParams gen_get_artist
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_artist_meta(artist_id = 16751)
#' }
#' @export
get_artist_meta <- function(artist_id, access_token = genius_token()) {

  .Deprecated("gen_get_artist_df")

  # pull artist meta
  artist <- gen_get_artist(artist_id, access_token)

  # make list for artist_info
  artist_info <- list(
    artist_id = artist$id,
    artist_name = artist$name,
    artist_url = artist$url,
    artist_image_url = artist$image_url,
    artist_followers_count = artist$followers_count,
    artist_twitter_name = artist$twitter_name
    )

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(artist_info, is.null)))
  for(i in ndxNULL){ artist_info[[i]] <- NA }

  as_tibble(artist_info)
}

#' Retrieve metadata for an artist
#'
#' The Genius API lets you search for meta data for an artist, given an
#' artist ID. \code{gen_get_artist_df} returns this data in a tidy, but
#' reduced, format.
#'
#' @seealso [gen_get_artist()] to return data in full as a list.
#'
#' @inheritParams gen_get_artist
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' gen_get_artist_df(artist_id = 16751)
#' }
#' @export
gen_get_artist_df <- get_artist_meta

#' Retrieve metadata for all of an artist's songs
#'
#' The Genius API lets you search for song metadata of an artist,
#' given an artist ID. \code{gen_get_artist_songs} returns this data
#' in full.
#'
#' @seealso [gen_get_artist_songs_df()] to return a tidy data frame.
#'
#' @inheritParams gen_get_artist
#' @param sort method to order results; by "title" (default) or by
#' "popularity"
#' @param include_features Whether to return results where artist
#' isn't the primary artist (logical, defaults to FALSE)
#'
#' @return a list
#'
#' @examples
#' \dontrun{
#' gen_get_artist_songs(artist_id = 1421)
#' gen_get_artist_songs(artist_id = 1421, sort = "popularity")
#' gen_get_artist_songs(artist_id = 1421, include_features = TRUE)
#' }
#' @export
gen_get_artist_songs <- function(artist_id,
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

    # extract request content
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

    songs[is_primary_artist]
  }
}

#' Retrieve metadata for all of an artist's songs
#'
#' The Genius API lets you search for song metadata of an artist,
#' given an artist ID. \code{get_artist_songs} returns this data
#' in a tidy, but reduced, format.
#'
#' @seealso [gen_get_artist_songs()] to return data in full as a
#' list.
#'
#' @inheritParams gen_get_artist
#' @inheritParams gen_get_artist_songs
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_artist_songs(artist_id = 1421)
#' }
#' @export
get_artist_songs <- function(artist_id,
                             sort = c("title", "popularity"),
                             include_features = FALSE,
                             access_token = genius_token()) {

  .Deprecated("gen_get_artist_songs_df")

  # pull artist discography
  songs <- gen_get_artist_songs(artist_id, sort,
                                include_features, access_token)

  # extract track info from returned results
  song_info <- map_df(seq_along(songs), function(x) {
    tmp <- songs[[x]]
    art <- songs[[x]]$primary_artist
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

  as_tibble(song_info)
}

#' Retrieve metadata for all of an artist's songs
#'
#' The Genius API lets you search for song metadata of an artist,
#' given an artist ID. \code{get_artist_songs} returns this data
#' in a tidy, but reduced, format.
#'
#' @seealso [gen_get_artist_songs()] to return data in full as a
#' list.
#'
#' @inheritParams gen_get_artist
#' @inheritParams gen_get_artist_songs
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' gen_get_artist_songs_df(artist_id = 1421)
#' }
#' @export
gen_get_artist_songs_df <- get_artist_songs
