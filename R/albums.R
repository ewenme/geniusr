# set custom genius album class
as_genius_album <- function(content, path, response) {

  structure(
    list(
      content = content,
      path = path,
      response = response
    ),
    class = "genius_album"
  )
}

#' Slightly more human-readable output for genius_album objects
#'
#' @param x a genius_album object
#' @param ... ignored
#' @export
print.genius_album <- function(x, ...) {

  cat(x$content$full_title, " <", x$path, ">\n", sep = "")
  utils::str(x$content, max=1)
  invisible(x)

}

#' Retrieve metadata for an album
#'
#' The Genius API lets you request data for a specific album, given an album ID.
#' \code{get_album()} returns this data in full.
#'
#' @family album
#' @seealso See \code{\link{get_album_df}} to return a tidy data frame.
#'
#' @param album_id ID of the album (\code{album_id} within an object returned by
#' \code{\link{get_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a \code{genius_album} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @examples
#' \dontrun{
#' get_album(album_id = 337082)
#' }
#' @export
get_album <- function(album_id, access_token = genius_token()) {

  check_internet()

  path <- sprintf("api.genius.com/albums/%s", album_id)

  # request track
  req <- genius_get(url = path, access_token)

  stop_for_status(req)

  res <- content(req)

  as_genius_album(
    res$response$album, path, req
  )
}

#' Convert genius_album object to a data frame
#'
#' @param x a \code{genius_album} object
#'
#' @return a tibble
#' @export
#'
#'
#' @examples
#' \dontrun{
#' album <- get_album(album_id = 337082)
#' album_to_df(album)
#' }
#'
album_to_df <- function(x) {

  stopifnot(is_genius_album(x))

  # pull album meta
  album <- x$content
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
#' @family album
#' @seealso See \code{\link{get_album}} to return extended data as a list.
#'
#' @inheritParams get_album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_album_df(album_id = 337082)
#' }
#' @export
get_album_df <- function(album_id, access_token = genius_token()) {

  # pull album meta
  album <- get_album(album_id, access_token)

  album_to_df(album)

}

# get tracklist of an album
get_tracklist <- function(session) {

  # get track numbers
  song_number <- as.numeric(html_text(
    html_nodes(session, ".chart_row-number_container-number"),
    trim = TRUE
  ))

  # get track titles
  song_title <- html_text(
    html_nodes(session, ".chart_row-content-title"),
    trim = TRUE
  )

  # remove everything after (and including) line break
  song_title <- gsub("\n.*", "", song_title)

  # get lyric links
  song_lyrics_url <- html_nodes(session, ".chart_row-content") %>%
    html_nodes("a") %>%
    html_attr("href")

  # combine album elements into dataframe
  album_meta <- data.frame(
    song_number, song_title, song_lyrics_url,
    stringsAsFactors = FALSE
  )

  # remove missing song nos
  album_meta <- album_meta[!is.na(album_meta$song_number), ]

  album_meta
}

#' Retrieve an album's tracklisting
#'
#' Get an album's tracklisting, and song meta data, given an album ID.
#'
#' @family album
#' @seealso See \code{\link{get_album_tracklist_search}} to search for
#' an album tracklist by searching artist/album names.
#'
#' @inheritParams get_album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_album_tracklist_id(album_id = 337082)
#' }
#' @export
get_album_tracklist_id <- function(album_id, access_token = genius_token()) {

  check_internet()

  # get album data
  album <- get_album(album_id, access_token)

  # start session
  session <- read_html(album$content$url)

  # get tracklist
  album_tracks <- get_tracklist(session)

  # add album meta data
  album_tracks$album_id <- album$content$id
  album_tracks$album_name <- album$content$name
  album_tracks$album_url <- album$content$url
  album_tracks$album_cover_art_url <- album$content$cover_art_url
  album_tracks$album_release_date <- album$content$release_date
  album_tracks$artist_id <- album$content$artist$id
  album_tracks$artist_name <- album$content$artist$name
  album_tracks$artist_url <- album$content$artist$url

  # reorder cols
  album_tracks <- album_tracks[, c(
    "song_number", "song_title", "song_lyrics_url", "album_name",
    "album_id", "artist_id", "artist_name", "artist_url"
  )]

  as_tibble(album_tracks)

}

#' Retrieve an album's tracklisting
#'
#' Attempt to get an album's tracklisting, given an artist
#' and album name.
#'
#' @family album
#' @seealso See \code{\link{get_album_tracklist_id}} to search for
#' an album tracklist using an album ID.
#'
#' @param artist_name Name of artist
#' @param album_name Name of album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_album_tracklist_search(artist_name = "Kendrick Lamar",
#' album_name = "DAMN.")
#' }
#' @export
get_album_tracklist_search <- function(artist_name, album_name) {

  # remove bad artist/album strings
  artist_name <- str_replace_all(artist_name, bad_lyric_strings)
  album_name <- str_replace_all(album_name, bad_lyric_strings)

  # construct search path
  path <- sprintf("https://genius.com/albums/%s/%s",
                  artist_name, album_name)

  # start session
  session <- read_html(path)

  # get tracklist
  album_tracks <- get_tracklist(session)

  as_tibble(album_tracks)

}

#' Extract album performances from a Genius album
#'
#' Extract "album performances" (i.e. album credits) info from a Genius album object,
#' as a tidy tibble.
#'
#' @family song
#' @seealso See \code{\link{get_album}} to generate a Genius album object.
#'
#' @param x A \code{genius_album} object
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' album <- get_album(album_id = 337082)
#'
#' tidy_album_performances(album)
#' }
#'
#' @export
tidy_album_performances <- function(x) {

  stopifnot(is_genius_album(x))

  credits <- map_dfr(x$content$song_performances, function(x) {

    people <- map_dfr(x$artists, function(y) dplyr::bind_rows(y))

    if (nrow(people) == 0) return(people)

    people$label <- x$label

    people
  })

  credits <- prefix_colnames(credits, "album_performances")

  credits$album_id <- x$content$id

  select(credits, album_id, album_performances_label,
         album_performances_name, everything())

}
