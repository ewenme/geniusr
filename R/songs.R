# set custom genius song class
as_genius_song <- function(content, path, response) {

  structure(
    list(
      content = content,
      path = path,
      response = response
    ),
    class = "genius_song"
  )
}

#' Slightly more human-readable output for genius_song objects
#'
#' @param x a genius_song object
#' @param ... ignored
#' @export
print.genius_song <- function(x, ...) {

  cat(x$content$full_title, " <", x$path, ">\n", sep = "")
  utils::str(x$content, max=1)
  invisible(x)

}

#' Retrieve metadata for a song
#'
#' The Genius API lets you return data for a specific song, given a song ID.
#' \code{get_song} returns this data in full.
#'
#' @family song
#' @seealso See \code{\link{get_song_df}} to return a tidy data frame.
#'
#' @param song_id ID of the song (\code{song_id} within an object returned by
#' \code{\link{search_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a \code{genius_song} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @examples
#' \dontrun{
#' get_song(song_id = 3039923)
#' }
#' @export
get_song <- function(song_id, access_token = genius_token()) {

  check_internet()

  path <- sprintf("api.genius.com/songs/%s", song_id)

  # request track
  req <- genius_get(url = path, access_token)

  stop_for_status(req)

  res <- content(req)

  as_genius_song(
    res$response$song, path, req
  )
}

# function to convert song object to data frame
song_to_df <- function(x) {

  song <- x

  # grab album, artist, stat data
  album <- song$album
  artist <- song$primary_artist
  stats <- song$stats

  # make list for song_info
  song_info <- list(
    song_id = song$id,
    song_name = song$title_with_featured,
    song_lyrics_url = song$url,
    song_art_image_url = song$song_art_image_url,
    song_release_date = song$release_date,
    song_pageviews = stats$pageviews,
    song_annotation_count = song$annotation_count,
    artist_id = artist$id,
    artist_name = artist$name,
    artist_url = artist$url,
    album_id = album$id,
    album_name = album$name,
    album_url = album$url
  )

  # find list indices of NULL values, change to NA
  ndxNULL <- which(unlist(lapply(song_info, is.null)))
  for(i in ndxNULL){ song_info[[i]] <- NA }

  as_tibble(song_info)
}

#' Retrieve metadata for a song
#'
#' The Genius API lets you search for meta data for a song, given a song ID.
#' \code{get_song_meta} returns this data in a tidy, but reduced, format.
#'
#' @family song
#' @seealso See \code{\link{get_song}} to return data in full as a list.
#'
#' @inheritParams get_song
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_song_df(song_id = 3039923)
#' }
#'
#' @export
get_song_df <- function(song_id, access_token = genius_token()) {

  # pull song meta
  song <- get_song(song_id, access_token)$content

  song_to_df(song)
}


#' Extract song relationships from a Genius song
#'
#' Extract "song relationships" info from a Genius song object, as a tidy tibble.
#'
#' @family song
#' @seealso See \code{\link{get_song}} to generate a Genius song object.
#'
#' @inheritParams get_song
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' song_data <- get_song(song_id = 3039923)
#'
#' tidy_song_relationships(song_data)
#' }
#'
#' @export
tidy_song_relationships <- function(x) {

  relationships <- map_dfr(x$content$song_relationships, function(x) {

    songs <- map_dfr(x$songs, song_to_df)

    if (nrow(songs) == 0) return(songs)

    songs$type <- x$type

    songs
  })

  colnames(relationships) <- paste("song_relationships", colnames(relationships), sep = "_")

  relationships$song_id <- x$content$id

  relationships <- relationships[,c(ncol(relationships),1:(ncol(relationships)-1))]

  relationships
}
