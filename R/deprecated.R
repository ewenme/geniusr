#' Retrieve metadata for an album
#'
#' The Genius API lets you return data for a specific album, given an album ID.
#' \code{get_album_meta} returns this data in a tidy, but reduced, format.
#'
#' @seealso [get_album()] to return data in full as a list.
#'
#' @inheritParams get_album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_album_meta(album_id = 337082)
#' }
#' @export
get_album_meta <- function(album_id, access_token = genius_token()) {

  .Deprecated("get_album_df")

  # pull album meta
  album <- get_album(album_id, access_token)

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

#' Retrieve an album's tracklisting
#'
#' Get an album's tracklisting, and song meta data, given an album ID.
#'
#' @inheritParams get_album
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' scrape_tracklist(album_id = 337082)
#' }
#' @export
scrape_tracklist <- function(album_id, access_token = genius_token()) {

  .Deprecated("get_album_tracklist_id")

  check_internet()

  # get album data
  album <- get_album(album_id, access_token)

  # start session
  session <- read_html(album$url)

  # get tracklist
  album_tracks <- get_tracklist(session)

  # add album meta data
  album_tracks$album_id <- album$id
  album_tracks$album_name <- album$name
  album_tracks$album_url <- album$url
  album_tracks$album_cover_art_url <- album$cover_art_url
  album_tracks$album_release_date <- album$release_date
  album_tracks$artist_id <- album$artist$id
  album_tracks$artist_name <- album$artist$name
  album_tracks$artist_url <- album$artist$url

  # reorder cols
  album_tracks <- album_tracks[, c(
    "song_number", "song_title", "song_lyrics_url", "album_name",
    "album_id", "artist_id", "artist_name", "artist_url"
  )]

  as_tibble(album_tracks)

}

#' Retrieve metadata for an artist
#'
#' The Genius API lets you search for meta data for an artist, given an
#' artist ID. \code{get_artist_meta} returns this data in a tidy, but
#' reduced, format.
#'
#' @seealso [get_artist()] to return data in full as a list.
#'
#' @inheritParams get_artist
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_artist_meta(artist_id = 16751)
#' }
#' @export
get_artist_meta <- function(artist_id, access_token = genius_token()) {

  .Deprecated("get_artist_df")

  # pull artist meta
  artist <- get_artist(artist_id, access_token)

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

#' Retrieve lyrics associated with a Genius song ID
#'
#' Get lyrics from Genius' lyric pages using an associated song ID.
#'
#' @inheritParams get_song
#'
#' @examples
#' \dontrun{
#' scrape_lyrics_id(song_id = 3214267)
#' }
#' @export
scrape_lyrics_id <- function(song_id, access_token = genius_token()) {

  .Deprecated("get_lyrics_id")

  check_internet()

  # get song meta data
  song <- get_song(song_id, access_token)

  # start session
  session <- read_html(song$url)

  # get song lyrics
  lyrics <- get_lyrics(session)

  # add fields
  lyrics$song_id <- song_id

  lyrics
}

#' Retrieve lyrics associated with a Genius lyrics page URL
#'
#' Scrape lyrics from a Genius' lyric page using it's associated URL. Best used with \code{\link{scrape_tracklist}}, when song IDs aren't returned - otherwise, \code{\link{scrape_lyrics_id}} is recommended.
#'
#' @param song_lyrics_url song lyrics url (like in \code{song_lyrics_url} returned by \code{\link{get_song_meta}})
#'
#' @examples
#' \dontrun{
#' scrape_lyrics_url(song_lyrics_url = "https://genius.com/Kendrick-lamar-dna-lyrics")
#' }
#' @export
scrape_lyrics_url <- function(song_lyrics_url) {

  .Deprecated("get_lyrics_url")

  check_internet()

  # start session
  session <- read_html(song_lyrics_url)

  # get song lyrics
  lyrics <- get_lyrics(session)

  lyrics$song_lyrics_url <- song_lyrics_url

  lyrics

}

#' Retrieve metadata for a song
#'
#' The Genius API lets you search for meta data for a song, given a song ID.
#' \code{get_song_meta} returns this data in a tidy, but reduced, format.
#'
#' @seealso [get_song()] to return data in full as a list.
#'
#' @inheritParams get_song
#'
#' @return a tibble
#'
#' @examples
#' \dontrun{
#' get_song_meta(song_id = 3039923)
#' }
#' @export
get_song_meta <- function(song_id, access_token = genius_token()) {

  .Deprecated("get_song_df")

  # pull song meta
  song <- get_song(song_id, access_token)

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
