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

  get_album_df(album_id, access_token)

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

  get_album_tracklist_id(album_id, access_token)
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

  get_artist_df(artist_id, access_token)

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

  get_lyrics_id(song_id, access_token)
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

  get_lyrics_url(song_lyrics_url)

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

  get_song_df(song_id, access_token)
}
