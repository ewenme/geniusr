get_lyrics <- function(session) {

  # read lyrics
  lyrics <- html_nodes(session, ".lyrics p")

  # ensure line breaks are preserved correctly
  xml_find_all(lyrics, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% xml_remove()

  # get plain text lyrics
  lyrics <- html_text(lyrics)

  # split on line break
  lyrics <- unlist(strsplit(lyrics, split = "\n"))

  # remove empty strings
  lyrics <- lyrics[lyrics != ""]

  # remove lines with square brackets
  lyrics <- lyrics[!grepl(pattern = "\\[|\\]", lyrics)]

  # error handling for instrumental songs, writes NA if there are no lyrics
  if (is_empty(lyrics)) {
    lyrics[1] <- NA
  }

  lyrics
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

  tibble(
    line = lyrics,
    song_id = song$id,
    song_name = song$title_with_featured,
    artist_id = song$primary_artist$id,
    artist_name = song$primary_artist$name
  )
}

#' Retrieve lyrics associated with a Genius song ID
#'
#' Get lyrics from Genius' lyric pages using an associated song ID.
#'
#' @inheritParams get_song
#'
#' @examples
#' \dontrun{
#' get_lyrics_id(song_id = 3214267)
#' }
#' @export
get_lyrics_id <- scrape_lyrics_id

#' Retrieve lyrics associated with a Genius lyrics page URL
#'
#' Scrape lyrics from a Genius' lyric page using it's associated URL. Best used with \code{\link{scrape_tracklist}}, when song IDs aren't returned - otherwise, \code{\link{scrape_lyrics_id}} is recommended.
#'
#' @param song_lyrics_url song lyrics url (like in \code{song_lyrics_url} returned by \code{\link{get_song_meta}})
#' @inheritParams get_song
#'
#' @examples
#' \dontrun{
#' scrape_lyrics_url(song_lyrics_url = "https://genius.com/Kendrick-lamar-dna-lyrics")
#' }
#' @export
scrape_lyrics_url <- function(song_lyrics_url, access_token = genius_token()) {

  .Deprecated("get_lyrics_url")

  check_internet()

  # start session
  session <- read_html(song_lyrics_url)

  # get song lyrics
  lyrics <- get_lyrics(session)

  # get meta data
  song <- html_nodes(session, ".header_with_cover_art-primary_info-title") %>%
    html_text()

  artist <- html_nodes(session, ".header_with_cover_art-primary_info-primary_artist") %>%
    html_text()

  tibble(
    line = lyrics,
    song_lyrics_url = song_lyrics_url,
    song_name = song,
    artist_name = artist
    )
}

#' Retrieve lyrics associated with a Genius lyrics page URL
#'
#' Scrape lyrics from a Genius' lyric page using it's associated URL. Best used with \code{\link{scrape_tracklist}}, when song IDs aren't returned - otherwise, \code{\link{scrape_lyrics_id}} is recommended.
#'
#' @param song_lyrics_url song lyrics url (like in \code{song_lyrics_url} returned by \code{\link{get_song_meta}})
#' @inheritParams get_song
#'
#' @examples
#' \dontrun{
#' get_lyrics_url(song_lyrics_url = "https://genius.com/Kendrick-lamar-dna-lyrics")
#' }
#' @export
get_lyrics_url <- scrape_lyrics_url
