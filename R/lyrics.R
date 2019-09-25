get_lyrics <- function(session) {

  # read lyrics
  lyrics <- html_nodes(session, ".lyrics p")

  # get meta data
  song <- html_nodes(session, ".header_with_cover_art-primary_info-title") %>%
    html_text()

  artist <- html_nodes(session, ".header_with_cover_art-primary_info-primary_artist") %>%
    html_text()

  # ensure line breaks are preserved correctly
  xml_find_all(lyrics, ".//br") %>% xml_add_sibling("p", "\n")
  xml_find_all(lyrics, ".//br") %>% xml_remove()

  # get plain text lyrics
  lyrics <- html_text(lyrics, trim = TRUE)

  # split on line break
  lyrics <- unlist(strsplit(lyrics, split = "\n"))

  # keep lines w content
  lyrics <- grep(pattern = "[[:alnum:]]", lyrics, value = TRUE)

  # error handling for instrumental songs, writes NA if no lyrics
  if (is_empty(lyrics)) {
    return(tibble(
      line = NA,
      section_name = NA,
      section_artist = NA,
      song_name = song,
      artist_name = artist
    ))
  }

  # identify section tags
  section_tags <- grepl(pattern = "\\[|\\]", lyrics)

  # repeat them across sections they apply to
  sections <- repeat_before(lyrics, section_tags)

  # remove square brackets
  sections <- gsub("\\[|\\]", "", sections)

  # separate section meta data
  sections <- strsplit(sections, split = ": ", fixed = TRUE)
  section_name <- sapply(sections, "[", 1)
  section_artist <- sapply(sections, "[", 2)

  section_artist[is.na(section_artist)] <- artist

  tibble(
    line = lyrics[!section_tags],
    section_name = section_name[!section_tags],
    section_artist = section_artist[!section_tags],
    song_name = song,
    artist_name = artist
  )
}

#' Retrieve lyrics associated with a Genius song ID
#'
#' Get lyrics from Genius' lyric pages using an associated song ID.
#'
#' @family lyrics
#' @seealso See \code{\link{get_lyrics_url}} to search lyrics using a song
#' URL, \code{\link{get_lyrics_search}} to search using artist name and song
#' title
#'
#' @inheritParams get_song
#'
#' @examples
#' \dontrun{
#' get_lyrics_id(song_id = 3214267)
#' }
#' @export
get_lyrics_id <- function(song_id, access_token = genius_token()) {

  check_internet()

  # get song meta data
  song <- get_song(song_id, access_token)

  # start session
  session <- read_html(song$content$url)

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
#' @family lyrics
#' @seealso See \code{\link{get_lyrics_id}} to search lyrics using a song
#' ID, \code{\link{get_lyrics_search}} to search using artist name and song
#' title
#'
#' @param song_lyrics_url song lyrics url (like in \code{song_lyrics_url} returned by \code{\link{get_song_meta}})
#'
#' @examples
#' \dontrun{
#' get_lyrics_url(song_lyrics_url = "https://genius.com/Kendrick-lamar-dna-lyrics")
#' }
#' @export
get_lyrics_url <- function(song_lyrics_url) {

  check_internet()

  # start session
  session <- read_html(song_lyrics_url)

  # get song lyrics
  lyrics <- get_lyrics(session)

  lyrics$song_lyrics_url <- song_lyrics_url

  lyrics

}

#' Retrieve lyrics associated with a Genius song
#'
#' Attempt to get lyrics from Genius' lyric pages using an associated
#' artist name and song title.
#'
#' @family lyrics
#' @seealso See \code{\link{get_lyrics_id}} to search lyrics using a song
#' ID, \code{\link{get_lyrics_url}} to search using a song URL
#'
#' @param artist_name Name of artist
#' @param song_title Title of song
#'
#' @examples
#' \dontrun{
#' get_lyrics_search(artist_name = "Anderson .Paak",
#' song_title = "Come Home")
#' }
#' @export
get_lyrics_search <- function(artist_name, song_title) {

  # remove bad artist/song strings
  artist_name <- str_replace_all(artist_name, bad_lyric_strings)
  song_title <- str_replace_all(song_title, bad_lyric_strings)

  # construct search path
  path <- sprintf("https://genius.com/%s-%s-lyrics",
                  artist_name, song_title)

  check_internet()

  # start session
  session <- read_html(path)

  # get song lyrics
  get_lyrics(session)

}

