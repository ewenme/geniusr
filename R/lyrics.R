#' Retrieve lyrics assoicated with a Genius song ID
#'
#' Scrape lyrics from Genius' lyric pages.
#' @param song_id song ID (like in \code{song_id} returned by \code{\link{search_song}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @importFrom magrittr "%>%"
#' @examples
#' scrape_lyrics_id(song_id = 3214267)
#' @export
scrape_lyrics_id <- function(song_id, access_token=genius_token()) {

  # get song meta data
  meta <- get_song_meta(song_id)

  # start session
  session <-suppressWarnings(rvest::html(meta$song_url))

  # read lyrics
  lyrics <- rvest::html_nodes(session, ".lyrics p")

  # ensure line breaks are preserved correctly
  xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_add_sibling("p", "\n")
  xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_remove()

  # get plain text lyrics
  lyrics <- rvest::html_text(lyrics)

  # keep first element
  lyrics <- lyrics[1]

  # split on line break
  lyrics <- unlist(stringr::str_split(lyrics, pattern = "\n"))

  # remove empty strings
  lyrics <- lyrics[lyrics != ""]

  # remove lines with square brackets
  lyrics <- lyrics[!stringr::str_detect(lyrics, pattern = "\\[|\\]")]

  # Convert to tibble
  lyrics <- dplyr::tibble(line = lyrics)

  # add song metadata
  lyrics <- dplyr::mutate(lyrics,
                          song_id=meta$song_id,
                          song_name=meta$song_name,
                          artist_id=meta$artist_id)

  # Remove lines with things such as [Intro: person & so and so]
  return(dplyr::as_tibble(lyrics))

}

#' Retrieve lyrics assoicated with a Genius song ID
#'
#' Scrape lyrics from Genius' lyric pages.
#' @param lyrics_url song lyrics url (like in \code{url} returned by \code{\link{get_song_meta}})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#' @importFrom magrittr "%>%"
#' @examples
#' scrape_lyrics_url(lyrics_url = "https://genius.com/Kendrick-lamar-dna-lyrics")
#' @export
scrape_lyrics_url <- function(lyrics_url, access_token=genius_token()) {

  # start session
  session <-suppressWarnings(rvest::html(lyrics_url))

  # read lyrics
  lyrics <- rvest::html_nodes(session, ".lyrics p")

  # ensure line breaks are preserved correctly
  xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_add_sibling("p", "\n")
  xml2::xml_find_all(lyrics, ".//br") %>% xml2::xml_remove()

  # get plain text lyrics
  lyrics <- rvest::html_text(lyrics)

  # keep first element
  lyrics <- lyrics[1]

  # split on line break
  lyrics <- unlist(stringr::str_split(lyrics, pattern = "\n"))

  # remove empty strings
  lyrics <- lyrics[lyrics != ""]

  # remove lines with square brackets
  lyrics <- lyrics[!stringr::str_detect(lyrics, pattern = "\\[|\\]")]

  # Convert to tibble
  lyrics <- dplyr::tibble(line = lyrics)

  # add song metadata
  lyrics <- dplyr::mutate(lyrics,
                          song_id=meta$song_id,
                          song_name=meta$song_name,
                          artist_id=meta$artist_id)

  # Remove lines with things such as [Intro: person & so and so]
  return(dplyr::as_tibble(lyrics))

}
