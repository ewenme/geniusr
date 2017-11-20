#' Retrieve lyrics from a Genius lyrics URL
#'
#' Get lyrics from Genius' lyric pages.
#' @param lyrics_url lyric page URL (like in \code{lyrics_url} returned by \code{\link{search_track}})
#' @examples
#' get_lyrics(lyrics_url = "/Waka-flocka-flame-rooster-in-my-rari-lyrics")
#' @export
get_lyrics <- function(lyrics_url) {

  # start session
  session <- rvest::html_session(paste0("https://genius.com", lyrics_url))

  # read lyrics
  lyrics <- gsub(pattern = "<.*?>",
                 replacement = "\n",
                 rvest::html_node(session, ".lyrics")) %>%
    readr::read_lines() %>%
    na.omit()

  # Convert to tibble
  lyrics <- dplyr::tibble(line = lyrics)

  # Isolate only lines that contain content
  index <- which(stringr::str_detect(lyrics$line, "[[:alnum:]]") == TRUE)
  lyrics <- lyrics[index,]

  # isolate lines with artist cues
  lyrics$cues <- stringr::str_detect(lyrics$line, "\\[|\\]")

  # split string at colon
  lyrics$cue_split <- stringr::str_split(lyrics$line[lyrics$cues == TRUE], ":")

  #

  # Remove lines with things such as [Intro: person & so and so]
  return(lyrics[stringr::str_detect(lyrics$line, "\\[|\\]") == FALSE, ])

}
