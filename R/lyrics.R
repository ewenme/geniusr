#' Retrieve lyrics assoicated with a Genius song ID
#'
#' Get lyrics from Genius' lyric pages.
#' @param song_id song ID (like in \code{song_id} returned by \code{\link{search_song}})
#' @examples
#' get_lyrics(song_id = 3214267)
#' @export
get_lyrics <- function(song_id) {

  # get song meta data
  meta <- song_meta(song_id)

  # start session
  session <- rvest::html_session(meta$song_url)

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
  lyrics$cue_split <- stringr::str_split(lyrics$line, ":")

  # function to map track section
  lyrics$track_section <- ifelse(lyrics$cues == TRUE, sapply(lyrics$cue_split, "[", 1), NA)

  # function to map artist
  lyrics$lyric_artist_name <- ifelse(lyrics$cues == TRUE, sapply(lyrics$cue_split, "[", 2), NA)

  # remove brackets from strings
  lyrics <- dplyr::mutate_at(lyrics, c("track_section", "lyric_artist_name"), gsub,
                             pattern="\\[|\\]", replacement="")

  # remove white space
  lyrics <- dplyr::mutate_at(lyrics, c("track_section", "lyric_artist_name"), stringr::str_trim)

  # replace NAs with section/artists
  lyrics <- dplyr::mutate_at(lyrics, c("track_section", "lyric_artist_name"),
                             zoo::na.locf)

  # remove cue split col
  lyrics$cue_split <- NULL

  # return non-cue lines
  lyrics <- lyrics[lyrics$cues == FALSE, ]

  # remove cue col
  lyrics$cues <- NULL

  # Remove lines with things such as [Intro: person & so and so]
  return(lyrics)

}
