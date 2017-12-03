#' \code{geniusr} package
#'
#' An interface to Genius' Web API.
#'
#' See the README on
#' \href{https://github.com/ewenme/geniusr#readme}{GitHub}
#'
#' @docType package
#' @name geniusr
NULL

globalVars <- c("artist_id",
                "artist_name",
                "artist_url",
                "song_id",
                "song_name",
                "song_lyrics_url",
                "song_art_image_url",
                "album_id",
                "album_name",
                "album_url",
                ".")

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(globalVars)
