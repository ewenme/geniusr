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

globalVars <- c("artist_name",
                "album_name",
                "artist_id",
                "artist_url",
                ".")

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(globalVars)
