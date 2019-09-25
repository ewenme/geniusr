# set user agent
ua <- user_agent("http://github.com/ewenme/geniusr")

# check user internet connection
check_internet <- function() {

  if (!has_internet()) stop("Please check your internet connection")
}

# simple GET wrapper
genius_get <- function(url, access_token) {
  GET(url = url, add_headers(
    Authorization = paste0("Bearer ", access_token)
    ))
}

# set custom genius resource class
as_genius_resource <- function(content, path, response) {

  structure(
    list(
      content = content,
      path = path,
      response = response
    ),
    class = "genius_resource"
  )
}

#' Slightly more human-readable output for genius_resource objects
#'
#' @param x a discogs_database object
#' @param ... ignored
#' @export
print.genius_resource <- function(x, ...) {

  cat("<Genius ", x$path, ">\n", sep = "")
  utils::str(x$content, max=1)
  invisible(x)

}

# named vector of bad strings and replacements
bad_lyric_strings <- c(
  "\\s*\\(Ft.[^\\)]+\\)" = "",
  "&" = "and",
  "\\$" = " ",
  "'" = "",
  "\u00e9" = "e",
  "\u00f6" = "o",
  "[[:punct:]]" = "",
  "[[:blank:]]+" = " ",
  " " = "-"
  )

# repeat element of x with last TRUE element of y
repeat_before = function(x, y) {
  ind = which(y)
  if(!y[1])
    ind = c(1,ind)
  rep(x[ind], times = diff(
    c(ind, length(x) + 1) ))
}
