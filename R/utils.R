# set user agent
ua <- user_agent("http://github.com/ewenme/geniusr")

# check user internet connection
check_internet <- function() {

  if (!has_internet()) stop("Please check your internet connection")
}

# check genius classes
is_genius_song <- function(x) {

  inherits(x, "genius_song")
}

is_genius_album <- function(x) {

  inherits(x, "genius_album")
}

is_genius_artist <- function(x) {

  inherits(x, "genius_artist")
}

is_genius_resource <- function(x) {

  inherits(x, "genius_resource")
}

# simple GET wrapper
genius_get <- function(url, access_token) {
  GET(url = url, add_headers(
    Authorization = paste0("Bearer ", access_token)
    ))
}

# named vector of bad strings and replacements
bad_lyric_strings <- c(
  "\\s*\\(Ft.[^\\)]+\\)" = "",
  "&" = "and",
  "\\$" = " ",
  "'" = "",
  # "\u00e9" = "e",
  # "\u00f6" = "o",
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

# add prefix to column names
prefix_colnames <- function(x, prefix) {

  colnames(x) <- paste(prefix, colnames(x), sep = "_")

  x
}
