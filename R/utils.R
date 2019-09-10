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

# named vector of bad strings and replacements
bad_lyric_strings <- c(
  "\\s*\\(Ft.[^\\)]+\\)" = "",
  "&" = "and",
  "\\$" = " ",
  "'" = "",
  "é" = "e",
  "ö" = "o",
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
