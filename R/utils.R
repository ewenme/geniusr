# set user agent
ua <- user_agent("http://github.com/ewenme/geniusr")

# check user internet connection
check_internet <- function(){
  stop_if_not(.x = has_internet(), msg = "Please check your internet connection")
}

# simple GET wrapper
genius_get <- function(url, access_token) {
  GET(url = url, add_headers(
    Authorization = paste0("Bearer ", access_token)
    ))
}
