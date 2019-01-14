ua <- httr::user_agent("http://github.com/ewenme/geniusr")

#' @importFrom attempt stop_if_not
#' @importFrom curl has_internet
check_internet <- function(){
  attempt::stop_if_not(.x = curl::has_internet(), msg = "Please check your internet connection")
}

replace_space <- function(x) {

  no_spaces <- gsub(" ", "%20", x)

  return(no_spaces)
}
