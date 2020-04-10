# set custom genius annotation class
as_genius_annotation <- function(content, path, response) {

  structure(
    list(
      content = content,
      path = path,
      response = response
    ),
    class = "genius_annotation"
  )
}

#' Slightly more human-readable output for genius_annotation objects
#'
#' @param x a genius_annotation object
#' @param ... ignored
#' @export
print.genius_annotation <- function(x, ...) {

  cat("Genius annotations <", x$path, ">\n", sep = "")
  utils::str(x$content, max=1)
  invisible(x)

}

#' Retrieve metadata for an annotation
#'
#' The Genius API lets you return data for a specific annotation, given an
#' annotation ID. \code{get_annotation} returns this data in full.
#'
#' A Genius annotation is a piece of content about a part of a document.
#' The document may be a song (hosted on Genius) or a web page (hosted anywhere).
#' The part of a document that an annotation is attached to is called a referent.
#'
#' @family annotation
#'
#' @param annotation_id ID of the annotation
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a \code{genius_annotation} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @examples
#' \dontrun{
#' get_annotation(annotation_id = 16511101)
#' }
#' @export
get_annotation <- function(annotation_id, access_token = genius_token()) {

  check_internet()

  path <- sprintf("api.genius.com/annotations/%s", annotation_id)

  # request track
  req <- genius_get(url = path, access_token)

  stop_for_status(req)

  res <- content(req)

  as_genius_annotation(
    res$response, path, req
  )
}

# set custom genius referent class
as_genius_referent <- function(content, path, response) {

  structure(
    list(
      content = content,
      path = path,
      response = response
    ),
    class = "genius_referent"
  )
}

#' Slightly more human-readable output for genius_referent objects
#'
#' @param x a genius_referent object
#' @param ... ignored
#' @export
print.genius_referent <- function(x, ...) {

  cat("Genius referent <", x$path, ">\n", sep = "")
  utils::str(x$content, max=1)
  invisible(x)

}

#' Retrieve metadata for a referent
#'
#' The Genius API lets you return data for a specific referent.
#' \code{get_referent} returns this data in full.
#'
#' Referents are the sections of a piece of content to which annotations are attached.
#' Each referent is associated with a web page or a song and may have one or more annotations.
#' Referents can be searched by the document they are attached to or by the user that created them.
#'
#' @family annotation
#'
#' @param created_by_id ID of a user to get referents for
#' @param song_id ID of a song to get referents for (pass only one of \code{song_id} and
#' \code{web_page_id})
#' @param web_page_id ID of a web page to get referents for (pass only one of \code{song_id} and
#' \code{web_page_id})
#' @param access_token Genius' client access token, defaults to \code{genius_token}
#'
#' @return a \code{genius_referent} object that contains the extracted content from the request,
#' the original JSON response object and the request path.
#'
#' @examples
#' \dontrun{
#' get_referent(song_id = 3039923)
#' }
#' @export
get_referent <- function(created_by_id, song_id, web_page_id,
                         access_token = genius_token()) {

  check_internet()

  # see if too many arguments passed
  if (!missing(song_id) && !missing(web_page_id))
    stop("You may pass only one of song_id and web_page_id, not both.")

  # construct request url
  if (!missing(song_id)) {
    path <- sprintf("api.genius.com/referents?song_id=%s", song_id)

  } else if (!missing(web_page_id)) {
    path <- sprintf("api.genius.com/referents?web_page_id=%s", web_page_id)
  }

  # add created by clause if arg passed
  if (!missing(created_by_id)) {
    path <- paste0(path, "&created_by_id=", web_page_id)
  }

  results <- list()

  # initiate page counter
  i <- 1

  # while loop for retrieving results
  while (i > 0) {

    # search for term
    req <- genius_get(url = paste0(path, "&page=", i),
                      access_token)

    # stop if unexpected request status returned
    stop_for_status(req)

    # extract request content
    res <- content(req)

    results[[i]] <- res$response$referents

    # check if there are any results
    if (length(res$response$referents) > 0) {
      i <- i + 1
    } else {
      break
    }
  }

  # bind result elements
  results <- do.call(c, results)

  as_genius_referent(
    results, path, req
  )
}
