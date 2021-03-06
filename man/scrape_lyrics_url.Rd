% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deprecated.R
\name{scrape_lyrics_url}
\alias{scrape_lyrics_url}
\title{Retrieve lyrics associated with a Genius lyrics page URL}
\usage{
scrape_lyrics_url(song_lyrics_url)
}
\arguments{
\item{song_lyrics_url}{song lyrics url (like in \code{song_lyrics_url} returned by \code{\link{get_song_meta}})}
}
\description{
Scrape lyrics from a Genius' lyric page using it's associated URL. Best used with \code{\link{scrape_tracklist}}, when song IDs aren't returned - otherwise, \code{\link{scrape_lyrics_id}} is recommended.
}
\examples{
\dontrun{
scrape_lyrics_url(song_lyrics_url = "https://genius.com/Kendrick-lamar-dna-lyrics")
}
}
