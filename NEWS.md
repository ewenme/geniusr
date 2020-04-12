# geniusr 1.1.0.9000

## Breaking changes

- `get_album|artist|song_df()` is the new name of the function previously known as `get_album|artist|song_meta()`
- `get_artist_songs_df()` is the new name of the function previously known as `get_artist_songs()`. `get_artist_songs()` now returns a nested list object, with additional fields not present in the tidy sister version (`get_artist_songs_df()`)
- `get_lyrics_id|url()` is the new name of the function previously known as `scrape_lyrics_id|url()`
- `get_album_tracklist_id()` is the new name of the function previously known as `scrape_tracklist()`

## New functions

- `get_album|artist|song()` returns a nested list object, with additional fields not present in the tidy sister version (`get_album|artist|song_df()`)
- `get_album_tracklist_search()` supports the retrieval of album tracklists using `artist_name` and `album_name`.
- `get_lyrics_search()` supports the retrieval of lyrics using `artist_name` and `song_title`.
- `search_genius()` supports a generic search of hosted content on Genius
- `tidy_song_xxx()` and `tidy_album_xxx()` functions support the extraction of deeply-nested elements, of `genius_song` and `genius_album` objects respectively, into a tidy tibble format.
- `album|artist|song_to_df()` simplify `genius_album|artist|song` objects respectively, returning tidy tibbles.
- `browse_genius()` and `browse_genius_resource()` support easy navigation to Genius URLs from R.

## Minor changes

- `comment_count` field added to `get_album_df()` output
- `artist_twitter_name` added to `get_artist_df()` output
- `sort` argument added to `get_artist_songs()` and `get_artist_songs_df()`, order results by "title" (default) or by "popularity"
- remove `attempt` package dependency

# geniusr 1.1.0

- Correct license added.
- Fixed a bug occurring in `get_song_meta` functions to handle missing song fields (#6).
- Fixed a bug in `scrape_lyrics_` functions to handle instrumental songs (#8).

# geniusr 1.0.0

- CRAN release
- Added a `NEWS.md` file to track changes to the package.
