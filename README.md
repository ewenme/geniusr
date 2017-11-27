geniusr
================

[![Build Status](https://travis-ci.org/ewenme/geniusr.png)](https://travis-ci.org/ewenme/geniusr)

Tools for working with the Genius API.

-   Genius Developers Site: <https://genius.com/developers>
-   Genius API Docs: <https://docs.genius.com/>

Install
-------

Development version

``` r
devtools::install_github('ewenme/geniusr')
```

Authenticate
------------

1.  [Create a Genius API client](https://genius.com/api-clients/new)
2.  Generate a client access token from your [API Clients page](https://genius.com/api-clients)
3.  Set your credentials in the System Environment variable `GENIUS_API_TOKEN` by calling the `genius_token()` function and entering your Genius Client Access Token when prompted.

Use
---

``` r
library(geniusr)
library(dplyr)
library(tidytext)
```

### How many times did Kanye West say "good morning", on the track "Good Morning"?

``` r
# Get song search results for the term 'good morning'
gm_search <- search_song(search_term = "good morning") %>%
  # look for Kanye as the primary artist
  filter(artist_name == "Kanye West")

# get lyrics
gm_lyrics <- scrape_lyrics_id(song_id = gm_search$song_id)

# tokenization of the lyrics
gm_lyrics %>%
  # get bigrams
  unnest_tokens(bigram, line, token = "ngrams", n = 2) %>%
  # count bigram frequency
  count(bigram) %>%
  # look for good morning
  filter(bigram == "good morning")
```

    ## # A tibble: 1 x 2
    ##         bigram     n
    ##          <chr> <int>
    ## 1 good morning    18

### Gimme artist's with 'Lil' in their name.

``` r
# return artist matches for term 'lil'
search_artist(search_term = "Lil", n_results = 500) %>% 
  distinct(artist_name)
```

    ## # A tibble: 40 x 1
    ##                       artist_name
    ##                             <chr>
    ##  1                   Lil Uzi Vert
    ##  2                      Lil Wayne
    ##  3                       Lil Pump
    ##  4 Nicki Minaj, Drake & Lil Wayne
    ##  5                      Lil Dicky
    ##  6                       Lil Durk
    ##  7                        Lil Xan
    ##  8      Lil' Kleine & Ronnie Flex
    ##  9                     Lil Yachty
    ## 10   Lil Jon & The East Side Boyz
    ## # ... with 30 more rows

### Which artist's has Kendrick Lamar done the most features for?

``` r
# get kendricks discography INCLUDING features
kendrick_discog <- get_artist_songs(artist_id = 1421, include_features = TRUE)

kendrick_discog %>%
  # isolate features
  filter(artist_id != 1421) %>%
  # count features
  count(artist_id, artist_name, sort = T) %>%
  # get top ten
  top_n(10, wt=n)
```

    ## # A tibble: 11 x 3
    ##    artist_id            artist_name     n
    ##        <int>                  <chr> <int>
    ##  1      1403               Jay Rock    34
    ##  2     17985            Black Hippy    16
    ##  3      2049                Ab-Soul    14
    ##  4     11353            ScHoolboy Q    14
    ##  5    140856 RG France Translations    13
    ##  6     13291         Terrace Martin     8
    ##  7        42               The Game     7
    ##  8       123                Dr. Dre     7
    ##  9        72             Kanye West     5
    ## 10      1955     BJ The Chicago Kid     5
    ## 11    161407   RGPolska Tłumaczenia     5
