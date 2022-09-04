context("get_lyrics")

id <- 3214267
url <- "https://genius.com/Kendrick-lamar-dna-lyrics"

test_that("get_lyrics_id returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- get_lyrics_id(song_id = id)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_id, id)

})

test_that("get_lyrics_url returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- get_lyrics_url(song_lyrics_url = "https://genius.com/Kendrick-lamar-dna-lyrics")
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_lyrics_url, url)

})

test_that("get_lyrics_search returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_lyrics_search(artist_name = "Anderson .Paak",
                              song_title = "Come Home")
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})
