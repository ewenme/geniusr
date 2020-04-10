context("get_song")

test_that("get_song returns the right output formats", {

  skip_on_cran()
  output <- get_song(song_id = 3039923)
  expect_is(output, "genius_song")
})

test_that("get_song_df returns the right output formats", {

  skip_on_cran()
  output <- get_song_df(song_id = 3039923)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})
