context("get_artist")

test_that("get_artist returns the right output formats", {

  skip_on_cran()
  output <- get_artist(artist_id = 16775)
  expect_is(output, "genius_resource")
})

test_that("get_artist_df returns the right output formats", {

  skip_on_cran()
  output <- get_artist_df(artist_id = 16775)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})

test_that("get_artist_songs returns the right output formats", {

  skip_on_cran()
  output <- get_artist_songs(artist_id = 1421)
  expect_is(output, "genius_resource")
})

test_that("get_artist_songs_df returns the right output formats", {

  skip_on_cran()
  output <- get_artist_songs_df(artist_id = 1421)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})
