context("get_albums")

test_that("get_album returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_album(album_id = 337082)
  expect_is(output, "genius_album")
})

test_that("get_album_df returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_album_df(album_id = 337082)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})

test_that("get_album_tracklist_id returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_album_tracklist_id(album_id = 337082)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})

test_that("get_album_tracklist_search returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_album_tracklist_search(artist_name = "Kendrick Lamar",
                                       album_name = "DAMN.")
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})
