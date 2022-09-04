context("get_artist")

id <- 16775

test_that("get_artist returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_artist(artist_id = id)
  expect_is(output, "genius_artist")
  expect_equal(output$content$id, id)
})

test_that("artist_to_df returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- artist_to_df(get_artist(artist_id = id))
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$artist_id, id)
})

test_that("get_artist_df returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- get_artist_df(artist_id = id)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$artist_id, id)

})

test_that("get_artist_songs returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_artist_songs(artist_id = id)
  expect_is(output, "genius_resource")
})

test_that("get_artist_songs_df returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_artist_songs_df(artist_id = id)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$artist_id, id)
})
