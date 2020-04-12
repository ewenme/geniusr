context("get_song")

id <- 3039923

test_that("get_song returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- get_song(song_id = id)
  expect_is(output, "genius_song")
  expect_equal(output$content$id, id)
})

test_that("song_to_df returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- song_to_df(get_song(song_id = id))
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_id, id)
})

test_that("get_song_df returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- get_song_df(song_id = id)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_id, id)

  output <- get_song_meta(song_id = id)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_id, id)
})
