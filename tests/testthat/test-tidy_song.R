context("tidy_song_xxx")

id <- 3039923

test_that("tidy_song_relationships returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  song <- get_song(song_id = id)

  output <- tidy_song_relationships(song)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_id, id)
})

test_that("tidy_song_performances returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  song <- get_song(song_id = id)

  output <- tidy_song_performances(song)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_id, id)
})

test_that("tidy_song_producers returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  song <- get_song(song_id = id)

  output <- tidy_song_producers(song)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_id, id)
})

test_that("tidy_song_writers returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  song <- get_song(song_id = id)

  output <- tidy_song_writers(song)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$song_id, id)
})
