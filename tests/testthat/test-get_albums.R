context("get_albums")

# set album id
id <- 337082

test_that("get_album returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- get_album(album_id = id)
  expect_is(output, "genius_album")
  expect_equal(output$content$id, id)
})

test_that("album_to_df returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- album_to_df(get_album(album_id = id))
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$album_id, id)
})

test_that("get_album_df returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- get_album_df(album_id = id)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$album_id, id)

})

test_that("get_album_tracklist_id returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  output <- get_album_tracklist_id(album_id = id)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$album_id, id)
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
