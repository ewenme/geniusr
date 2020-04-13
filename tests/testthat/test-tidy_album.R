context("tidy_album_xxx")

id <- 337082

test_that("tidy_album_performances returns the right output formats", {

  skip_on_cran()
  skip_if_offline()

  album <- get_album(album_id = id)
  output <- tidy_album_performances(album)
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
  expect_setequal(output$album_id, id)
})
