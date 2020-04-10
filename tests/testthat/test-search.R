context("search")

test_that("search_genius returns the right output formats", {

  skip_on_cran()
  output <- search_genius(search_term = "Lil")
  expect_is(output, "genius_resource")
})

test_that("search_artist returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- search_artist(search_term = "Lil")
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})

test_that("search_song returns the right output formats", {

  skip_on_cran()
  skip_if_offline()
  output <- search_song(search_term = "Gucci")
  expect_is(output, "tbl_df")
  expect_is(output, "tbl")
  expect_is(output, "data.frame")
})
