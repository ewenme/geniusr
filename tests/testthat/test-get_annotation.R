test_that("get_annotation returns the right output formats", {

  skip_on_cran()
  output <- get_annotation(annotation_id = 16511101)
  expect_is(output, "genius_resource")
})

test_that("get_referent returns the right output formats", {

  skip_on_cran()
  output <- get_referent(song_id = 3039923)
  expect_is(output, "genius_resource")
})
