test_that("Get Telraam Segments (as defined by default)", {
  suppressWarnings(create_config(overwrite = TRUE))
  expect_type(get_segments(),'list')
  expect_equal(names(get_segments()),c('segment-01','segment-02'))
  expect_type(get_segments()$'segment-01','character')
})
