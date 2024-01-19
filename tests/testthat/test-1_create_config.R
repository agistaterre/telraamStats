test_that("create configuration folder if needed", {
  create_config()
  expect_true(dir.exists("inst/"))
  expect_true(file.exists("inst/config.yml"))
})

test_that("check yml arguments", {
  create_config()
  configuration <- config::get(file = "inst/config.yml")
  expect_identical(sort(names(configuration)), c('segments','url'))
})
