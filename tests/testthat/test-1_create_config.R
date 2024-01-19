test_that("create configuration folder if needed", {
  if(dir.exists("inst/")){
    expect_warning(create_config())
  } else {
    expect_no_warning(create_config())
  }
  expect_true(dir.exists("inst/"))
  expect_true(file.exists("inst/config.yml"))
})

test_that("check yml arguments", {
  expect_warning(create_config())
  configuration <- config::get(file = "inst/config.yml")
  expect_identical(sort(names(configuration)), c('segments','url'))
})
