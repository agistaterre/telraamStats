test_that("create configuration folder if needed", {
  if(file.exists(paste(tempdir(), "config.yml", sep = "/"))){
    expect_warning(create_config(create_directory=FALSE))
  } else {
    expect_no_warning(create_config(create_directory=FALSE))
  }
  expect_true(file.exists(paste(tempdir(), "config.yml", sep = "/")))
})

test_that("check yml arguments", {
  expect_warning(create_config(create_directory=FALSE))
  configuration <- config::get(file = paste(tempdir(), "config.yml", sep = "/"))
  expect_identical(sort(names(configuration)), c('segments','url'))
})
