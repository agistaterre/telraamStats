test_that("set a Telraam token", {
  expect_error(set_telraam_token())
  set_telraam_token('test')
  expect_equal(get_telraam_token(),'test')
})

test_that("call API with a wrong token", {
  set_telraam_token('test')
  expect_false(get_api_state())
})
