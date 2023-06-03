test_that("appTpl is not new", {
  expect_equal(app_isNew('appTpl'), FALSE)
})

test_that("app jhdms is new", {
  expect_equal(app_isNew('jhdms'), TRUE)
})


test_that("appTpl  need not created", {
  expect_equal(app_create('appTpl'), FALSE)
})


test_that("jhdms  need to created", {
  expect_equal(app_create('jhdms'), TRUE)
})


