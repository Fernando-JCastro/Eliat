
test_that("get_metadata no se rompe con una secuencia vacía", {
  expect_error(get_metadata(character(0)), "Input vector is empty")
})

