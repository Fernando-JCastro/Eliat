
test_that("get_products_counts works", {
  df <- data.frame(Products = c("COI", "COI, 16S", "16S"))
  res <- get_products_counts(df, Products)
  expect_equal(nrow(res), 2)
  expect_equal(res$cantidad[1], 2)
})

