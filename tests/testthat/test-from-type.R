context("Debugging From Type")

library(provDebugR)

test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.init(test.data)

test_that("the correct types are being returned", {
  type.results <- debug.from.type("x", "numeric")
  expect_match(class(type.results), "data.frame")
})

test_that("wrong arguments can be ignored", {
  type.results <- debug.from.type("y", "numeric")
  expect_match(class(type.results), "character")
})
