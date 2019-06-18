context("Intialization")

library(provDebugR)

test_that("Debugger initialization on empty provenance", {
  test.data <- system.file("testdata/cases", "empty.json", package = "provDebugR")
  expect_error(debug.init(test.data))
  expect_false(provDebugR:::.debug.env$has.graph)
})

test_that("the debugger can be initialized", {
  test.data <- system.file("testdata", "test.json", package = "provDebugR")
  debug.init(test.data)
  expect_true(provDebugR:::.debug.env$has.graph)
})
