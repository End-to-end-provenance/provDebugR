context("Intialization")

library(provDebugR)

test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.init(test.data)

test_that("the debugger can be initialized", {
  expect_true(provDebugR:::.debug.env$has.graph)
})

