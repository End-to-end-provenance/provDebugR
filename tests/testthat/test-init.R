context("Intialization")

library(provDebugR)

test.data <- system.file("testdata", "test.json", package = "provDebugR")
prov.complete <- debug.prov(test.data)

test_that("the debugger can be initialized", {
  expect_true(prov.complete)
  expect_true(provDebugR:::.debug.env$has.graph)
})

