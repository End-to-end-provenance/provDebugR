context("Debugging Warning and Error Tracing")

library(provDebugR)

test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.prov(test.data)

test_that("scripts with no errors/warnings will throw an error", {
  warning.results <- capture.output(debug.warning.trace())
  expect_match(warning.results, "There were no warnings in this script!")
  error.results <- capture.output(debug.error.trace())
  expect_match(error.results, "There were no errors in this script!")
})

test.data <- system.file("testdata", "testErrors.json", package = "provDebugR")
debug.prov(test.data)

test_that("possible results can be grabbed", {
  warning.results <- capture.output(debug.warning.trace())
  expect_equal(length(warning.results), 4)
  expect_match(warning.results[[2]] ,
               "[1] \"In  eval(annot, environ, NULL) :  this is a test\"     ", fixed = T)
})

test_that("all variables can be queried", {
  warning.results <- debug.warning.trace(1:2)
  expect_equal(length(warning.results), 2)
  expect_match(warning.results[[1]]$code, "warning(\"this is a test\")", fixed = T)
})

test_that("various arguments can be provided", {
  warning.results <- debug.warning.trace(1)
  expect_equal(length(warning.results), 1)
  warning.results <- debug.warning.trace(list(1, 2))
  expect_equal(length(warning.results), 2)
})

test_that("wrong arguments can be ignored", {
  warning.results <- debug.warning.trace(1, 4)
  expect_equal(length(warning.results), 1)
  warning.results <- debug.warning.trace(4)
  expect_equal(length(warning.results), 0)
})

test_that("debug error does something", {
  error.results <- debug.error.trace()
  expect_match(class(error.results), "data.frame")
  expect_equal(error.results$line[[2]], 24)
  expect_match(error.results$code[[2]],"x <- b + y", fixed = T)
})
