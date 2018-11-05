library(provDebugR)
library (testthat)

context("Debugging Variable Type")

test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.init(test.data)

test_that("the correct types are being returned", {
  variable.results <- debug.variable.type()
  expect_match(class(variable.results), "character")
  variable.results <- debug.lineage("x")
  expect_match(class(variable.results), "list")
  expect_match(class(variable.results[[1]]), "data.frame")
})

test_that("possible results can be grabbed", {
  variable.results <- debug.variable.type()
  expect_equal(length(variable.results), 12)
  expect_match(variable.results[1], "x")
  expect_match(variable.results[12], "test.product")
})

test_that("all variables can be queried", {
  variable.results <- debug.variable.type(debug.variable.type())
  print ("variable.results")
  print (variable.results)
  expect_equal(length(variable.results), 12)
  expect_equal(variable.results$x$line[[1]], "1")
  expect_match(variable.results$y$type[[1]], "numeric")
})

test_that("various arguments can be provided", {
  variable.results <- debug.variable.type("x")
  expect_equal(length(variable.results), 1)
  variable.results <- debug.variable.type("x", "y", "z")
  expect_equal(length(variable.results), 3)
  variable.results <- debug.variable.type("x", list("y", "z"), "df1")
  expect_equal(length(variable.results), 4)
})

test_that("wrong arguments can be ignored", {
  variable.results <- debug.variable.type("x", "y", "test")
  expect_equal(length(variable.results), 2)
  variable.results <- debug.variable.type("test")
  expect_equal(length(variable.results), 12)
})

test.data <- system.file("testdata", "specialChar.json", package = "provDebugR")
debug.init(test.data)

test_that("special values are being grabbed", {
  variable.results <- debug.variable.type("y")
  expect_match(variable.results$y$type[2], "object")
  expect_match(variable.results$y$type[3], "environment")
  #expect_match(variable.results$y$type[2], "language")  #**NOT currently supported by RDT
})


