context("Debugging Variable Type")

library(provDebugR)

test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.prov(test.data)

test_that("the correct types are being returned", {
  lineage.results <- debug.variable.type()
  expect_match(class(lineage.results), "character")
  lineage.results <- debug.lineage("x")
  expect_match(class(lineage.results), "list")
  expect_match(class(lineage.results[[1]]), "data.frame")
})

test_that("possible results can be grabbed", {
  lineage.results <- debug.variable.type()
  expect_equal(length(lineage.results), 12)
  expect_match(lineage.results[1], "x")
  expect_match(lineage.results[12], "test.product")
})

test_that("all variables can be queried", {
  lineage.results <- debug.variable.type(debug.variable.type())
  expect_equal(length(lineage.results), 12)
  expect_equal(lineage.results$x$line[[1]], "1")
  expect_match(lineage.results$y$type[[1]], "numeric")
})

test_that("various arguments can be provided", {
  lineage.results <- debug.variable.type("x")
  expect_equal(length(lineage.results), 1)
  lineage.results <- debug.variable.type("x", "y", "z")
  expect_equal(length(lineage.results), 3)
  lineage.results <- debug.variable.type("x", list("y", "z"), "df1")
  expect_equal(length(lineage.results), 4)
})

test_that("wrong arguments can be ignored", {
  lineage.results <- debug.variable.type("x", "y", "test")
  expect_equal(length(lineage.results), 2)
  lineage.results <- debug.variable.type("test")
  expect_equal(length(lineage.results), 12)
})



