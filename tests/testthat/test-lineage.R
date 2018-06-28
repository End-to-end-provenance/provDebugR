context("Debugging Lineage")

library(provDebugR)

test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.init(test.data)

test_that("the correct types are being returned", {
  lineage.results <- debug.lineage()
  expect_match(class(lineage.results), "character")
  lineage.results <- debug.lineage("x")
  expect_match(class(lineage.results), "list")
  expect_match(class(lineage.results[[1]]), "data.frame")
})

test_that("possible results can be grabbed", {
  lineage.results <- debug.lineage()
  expect_equal(length(lineage.results), 12)
  expect_match(lineage.results[1], "x")
  expect_match(lineage.results[12], "test.product")
})

test_that("all variables can be queried", {
  lineage.results <- debug.lineage(debug.lineage())
  expect_equal(length(lineage.results), 12)
  expect_equal(lineage.results$x$line[[1]], 3)
  expect_match(lineage.results$z$code[[1]], "z <- 6:67")
})

test_that("various arguments can be provided", {
  lineage.results <- debug.lineage("x")
  expect_equal(length(lineage.results), 1)
  lineage.results <- debug.lineage("x", "y", "z")
  expect_equal(length(lineage.results), 3)
  lineage.results <- debug.lineage("x", list("y", "z"), "df1")
  expect_equal(length(lineage.results), 4)
})

test_that("wrong arguments can be ignored", {
  lineage.results <- debug.lineage("x", "y", "test")
  expect_equal(length(lineage.results), 2)
  lineage.results <- debug.lineage("test")
  expect_equal(length(lineage.results), 12)
})



