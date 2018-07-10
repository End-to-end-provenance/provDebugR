context("Debugging From Line")

library(provDebugR)

test.data <- system.file("testdata", "test.json", package = "provDebugR")
debug.init(test.data)

test_that("the correct types are being returned", {
  line.results <- debug.from.line()
  expect_match(class(line.results), "data.frame")
  line.results <- debug.from.line(8, 10, state = F)
  expect_match(class(line.results), "list")
  expect_match(class(line.results[[1]]), "data.frame")
})

test_that("possible results can be grabbed", {
  line.results <- debug.from.line()
  expect_equal(ncol(line.results), 4)
  expect_equal(nrow(line.results), 10)
  expect_match(line.results[1, 1], "a")
  expect_match(line.results[10, 1], "warning.trace")
})

test_that("all possible lines can be queried", {
  pos.lines <- unique(sort(get.proc.nodes()$startLine))
  line.results <- debug.from.line(pos.lines, state = F)
  expect_equal(length(line.results), 17)
  expect_match(line.results$'12'$'val', "a match")
  expect_match(line.results$'7'$'type', "double")
})

test_that("various assignments can be provided", {
  line.results <- debug.from.line(1, state = F)
  expect_equal(length(line.results), 1)
  line.results <- debug.from.line(2:4, 10, 14, state = FALSE)
  expect_equal(length(line.results), 5)
  line.results <- debug.from.line(2:4, 10, 14, state = TRUE)
  expect_equal(length(line.results), 5)
})

test_that("wrong arguments can be ignored", {
  line.results <- debug.from.line(5, 6, state = F)
  expect_equal(length(line.results), 1)
  line.results <- debug.from.line(6)
  expect_equal(nrow(line.results), 10)
})