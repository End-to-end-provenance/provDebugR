library(provDebugR)
library (testthat)

context("Debugging From Line")

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
  expect_equal(ncol(line.results), 6)
  expect_equal(nrow(line.results), 9)
  expect_match(line.results[1, 1], "a")
  expect_match(line.results[9, 1], "y")
})

test_that("all possible lines can be queried", {
  pos.lines <- unique(sort(provParseR::get.proc.nodes(.debug.env$prov)$startLine))
  line.results <- debug.from.line(pos.lines, state = F)
  expect_equal(length(line.results), 18)
  expect_match(line.results$'12'$'val', "a test")
  expect_match(line.results$'7'$'type'[1], "{ \"type\" : [ \"numeric\" ]}", fixed = T)
})

test_that("various assignments can be provided", {
  line.results <- debug.from.line(1, state = F)
  expect_equal(length(line.results), 1)
  line.results <- debug.from.line(2:4, 10, 14, state = FALSE)
  expect_equal(length(line.results), 5)
  line.results <- debug.from.line(2:4, 10, 14, state = TRUE)
})

test_that("wrong arguments can be ignored", {
  line.results <- debug.from.line(5, 6, state = F)
  expect_equal(length(line.results), 1)
  line.results <- debug.from.line(6)
  expect_equal(nrow(line.results), 9)
})

test.data <- system.file("testdata", "testErrors2.json", package = "provDebugR")
debug.init(test.data)

test_that("error output works", {
  line.results <- debug.from.line(4, state = F)
  expect_equal(length(line.results), 1)
  expect_equal(nrow(line.results[[1]]), 4)
  line.results <- debug.from.line(4, state = T)
  expect_equal(length(line.results), 1)
  expect_equal(nrow(line.results[[1]]), 5)
  line.results <- debug.from.line(6, state = F)
  expect_equal(length(line.results), 1)
  expect_equal(nrow(line.results[[1]]), 4)
  line.results <- debug.from.line(6, state = T)
  expect_equal(length(line.results), 1)
  expect_equal(nrow(line.results[[1]]), 6)
})

# case when called from debug.browser
# and when the first line has no data nodes associated
test.data <- system.file("testdata", "stepin3.json", package = "provDebugR")
debug.init(test.data)

test_that("when called from debug.browser", {
  line.results <- debug.from.line(1, state = T)[[1]]
  
  # test dimensions and type
  expect_true(is.matrix(line.results))
  expect_equal(dim(line.results), c(1,6))
  
  # test content
  expect_equivalent(as.vector(line.results), rep(NA, 6))
})