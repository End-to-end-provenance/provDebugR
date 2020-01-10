library(testthat)
library(provDebugR)

context("debug.warning")

# no provenance
test_that("debug.warning - no/empty provenance", 
{
	# clean debug environment of provDebugR first to ensure inital state
	provDebugR:::.clear()
	
	# initialisation not run
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.warning())
	
	# empty provenance
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(json))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.warning())
})

# test: no warnings

# general case:
test_that("debug.warning - general",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	provDebugR:::.clear()
	prov.debug.file(json)
	
	# by individual warnings
	c1 <- debug.warning(1)[[1]]
	c2 <- debug.warning("2")[[1]]
	
	# all = TRUE
	c3 <- debug.warning(all = TRUE)   # with no queries
	c4 <- debug.warning(1, all = TRUE)   # with valid query
	c5 <- debug.warning(5, all = TRUE)   # with invalid query
	
	# expected
	e1 <- system.file("testexpected", "warnings1.csv", package = "provDebugR")
	e2 <- system.file("testexpected", "warnings2.csv", package = "provDebugR")
	
	e1 <- read.csv(e1, row.names = 1, stringsAsFactors = FALSE)
	e2 <- read.csv(e2, row.names = 1, stringsAsFactors = FALSE)
	
	# test: table contents
	expect_equivalent(c1, e1)
	expect_equivalent(c2, e2)
	
	# test: all = TRUE
	expect_equal(length(c3), 2)
	expect_equivalent(c3[[1]], e1)
	expect_equivalent(c3[[2]], e2)
	
	expect_equal(length(c4), 2)
	expect_equivalent(c4[[1]], e1)
	expect_equivalent(c4[[2]], e2)
	
	expect_equal(length(c5), 2)
	expect_equivalent(c5[[1]], e1)
	expect_equivalent(c5[[2]], e2)
})

# no queries
test_that("debug.warning - no queries", 
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	provDebugR:::.clear()
	prov.debug.file(json)
	
	c2 <- utils::capture.output(c1 <- debug.warning())
	
	# returned value
	expect_null(c1)
	
	# output for user
	expect_equal(typeof(c2), "character")
	expect_true(length(c2) > 2)
})

# invalid queries
test_that("debug.warning - invalid queries",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	provDebugR:::.clear()
	prov.debug.file(json)
	
	# an expected output table
	e1 <- system.file("testexpected", "warnings1.csv", package = "provDebugR")
	e1 <- read.csv(e1, row.names = 1, stringsAsFactors = FALSE)
	
	# queries
	q1 <- "invalid"
	q2 <- 50
	q3 <- c(3:5)
	q4 <- c(3,1,5)   # with a valid query
	
	# test cases
	c2 <- utils::capture.output(c1 <- debug.warning(q1))
	c4 <- utils::capture.output(c3 <- debug.warning(q2))
	c6 <- utils::capture.output(c5 <- debug.warning(q3))
	c8 <- utils::capture.output(c7 <- debug.warning(q4))
	
	# tests: returned value and user output
	expect_null(c1)
	c2 <- paste(c2, collapse="\n")
	expect_true(nchar(c2) > 0)
	
	expect_null(c3)
	c4 <- paste(c4, collapse="\n")
	expect_true(nchar(c4) > 0)
	
	expect_null(c5)
	c6 <- paste(c6, collapse="\n")
	expect_true(nchar(c6) > 0)
	
	expect_equivalent(c7[[1]], e1)
	c8 <- paste(c8, collapse="\n")
	expect_true(nchar(c8) > 0)
})

# .get.valid.warn
test_that("debug.warning - .get.valid.warn",
{
	# possible warning nodes
	pos.nodes <- data.frame(id=c("d1", "d2", "d3"),
							value=c("warning 1", "warning 2", "warning 3"), 
							stringsAsFactors = FALSE)
	
	# queries
	q1 <- "invalid"             # invalid
	q2 <- 50                    # invalid
	q3 <- c(1:3)                # all valid
	q4 <- c("3", "invalid", 2)  # some invalid, some valid
	q5 <- c(6:8)                # all invalid
	
	# test cases
	c2 <- utils::capture.output(
		c1 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes))       # no queries
	c4 <- utils::capture.output(
		c3 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes, q1))   # invalid
	c6 <- utils::capture.output(
		c5 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes, q2))   # invalid
	c8 <- utils::capture.output(
		c7 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes, q3))   # all valid
	c10 <- utils::capture.output(
		c9 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes, q4))   # some invalid, some valid
	c12 <- utils::capture.output(
		c11 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes, q5))  # all invalid
	
	c14 <- utils::capture.output(
		c13 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes, all = TRUE))      # all
	c16 <- utils::capture.output(
		c15 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes, 3, all = TRUE))   # all, valid query
	c18 <- utils::capture.output(
		c17 <- provDebugR:::.get.valid.warn(warning.nodes = pos.nodes, 9, all = TRUE))   # all, invalid query
	
	# test
	expect_null(c1)                                 # no queries
	c2 <- paste(c2, collapse="\n")
	expect_true(nchar(c2) > 0)
	
	expect_null(c3)                                 # invalid
	c4 <- paste(c4, collapse="\n")
	expect_true(nchar(c4) > 0)
	
	expect_null(c5)                                 # invalid
	c6 <- paste(c6, collapse="\n")
	expect_true(nchar(c6) > 0)
	
	expect_equivalent(c7, pos.nodes)                # all valid
	expect_equal(length(c8), 0)
	
	expect_equivalent(c9, pos.nodes[c(3,2), ])      # some valid, some invalid
	c10 <- paste(c10, collapse="\n")
	expect_true(nchar(c10) > 0)
	
	expect_null(c11)                                # all invalid
	c12 <- paste(c12, collapse="\n")
	expect_true(nchar(c12) > 0)
	
	expect_equivalent(c13, pos.nodes)               # all
	expect_equal(length(c14), 0)
	
	expect_equivalent(c15, pos.nodes)               # all, valid query
	expect_equal(length(c16), 0)
	
	expect_equivalent(c17, pos.nodes)               # all, invalid query
	expect_equal(length(c18), 0)
})
