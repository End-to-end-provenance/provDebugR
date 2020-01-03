library(testthat)

context("debug.warning")

# no provenance
test_that("debug.warning - no/empty provenance", 
{
	# initialisation not run
	expect_false(provDeubgR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.warning())
	
	# empty provenance
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(json))
	expect_false(provDeubgR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.warning())
})

# test: no warnings

# general case:
test_that("debug.warning - general",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	provDebugR::prov.debug.file(json)
	
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
	provDebugR::prov.debug.file(json)
	
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
	provDebugR::prov.debug.file(json)
	
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
	
	# tests
	expect_null(c1)
	expect_equal(typeof(c2), "character")
	expect_true(length(c2) > 4)
	
	expect_null(c3)
	expect_equal(typeof(c4), "character")
	expect_true(length(c4) > 4)
	
	expect_null(c5)
	expect_equal(typeof(c6), "character")
	expect_true(length(c6) > 6)
	
	expect_equivalent(c7[[1]], e1)
	expect_equal(typeof(c8), "character")
	expect_true(length(c8) > 5)
})

# .get.valid.warn
test_that("debug.warning - .get.valid.warn",
{
	# possible warning nodes
	pos.nodes <- data.frame(name=c("warning 1", "warning 2", "warning 3"), 
							stringsAsFactors = FALSE)
	
	# queries
	q1 <- "invalid"             # invalid
	q2 <- 50                    # invalid
	q3 <- c(1:3)                # all valid
	q4 <- c("3", "invalid", 2)  # some invalid, some valid
	q5 <- c(6:8)                # all invalid
	
	# test cases
	c2 <- utils::capture.output(c1 <- provDebugR:::.get.valid.warn())       # no queries
	
	c4 <- utils::capture.output(c3 <- provDebugR:::.get.valid.warn(q1))     # invalid
	c6 <- utils::capture.output(c5 <- provDebugR:::.get.valid.warn(q2))     # invalid
	c8 <- utils::capture.output(c7 <- provDebugR:::.get.valid.warn(q3))     # all valid
	c10 <- utils::capture.output(c9 <- provDebugR:::.get.valid.warn(q4))    # some invalid, some valid
	c12 <- utils::capture.output(c11 <- provDebugR:::.get.valid.warn(q5))   # all invalid
	
	c14 <- utils::capture.output(c13 <- provDebugR:::.get.valid.warn(all = TRUE))      # all
	c16 <- utils::capture.output(c15 <- provDebugR:::.get.valid.warn(3, all = TRUE))   # all, valid query
	c18 <- utils::capture.output(c17 <- provDebugR:::.get.valid.warn(9, all = TRUE))   # all, invalid query
	
	# test
	expect_null(c1)                                 # no queries
	expect_equal(typeof(c2), "character")
	expect_true(length(c2) > nrow(pos.nodes))
	
	expect_null(c3)                                 # invalid
	expect_equal(typeof(c4), "character")
	expect_true(length(c4) > nrow(pos.nodes) + 1)
	
	expect_null(c5)                                 # invalid
	expect_equal(typeof(c6), "character")
	expect_true(length(c6) > nrow(pos.nodes) + 1)
	
	expect_equivalent(c7, pos.nodes)                # all valid
	expect_equal(length(c8), 0)
	
	expect_equivalent(c9, pos.nodes[c(3,2), ])      # some valid, some invalid
	expect_true(length(c10) > 1)
	
	expect_null(c11)                                # all invalid
	expect_equal(typeof(c12), "character")
	expect_true(length(c12) > nrow(pos.nodes) + 3)
	
	expect_equivalent(c13, pos.nodes)               # all
	expect_equal(length(c14), 0)
	
	expect_equivalent(c15, pos.nodes)               # all, valid query
	expect_equal(length(c16), 0)
	
	expect_equivalent(c17, pos.nodes)               # all, invalid query
	expect_equal(length(c18), 0)
})
