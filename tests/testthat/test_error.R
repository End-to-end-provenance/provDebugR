library(testthat)
library(provDebugR)

context("debug.error")

# no provenance
test_that("debug.error - no/empty provenance", 
{
	# clear debug environment of provDebugR first to ensure inital state
	provDebugR:::.clear()
	
	# initialisation not run
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.error())
	
	# empty provenance
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(json))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(debug.error())
})

# no data nodes
test_that("debug.error - no data nodes",
{
	skip("debug.error - no data nodes")
	
	json <- system.file("testdata", "noDataNodes.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to prov folder deleted
	
	c2 <- utils::capture.output(c1 <- debug.error())
	c2 <- paste(c2, collapse = '\n')
	
	expect_null(c1)              # check the returned value
	expect_true(nchar(c2) > 0)   # check that a message exists
})

# no error
test_that("debug.error - no error",
{
	json <- system.file("testdata", "typeChanges.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to prov folder deleted
	
	c2 <- utils::capture.output(c1 <- debug.error())
	c2 <- paste(c2, collapse = '\n')
	
	expect_null(c1)              # check the returned value
	expect_true(nchar(c2) > 0)   # check that a message exists
})

# general case: error with lineage of > 1 nodes
test_that("debug.error - general",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	provDebugR:::.clear()
	expect_warning(prov.debug.file(json))   # warning due to deleted prov folder
	
	# c1: output table on lineage of error
	# c2: message about error
	c2 <- utils::capture.output(c1 <- debug.error())
	c2 <- paste(c2, collapse = '\n')
	
	# e1: expected output table on lineage of error
	e1 <- system.file("testexpected", "debugError_general.csv", package = "provDebugR")
	e1 <- read.csv(e1, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(c1, e1)	# check equivalence of table contents
	expect_true(nchar(c2) > 0)	# check that there exists message about error
})

# .process.error
test_that("debug.error - .process.error",
{
	# queries
	q1 <- "Error in FUN(X[[i]], ...): only defined on a data frame with all numeric variables\n"
	q2 <- ""
	q3 <- "an error"
	q4 <- "\nError in FUN(newX[, i], ...): invalid 'type' (character) of argument\n"
	q5 <- "\t\tError: object 'a' not found\n\t"
	
	# QUOTES REMOVED
	# cases
	c1 <- provDebugR:::.process.error(q1)
	c2 <- provDebugR:::.process.error(q2)
	c3 <- provDebugR:::.process.error(q3)
	c4 <- provDebugR:::.process.error(q4)
	c5 <- provDebugR:::.process.error(q5)
	
	# expected
	e1 <- "only defined on a data frame with all numeric variables"
	e2 <- q2
	e3 <- q3
	e4 <- "invalid  (character) of argument"
	e5 <- "object  not found"
	
	# test
	expect_equal(c1, e1)
	expect_equal(c2, e2)
	expect_equal(c3, e3)
	expect_equal(c4, e4)
	expect_equal(c5, e5)
	
	# QUOTES RETAINED
	# cases
	c6 <- provDebugR:::.process.error(q1, remove.quotes = FALSE)
	c7 <- provDebugR:::.process.error(q2, remove.quotes = FALSE)
	c8 <- provDebugR:::.process.error(q3, remove.quotes = FALSE)
	c9 <- provDebugR:::.process.error(q4, remove.quotes = FALSE)
	c10 <- provDebugR:::.process.error(q5, remove.quotes = FALSE)
	
	# expected
	e6 <- e1
	e7 <- e2
	e8 <- e3
	e9 <- "invalid 'type' (character) of argument"
	e10 <- "object 'a' not found"
	
	# test
	expect_equal(c6, e6)
	expect_equal(c7, e7)
	expect_equal(c8, e8)
	expect_equal(c9, e9)
	expect_equal(c10, e10)
})
