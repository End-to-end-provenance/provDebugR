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
	# cases
	q1 <- "Error in FUN(X[[i]], ...): only defined on a data frame with all numeric variables\n"
	q2 <- ""
	q3 <- "an error"
	
	c1 <- provDebugR:::.process.error(q1)
	c2 <- provDebugR:::.process.error(q2)
	c3 <- provDebugR:::.process.error(q3)
	
	# expected
	e1 <- "only defined on a data frame with all numeric variables"
	e2 <- q2
	e3 <- q3
	
	# test
	expect_equal(c1, e1)
	expect_equal(c2, e2)
	expect_equal(c3, e3)
})
