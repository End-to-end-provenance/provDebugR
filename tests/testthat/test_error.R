library(testthat)

context("debug.error")

# no provenance
test_that("debug.error - no/empty provenance", 
{
	# initialisation not run
	expect_false(provDeubgR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.error())
	
	# empty provenance
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(json))
	expect_false(provDeubgR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.error())
})

# no error

# general case: error with lineage of >1 nodes
test_that("debug.error - general",
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	provDebugR::prov.debug.file(json)
	
	# c1: output table on lineage of error
	# c2: message about error
	c2 <- utils::capture.output(c1 <- debug.error())
	
	# e1: expected output table on lineage of error
	e1 <- system.file("testexpected", "debugError_general.csv", package = "provDebugR")
	e1 <- read.csv(e0, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(c1, e1)	# check equivalence of table contents
	expect_true(nchar(c2) > 0)	# check that there exists message about error
})
