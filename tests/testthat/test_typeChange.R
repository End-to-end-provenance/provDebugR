library(testthat)

context("debug.type.changes")

# no provenance
test_that("debug.type.changes - no/empty provenance", 
{
	# clean debug environment of provDebugR first to ensure inital state
	provDebugR:::.clean()
	
	# initialisation not run
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.type.changes())
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(c0))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.type.changes())
})
