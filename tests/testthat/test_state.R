library(testthat)

context("debug.state")

# no provenance
test_that("debug.state - no/empty provenance", 
{
	# initialisation not run
	expect_false(provDeubgR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.state(10))
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(c0))
	expect_false(provDeubgR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.state(10))
})
