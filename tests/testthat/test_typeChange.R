library(testthat)

context("debug.type.changes")

# no provenance
test_that("debug.type.changes - no/empty provenance", 
{
	# initialisation not run
	expect_false(provDeubgR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.type.changes())
	
	# empty provenance
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(c0))
	expect_false(provDeubgR:::.debug.env$has.graph)
	expect_error(provDebugR::debug.type.changes())
})
