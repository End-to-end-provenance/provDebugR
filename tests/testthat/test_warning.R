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
