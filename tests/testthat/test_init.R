library(testthat)

context("Initialization")

# prov.debug.file (general case)

# prov.debug.run (general case)

# prov.debug (general case)

# empty prov
test_that("Initialization - Empty provenance", 
{
	c0 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(c0))
	expect_false(provDeubgR:::.debug.env$has.graph)
})

# .get.full.code
