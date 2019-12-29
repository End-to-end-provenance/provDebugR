library(testthat)

context("Initialization")

# prov.debug.file (general case)

# prov.debug.run (general case)

# prov.debug (general case)

# empty prov
test_that("Debugger initialization on empty provenance", 
{
	c1 <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(c1))
	expect_false(provDeubgR:::.debug.env$has.graph)
})

# .get.full.code
