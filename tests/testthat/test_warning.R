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
	provDebutR::prov.debug.file(json)
	
	# by individual warnings
	c1 <- debug.warning(1)[[1]]
	c2 <- debug.warning(2)[[1]]
	
	# all = TRUE
	c3 <- debug.warning(all = TRUE)
	c4 <- debug.warning(1, all = TRUE)   # with valid query
	
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
})

# test invalid query
