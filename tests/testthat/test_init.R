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
test_that(".get.full.code",
{
	# uses test case for warnings and errors
	# multiple scripts, some proc nodes span multiple lines
	c0 <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	provDebugR::prov.debug.file(c0)
	t0 <- provDebugR:::.get.full.code()
	t0 <- data.frame(t0, stringsAsFactors = TRUE)
	
	e0 <- system.file("testexpected", "fullCode_exceptions.csv", package = "provDebugR")
	e0 <- read.csv(e0, header = TRUE, row.names = 1, stringsAsFactors = TRUE)
	
	expect_equivalent(t0, e0)
})