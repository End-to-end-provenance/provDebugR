library(testthat)

context("Initialization")

# prov.debug.file (general case)

# prov.debug.run (general case)

# prov.debug (general case)

# empty prov
test_that("Initialization - Empty provenance", 
{
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(json))
	expect_false(provDeubgR:::.debug.env$has.graph)
})

# .get.full.code
test_that(".get.full.code",
{
	# uses test case for warnings and errors
	# multiple scripts, some proc nodes span multiple lines
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	provDebugR::prov.debug.file(json)
	c0 <- provDebugR:::.get.full.code()
	c0 <- data.frame(t0, stringsAsFactors = FALSE)
	
	e0 <- system.file("testexpected", "fullCode_exceptions.csv", package = "provDebugR")
	e0 <- read.csv(e0, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(t0, e0)
})