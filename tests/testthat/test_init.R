library(testthat)

context("Initialization")

# prov.debug.file (general case)
test_that("Initialization - prov.debug.file",
{
	#json <- system.file("testdata", "<json.fle.here>", package = "provDebugR")
	#provDebugR::prov.debug.file(json)
	
	#expect_true(provDebugR:::.debug.env$has.graph)
	
	# TODO - expect test on rest of .debug.env variables
})

# prov.debug.run (general case)
test_that("Initialization - prov.debug.run",
{
	#file <- system.file("testscripts", "<r.fle.here>", package = "provDebugR")
	
	# TODO
	# check provenance generated matched one in expected folder
	# expected json generated from create-json
	
	#expect_true(provDebugR:::.debug.env$has.graph)
	
	# TODO - make sure to delete prov folder that running the script generated
})

# prov.debug (general case)
test_that("Initialization - prov.debug",
{
	#file <- system.file("testscripts", "<r.fle.here>", package = "provDebugR")
	#rdtLite::prov.run(file)
	
	# TODO
	# check provenance genreated matched one in expected folder
	# expected json generated from create-json
	
	#provDebugR::prov.debug()
	#expect_true(provDebugR:::.debug.env$has.graph)
	
	# TODO - make sure to delete prov folder that running the script generated
})

# empty prov
test_that("Initialization - Empty provenance", 
{
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(provDebugR::prov.debug.file(json))
	expect_false(provDeubgR:::.debug.env$has.graph)
})

# .get.full.code
test_that("Initialization - .get.full.code",
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