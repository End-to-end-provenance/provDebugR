library(testthat)
library(provDebugR)

context("Initialization")

# prov.debug.file (general case)
test_that("Initialization - prov.debug.file",
{
	#json <- system.file("testdata", "<json.fle.here>", package = "provDebugR")
	#prov.debug.file(json)
	
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
	
	#prov.debug()
	#expect_true(provDebugR:::.debug.env$has.graph)
	
	# TODO - make sure to delete prov folder that running the script generated
})

# empty prov
test_that("Initialization - Empty provenance", 
{
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(json))
	expect_false(provDebugR:::.debug.env$has.graph)
})

# .get.full.code
# .get.full.code throws a warning when files are not found.
# this makes the initialisation functions throw the warning too.
test_that("Initialization - .get.full.code",
{
	# GENERAL CASE
	# uses test case for warnings and errors
	# multiple scripts, some proc nodes span multiple lines
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	
	prov.debug.file(json)
	c1 <- provDebugR:::.get.full.code()
	c1 <- data.frame(c1, stringsAsFactors = FALSE)
	
	e1 <- system.file("testexpected", "fullCode_exceptions.csv", package = "provDebugR")
	e1 <- read.csv(e1, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(c1, e1)
	
	# EDGE CASE 1
	# case where no script files could be found
	# this uses the test case for type changes
	provDebugR:::.clear()
	json <- system.file("testdata", "typeChanges.json", package = "provDebugR")
	
	expect_warning(prov.debug.file(json))
	expect_warning(c2 <- provDebugR:::.get.full.code())
	c2 <- data.frame(c2, stringsAsFactors = FALSE)
	
	e2 <- system.file("testexpected", "fullCode_typeChanges.csv", package = "provDebugR")
	e2 <- read.csv(e2, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(c2, e2)
	
	# EDGE CASE 2
	# case where only some of the script files could be found
	provDebugR:::.clear()
	json <- system.file("testdata", "missingScripts.json", package = "provDebugR")
	
	expect_warning(prov.debug.file(json))
	expect_warning(c3 <- provDebugR:::.get.full.code())
	c3 <- data.frame(c3, stringsAsFactors = FALSE)
	
	e3 <- system.file("testexpected", "fullCode_missingScripts.csv", package = "provDebugR")
	e3 <- read.csv(e3, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
	expect_equivalent(c3, e3)
})
