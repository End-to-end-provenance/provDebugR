library(testthat)
library(provDebugR)

context("Initialization")

# empty prov
test_that("Initialization - Empty provenance", 
{
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(prov.debug.file(json))
	expect_false(provDebugR:::.debug.env$has.graph)
})

# # .get.full.code
# # tests the helper function of .get.full.code
# # this is to prevent travis from having to run rdt/rdtLite
# test_that("Initialization - .get.full.code",
# {
# 	# FUNCTION INPUTS
# 	# get proc.nodes
# 	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
# 	prov <- provParseR::prov.parse(json, isFile = TRUE)
# 	proc.nodes <- provParseR::get.proc.nodes(prov)
	
# 	# saved scripts
# 	script1 <- system.file("testscripts", "exceptions.R", package = "provDebugR")
# 	script2 <- system.file("testscripts", "source_warning.R", package = "provDebugR")
# 	script3 <- system.file("testscripts", "source_error.R", package = "provDebugR")
	
# 	s1 <- c(script1, script2, script3)            # all scripts are found
# 	s2 <- c("unknown1", "unknown2", "unknown3")   # no scripts found
# 	s3 <- c(script1, script2, "unknown3")         # some scripts found, some not found
	
# 	# CASES
# 	c1 <- provDebugR:::.get.full.code.helper(proc.nodes, s1)        # all scripts are found
# 	expect_warning(                                                 # no scripts found
# 		c2 <- provDebugR:::.get.full.code.helper(proc.nodes, s2))
# 	expect_warning(                                                 # some scripts found, some not found
# 		c3 <- provDebugR:::.get.full.code.helper(proc.nodes, s3))
	
# 	# EXPECTED
# 	e1 <- system.file("testexpected", "fullCode.csv", package = "provDebugR")
# 	e3 <- system.file("testexpected", "fullCode_missingScripts.csv", package = "provDebugR")
	
# 	e1 <- read.csv(e1, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
# 	e3 <- read.csv(e3, header = TRUE, row.names = 1, stringsAsFactors = FALSE)
	
# 	e1 <- e1[ , 1]          # all scripts are found
# 	e2 <- proc.nodes$name   # no scripts found
# 	e3 <- e3[ , 1]          # some scripts found, some not found
	
# 	# TEST
# 	expect_equal(c1, e1)
# 	expect_equal(c2, e2)
# 	expect_equal(c3, e3)
# })

# .get.scripts
test_that("Initialization - .get.scripts",
{
	# multiple scripts
	provDebugR:::.clear()
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	expect_warning(provDebugR::prov.debug.file(json))
	
	c1 <- provDebugR:::.get.scripts()
	e1 <- c("exceptions.R", "source_warning.r", "source_error.r")
	
	expect_equivalent(c1, e1)
	
	
	# single script
	provDebugR:::.clear()
	json <- system.file("testdata", "typeChanges.json", package = "provDebugR")
	expect_warning(provDebugR::prov.debug.file(json))
	
	c2 <- provDebugR:::.get.scripts()
	e2 <- c("typeChanges.R")
	
	expect_equivalent(c2, e2)
	
	
	# console mode
	provDebugR:::.clear()
	json <- system.file("testdata", "fromEnv.json", package = "provDebugR")
	expect_warning(provDebugR::prov.debug.file(json))
	
	c3 <- provDebugR:::.get.scripts()
	e3 <- c("console")
	
	expect_equivalent(c3, e3)
})

# .get.script.names
test_that("Initialization - .get.script.names",
{
	scripts <- c("exceptions.R", "source_warning.r", "source_error.r")
	script.nums <- as.integer(c(1,2,3,1,3,2))
	
	c1 <- provDebugR:::.get.script.names(script.nums, scripts)
	e1 <- c("exceptions.R", "source_warning.r", "source_error.r",
			"exceptions.R", "source_error.r", "source_warning.r")
	
	expect_equivalent(c1, e1)
})