library(testthat)
library(provDebugR)

context("Lineage")

json <- system.file("testdata", "test.json", package = "provDebugR")
prov.debug.file(json)

test_that("debug.line"
{
	debug.line <- provDebugR::debug.line
	
	# 1 valid
	result <- debug.line(10)
	expect_equal(length(result), 1)
	expect_equal(length(result)[[1]], 2)
	
	# 1 valid, 1 invalid
	result <- debug.line(10, 12)
	expect_equal(length(result), 1)
	expect_equal(length(result)[[1]], 2)
	
	
})