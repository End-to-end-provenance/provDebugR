# author: Elizabeth Fong
library(testthat)
library(provDebugR)
library(provParseR)

context("Browser")

# no provenance
test_that("no/empty provenance", {
	
	# debugger has not been initialised
	expect_error(debug.browser())
	
	# no provenance (no proc nodes)
	json <- system.file("testdata", "empty.json", package = "provDebugR")
	expect_error(debug.init(json))
	expect_error(debug.browser())
})

# testing .read.input (testing moving to correct branch)
test_that(".read.input (moving to correct branch)", {
	
	# TMP - dummy var.env
	var.env = new.env()
	
	quit <- FALSE
	
	# quit
	printed <- capture_output( quit <- .read.input("Q", var.env) )
	expect_equivalent(printed, "Quitting")
	expect_true(quit)
})