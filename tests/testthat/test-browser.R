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
	
	# help
	printed <- capture_output( quit <- .read.input("help", var.env) )
	expect_false(quit)
	
	# TODO - move forward
	printed <- capture_output( quit <- .read.input("n", var.env) )
	expect_equivalent(printed, "in .move.forward")
	expect_false(quit)
	
	# TODO - move backwards
	printed <- capture_output( quit <- .read.input("b", var.env) )
	expect_equivalent(printed, "in .move.backwards")
	expect_false(quit)
	
	# TODO - continue to end of script
	
	# TODO - step in
	printed <- capture_output( quit <- .read.input("s", var.env) )
	expect_equivalent(printed, "in .step.in")
	expect_false(quit)
	
	# TODO - l
	
	# TODO - ls: query list of vars
	
	# TOOD - query var
	
	# TODO - mv
	
	
	
	# TODO - code to interpreter
	
})