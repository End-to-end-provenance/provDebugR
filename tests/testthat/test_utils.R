library(testthat)
library(provDebugR)

context("Utility functions")

# no provenance

# .clear
# This is a function that is used only for testing purposes
test_that("Utility - .clear", 
{
	json <- system.file("testdata", "exceptions.json", package = "provDebugR")
	prov.debug.file(json)
	
	# ensure .debug.env has been changed
	expect_false(is.null(provDebugR:::.debug.env$prov))
	expect_false(is.null(provDebugR:::.debug.env$graph))
	expect_true(provDebugR:::.debug.env$has.graph)
	expect_false(is.null(provDebugR:::.debug.env$proc.nodes))
	expect_false(is.null(provDebugR:::.debug.env$data.nodes))
	expect_false(is.null(provDebugR:::.debug.env$data.proc))
	expect_false(is.null(provDebugR:::.debug.env$proc.data))
	
	# clear .debug.env
	provDebugR:::.clear()
	
	# ensure .debug.env has been returned to initial state
	expect_true(is.null(provDebugR:::.debug.env$prov))
	expect_true(is.null(provDebugR:::.debug.env$graph))
	expect_false(provDebugR:::.debug.env$has.graph)
	expect_true(is.null(provDebugR:::.debug.env$proc.nodes))
	expect_true(is.null(provDebugR:::.debug.env$data.nodes))
	expect_true(is.null(provDebugR:::.debug.env$data.proc))
	expect_true(is.null(provDebugR:::.debug.env$proc.data))
})
