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

test_that("step-in table", {
	
	# no step-in locations
	json <- system.file("testdata", "stepin0.json", package = "provDebugR")
	proc.nodes <- get.proc.nodes(prov.parse(json))
	expect_true(nrow(.get.step.in.table(proc.nodes)) == 0)
	
	
})
